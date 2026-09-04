# Transactional write/merge layer for citius's three big corpus tables.
#
# WHY THIS EXISTS. On 2026-08-29 a script merging new Diamond League results
# into championship_results.rds used a dedup key that wasn't actually unique
# for multi-attempt field events, silently collapsing 21,440 rows before being
# caught. The correct merge pattern (drop whole competitions already present,
# never row-level dedup; assert the row count after) already existed,
# independently duplicated across two live scripts. This module makes it ONE
# function, wraps it in a real transaction, and adds a schema guard modeled on
# bouncer::store_player_game_data() -- which itself exists because a warning
# nobody read let a computed column silently vanish for weeks.
#
# This is the WRITE layer only. Fast filtered reads still go through
# citius/R/store.R's partitioned-parquet `write_results_store()` /
# `read_results_store()` -- 118x faster than a raw RDS scan on this
# pipeline's actual event_id-filtered query pattern, and untouched by this
# module. `load_*()` here exist for callers that want the transactionally
# up-to-date table directly (e.g. right after a merge, before the parquet
# store is rebuilt).
#
# WINDOWS NOTE: unlike `arrow`, which segfaults under Git Bash R with no
# message, `duckdb` was smoke-tested clean under both bare `Rscript` (Git
# Bash) and `powershell.exe -Command 'Rscript ...'` on 2026-08-30. No special
# invocation is required for anything in this file.

.citius_duckdb_cache <- new.env(parent = emptyenv())

#' Check DuckDB/DBI are usable
#' @keywords internal
.check_citius_duckdb_available <- function() {
  if (isTRUE(.citius_duckdb_cache$available)) return(invisible(TRUE))
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("duckdb", quietly = TRUE)) {
    cli::cli_abort(c(
      "Packages {.pkg DBI} and {.pkg duckdb} are required for the citius store.",
      i = "Install with: {.code install.packages(c('DBI','duckdb'))}"
    ))
  }
  .citius_duckdb_cache$available <- TRUE
  invisible(TRUE)
}

#' Resolve the citius DuckDB database path
#'
#' Walks up from the working directory looking for a `citiusdata/` sibling or
#' child, matching `citius/R/store.R`'s and bouncer's `find_bouncerdata_dir()`
#' resolution convention. Ported rather than shared because the two packages
#' do not depend on each other.
#'
#' @param path Explicit path; returned as-is (normalised) if given.
#' @param subpath Path to the database file, relative to the resolved
#'   `citiusdata/` directory. Defaults to today's flat-layout location
#'   (`data/citius.duckdb`). A future data-directory reorg only needs this
#'   default updated here, not every caller -- the whole point of naming it
#'   as a parameter rather than hardcoding it inline (found worth doing
#'   2026-08-30, alongside `citiusdata/scripts/_paths.R`, the equivalent
#'   indirection layer for the R-script side of this same problem).
#' @return Character path to the database file.
#' @keywords internal
get_citius_db_path <- function(path = NULL, subpath = file.path("data", "citius.duckdb")) {
  if (!is.null(path)) return(normalizePath(path, winslash = "/", mustWork = FALSE))
  cwd <- normalizePath(getwd(), winslash = "/")
  current <- cwd
  for (i in 1:10) {
    sib <- file.path(dirname(current), "citiusdata")
    if (dir.exists(sib)) return(normalizePath(file.path(sib, subpath),
                                              winslash = "/", mustWork = FALSE))
    child <- file.path(current, "citiusdata")
    if (dir.exists(child)) return(normalizePath(file.path(child, subpath),
                                                winslash = "/", mustWork = FALSE))
    parent <- dirname(current)
    if (parent == current) break
    current <- parent
  }
  cli::cli_abort(c(
    "Could not find a {.file citiusdata/} directory walking up from {.file {cwd}}.",
    i = "Run from inside the citiusverse tree, or pass {.arg path} explicitly."
  ))
}

#' Open a connection to the citius DuckDB database
#' @param path Explicit database path. `NULL` resolves via `get_citius_db_path()`.
#' @param read_only Open read-only.
#' @return A DBI connection.
#' @keywords internal
get_citius_db_connection <- function(path = NULL, read_only = FALSE) {
  .check_citius_duckdb_available()
  path <- get_citius_db_path(path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = read_only)
}

#' Run `fn` against a connection that is always closed on the way out
#'
#' DuckDB permits exactly one write connection at a time -- a connection
#' leaked by an error holds the lock for the rest of the R session. Prefer
#' this to hand-rolling `on.exit()` at each call site (ported from bouncer's
#' `with_db_connection()`).
#'
#' @param fn Function of one argument, the connection.
#' @param path,read_only See `get_citius_db_connection()`.
#' @return Whatever `fn` returns.
#' @export
with_citius_db_connection <- function(fn, path = NULL, read_only = FALSE) {
  conn <- get_citius_db_connection(path = path, read_only = read_only)
  on.exit(
    tryCatch(
      DBI::dbDisconnect(conn, shutdown = TRUE),
      error = function(e) cli::cli_warn(c(
        "Failed to close the citius DB connection cleanly: {conditionMessage(e)}",
        "!" = "A write lock may still be held for the rest of this session."
      ))
    ),
    add = TRUE
  )
  fn(conn)
}

#' Does a table exist in the citius DuckDB database?
#' @param conn Connection.
#' @param table_name Unqualified table name.
#' @return Logical.
#' @keywords internal
citius_table_exists <- function(conn, table_name) {
  nrow(DBI::dbGetQuery(conn,
    "SELECT 1 FROM information_schema.tables WHERE table_name = ?",
    params = list(table_name))) > 0
}

#' The one merge/replace engine every `store_*()` wrapper calls into
#'
#' Schema guard (abort on an extra column the target lacks -- would be
#' silently dropped; warn-only on a missing column) modeled directly on
#' `bouncer::store_player_game_data()`. Merge dedup drops whole competitions
#' already present -- never row-level -- exactly
#' `citiusdata/scripts/merge_referenced.R`'s pattern, now inside a
#' transaction that rolls back on a failed row-count assertion instead of
#' leaving a partial or silently-wrong table.
#'
#' @param conn Writable connection.
#' @param table_name Target table.
#' @param new New data (data.frame or data.table).
#' @param dedup_key Column whole rows of which are deduplicated on, for
#'   `mode = "merge"`. Ignored for `mode = "replace"`.
#' @param schema Expected column names, used only when the table does not yet
#'   exist (see `CITIUS_DB_SCHEMA` in db_schema.R).
#' @param mode `"merge"` (default) or `"replace"` (full rebuild).
#' @return Invisibly, the number of rows added (merge) or written (replace).
#' @keywords internal
.citius_store_merge <- function(conn, table_name, new, dedup_key = "competition_id",
                                schema = NULL, mode = c("merge", "replace")) {
  mode <- match.arg(mode)
  new <- data.table::as.data.table(new)
  if (!nrow(new)) {
    cli::cli_alert_warning("No rows to store in {.field {table_name}}; leaving it untouched.")
    return(invisible(0L))
  }

  exists <- citius_table_exists(conn, table_name)

  # SCHEMA GUARD. Target columns come from the live table when merging into
  # one that exists (it is the ground truth if the two have drifted) or from
  # the declared schema for a table's first-ever write. "replace" is
  # different: it DROPs and recreates the table from `new`'s own columns
  # (below), so checking `new` against the OLD table's columns would forbid
  # replace from ever changing the schema -- defeating the reason replace
  # mode exists (build_athletics_corpus.R's full-rebuild-every-run pattern
  # needs exactly this: a new run adding a column must not be rejected
  # because yesterday's table didn't have it yet). Found in production use
  # (2026-08-30, the championship_results recovery), not in review -- the
  # test suite never exercised a replace against an already-populated table
  # with a narrower schema. Replace checks against the DECLARED schema
  # instead, which still catches a genuine mistake (unexpected columns on
  # the incoming data), just not a schema that intentionally evolved.
  target_cols <- if (mode == "replace") {
    if (is.null(schema)) {
      DBI::dbGetQuery(conn, sprintf(
        "SELECT column_name FROM information_schema.columns WHERE table_name = '%s'
         ORDER BY ordinal_position", table_name))$column_name
    } else schema
  } else if (exists) {
    DBI::dbGetQuery(conn, sprintf(
      "SELECT column_name FROM information_schema.columns WHERE table_name = '%s'
       ORDER BY ordinal_position", table_name))$column_name
  } else schema
  if (is.null(target_cols)) {
    cli::cli_abort("No schema known for {.field {table_name}} and it does not exist yet; pass {.arg schema}.")
  }
  extra <- setdiff(names(new), target_cols)
  if (length(extra)) {
    cli::cli_abort(c(
      "{length(extra)} column{?s} would be dropped writing {.field {table_name}}.",
      "x" = "{.val {extra}}",
      i = "Add them to CITIUS_DB_SCHEMA in db_schema.R and, if the table already
           exists, migrate it with {.code ALTER TABLE ... ADD COLUMN}; or drop
           them upstream deliberately."
    ))
  }
  missing <- setdiff(target_cols, names(new))
  # The dedup key is not an ordinary column: if it's absent in "merge" mode,
  # the 0-sentinel guard below silently no-ops (its own `if` is gated on the
  # column existing), the dedup step matches nothing so no competition is
  # dropped, and the INSERT's column list excludes it -- every new row lands
  # with a NULL key. Row count still balances, so the assertion below passes
  # and this reproduces the exact "clean exit, wrong data" shape of the
  # 2026-08-29 incident from a different door. Found in review (2026-08-30) of
  # this very module. A missing ORDINARY column stays a warning; the dedup key
  # does not get to share that severity.
  if (mode == "merge" && dedup_key %in% missing) {
    cli::cli_abort(c(
      "Dedup key {.field {dedup_key}} is missing from the new data.",
      i = "Merging without it would insert every row with a NULL key and skip
           duplicate-competition detection entirely, silently. Add the column
           or pass {.code mode = \"replace\"} if that is genuinely intended."
    ))
  }
  missing <- setdiff(missing, dedup_key)
  if (length(missing)) {
    cli::cli_warn("Columns in {table_name}'s schema absent from new data (left NULL): {.field {missing}}")
  }

  # 0-SENTINEL GUARD, carried verbatim from merge_referenced.R: a phantom
  # competition_id 0 once collected 2.5M rows and made every meet-level
  # statistic meaningless. Cheap to assert, expensive to miss, and real
  # regardless of mode -- a literal 0 is never a legitimate id, so this part
  # of the guard applies to both "merge" and "replace".
  #
  # The NA half is NOT the same in both modes, and treating it as if it were
  # broke store_athletics_corpus(mode = "replace") against real data --
  # found live in review 2026-08-30: ~35% of athletics_corpus's rows
  # legitimately carry NA competition_id BY DESIGN (career-route rows with
  # no competition context, see build_athletics_corpus.R's own header). The
  # original rationale ("a NULL key would never be recognised as a
  # duplicate on a later run") is a MERGE-mode-only concern -- there is no
  # dedup, and therefore no "later run" to fail to recognise a duplicate on,
  # when "replace" drops and recreates the whole table from scratch every
  # time. Gating this half to merge mode is not a relaxation of the original
  # incident's fix (a phantom competition_id 0/NA silently corrupting an
  # INCREMENTAL merge) -- it is scoping it to the mode where that incident
  # could actually happen again.
  if (dedup_key %in% names(new)) {
    key_vals <- new[[dedup_key]]
    if (mode == "merge" && anyNA(key_vals)) {
      cli::cli_abort("{.field {dedup_key}} contains NA in the new data -- refusing to store (a NULL key would never be recognised as a duplicate on a later run).")
    }
    if (any(key_vals == 0, na.rm = TRUE)) {
      cli::cli_abort("{.field {dedup_key}} contains 0 in the new data -- refusing to store (see merge_referenced.R history).")
    }
  }

  DBI::dbBegin(conn)
  result <- tryCatch({
    before <- if (exists) DBI::dbGetQuery(conn, sprintf(
      "SELECT COUNT(*) AS n FROM %s", table_name))$n else 0L

    if (mode == "replace" || !exists) {
      duckdb::duckdb_register(conn, "citius_staging_tmp", new)
      on.exit(tryCatch(duckdb::duckdb_unregister(conn, "citius_staging_tmp"),
                       error = function(e) cli::cli_warn(
                         "Failed to unregister citius_staging_tmp: {conditionMessage(e)}")),
              add = TRUE)
      if (exists) DBI::dbExecute(conn, sprintf("DROP TABLE %s", table_name))
      DBI::dbExecute(conn, sprintf("CREATE TABLE %s AS SELECT * FROM citius_staging_tmp", table_name))
      after <- DBI::dbGetQuery(conn, sprintf("SELECT COUNT(*) AS n FROM %s", table_name))$n
      stopifnot("replace did not write the expected row count" = after == nrow(new))
      list(before = before, after = after)
    } else {
      # THE competition-level dedup: drop whole competitions already present.
      # A half-merged field corrupts the shared race effect, which is the
      # entire reason these competitions are worth fetching -- never
      # row-level dedup here.
      existing_keys <- DBI::dbGetQuery(conn, sprintf(
        "SELECT DISTINCT %s FROM %s", dedup_key, table_name))[[dedup_key]]
      key_vals <- new[[dedup_key]]
      dup <- intersect(unique(key_vals), existing_keys)
      if (length(dup)) {
        cli::cli_alert_info("Dropping {length(dup)} competition(s) already in {table_name}.")
        new <- new[!key_vals %in% dup]
      }
      if (!nrow(new)) {
        DBI::dbRollback(conn)
        return(invisible(0L))
      }
      duckdb::duckdb_register(conn, "citius_staging_tmp", new)
      on.exit(tryCatch(duckdb::duckdb_unregister(conn, "citius_staging_tmp"),
                       error = function(e) cli::cli_warn(
                         "Failed to unregister citius_staging_tmp: {conditionMessage(e)}")),
              add = TRUE)
      col_list <- paste(intersect(target_cols, names(new)), collapse = ", ")
      DBI::dbExecute(conn, sprintf("INSERT INTO %s (%s) SELECT %s FROM citius_staging_tmp",
                                   table_name, col_list, col_list))
      after <- DBI::dbGetQuery(conn, sprintf("SELECT COUNT(*) AS n FROM %s", table_name))$n
      # THE row-count assertion, exactly merge_referenced.R:79. A failure here
      # rolls back the whole write -- the actual upgrade over saveRDS()-then-hope.
      stopifnot("row count after merge does not match before + new" = after == before + nrow(new))
      list(before = before, after = after)
    }
  }, error = function(e) {
    # If rollback ITSELF throws (e.g. the connection is already broken by
    # whatever caused `e`), a bare `DBI::dbRollback(conn)` here would replace
    # the real error with the rollback failure, and `stop(e)` below would
    # never run -- hiding the actual root cause on the one path where
    # diagnosability matters most. Found in review (2026-08-30).
    tryCatch(DBI::dbRollback(conn), error = function(e2) cli::cli_warn(
      "Rollback also failed after the original error: {conditionMessage(e2)}"))
    stop(e)
  })
  DBI::dbCommit(conn)
  cli::cli_alert_success("{table_name}: {result$before} -> {result$after} rows.")
  invisible(result$after - result$before)
}

#' Store new championship results, merging by competition
#' @param conn Writable connection.
#' @param new New results (data.table).
#' @param mode `"merge"` (default) or `"replace"`.
#' @return Invisibly, rows added.
#' @export
store_championship_results <- function(conn, new, mode = c("merge", "replace")) {
  .citius_store_merge(conn, "championship_results", new,
                      dedup_key = CITIUS_DB_DEDUP_KEY$championship_results,
                      schema = CITIUS_DB_SCHEMA$championship_results,
                      mode = match.arg(mode))
}

#' Store the athletics corpus, normally a full rebuild
#' @param conn Writable connection.
#' @param data The assembled corpus (data.table).
#' @param mode `"replace"` (default, matches `build_athletics_corpus.R`'s
#'   full-rebuild-every-run shape) or `"merge"`.
#' @return Invisibly, rows written/added.
#' @export
store_athletics_corpus <- function(conn, data, mode = c("replace", "merge")) {
  .citius_store_merge(conn, "athletics_corpus", data,
                      dedup_key = CITIUS_DB_DEDUP_KEY$athletics_corpus,
                      schema = CITIUS_DB_SCHEMA$athletics_corpus,
                      mode = match.arg(mode))
}

#' Store new athlete history rows, merging by competition
#' @param conn Writable connection.
#' @param new New history rows (data.table).
#' @param mode `"merge"` (default) or `"replace"`.
#' @return Invisibly, rows added.
#' @export
store_athletics_history <- function(conn, new, mode = c("merge", "replace")) {
  .citius_store_merge(conn, "athletics_history", new,
                      dedup_key = CITIUS_DB_DEDUP_KEY$athletics_history,
                      schema = CITIUS_DB_SCHEMA$athletics_history,
                      mode = match.arg(mode))
}

# ALL THREE LOADERS TAKE THE SAME FILTERS, deliberately (2026-09-03). They
# used to differ: only the corpus supported `columns`, and history supported
# only `athlete_ids` with no date/event/column filtering at all. That
# asymmetry is a migration blocker rather than a style problem -- a caller
# moving off readRDS() has to narrow columns to avoid materialising the
# whole table (backtest_athletics.R documents an OOM kill from exactly that),
# and it could only do so for one of the three tables. All three schemas
# carry event_id, date and athlete_id (see db_schema.R), so there is no
# table-specific reason for the filters to differ.

#' Load championship results from the citius DuckDB store
#' @param conn Connection.
#' @param events,from,to Optional filters: event_id vector, date range.
#' @param athlete_ids Optional athlete_id filter.
#' @param columns Optional column subset.
#' @return A data.table.
#' @export
load_championship_results <- function(conn, events = NULL, from = NULL, to = NULL,
                                      athlete_ids = NULL, columns = NULL) {
  .citius_load(conn, "championship_results", events, from, to, columns, athlete_ids)
}

#' Load the athletics corpus from the citius DuckDB store
#' @param conn Connection.
#' @param events,from,to Optional filters: event_id vector, date range.
#' @param athlete_ids Optional athlete_id filter.
#' @param columns Optional column subset.
#' @return A data.table.
#' @export
load_athletics_corpus <- function(conn, events = NULL, from = NULL, to = NULL,
                                  columns = NULL, athlete_ids = NULL) {
  .citius_load(conn, "athletics_corpus", events, from, to, columns, athlete_ids)
}

#' Load athlete history rows from the citius DuckDB store
#' @param conn Connection.
#' @param athlete_ids Optional athlete_id filter.
#' @param events,from,to Optional filters: event_id vector, date range.
#' @param columns Optional column subset.
#' @return A data.table.
#' @export
load_athletics_history <- function(conn, athlete_ids = NULL, events = NULL,
                                   from = NULL, to = NULL, columns = NULL) {
  .citius_load(conn, "athletics_history", events, from, to, columns, athlete_ids)
}

#' @keywords internal
.citius_load <- function(conn, table_name, events = NULL, from = NULL, to = NULL,
                         columns = NULL, athlete_ids = NULL) {
  if (!citius_table_exists(conn, table_name)) {
    cli::cli_abort("No {.field {table_name}} table in the citius DB yet.")
  }
  # A caller asking for a column this table does not have should hear about
  # it here, not get a bare DuckDB parse error naming no table.
  if (!is.null(columns)) {
    known <- CITIUS_DB_SCHEMA[[table_name]]
    unknown <- setdiff(columns, known)
    if (length(unknown)) {
      cli::cli_abort(c(
        "x" = "{.field {table_name}} has no column{?s} {.val {unknown}}.",
        "i" = "Known columns: {.val {known}}"))
    }
  }
  cols <- if (is.null(columns)) "*" else paste(columns, collapse = ", ")
  where <- character(0)
  params <- list()
  if (!is.null(events)) {
    where <- c(where, sprintf("event_id IN (%s)", paste(rep("?", length(events)), collapse = ",")))
    params <- c(params, as.list(events))
  }
  if (!is.null(athlete_ids)) {
    where <- c(where, sprintf("athlete_id IN (%s)", paste(rep("?", length(athlete_ids)), collapse = ",")))
    params <- c(params, as.list(as.character(athlete_ids)))
  }
  if (!is.null(from)) { where <- c(where, "date >= ?"); params <- c(params, list(as.character(from))) }
  if (!is.null(to))   { where <- c(where, "date <= ?"); params <- c(params, list(as.character(to))) }
  sql <- sprintf("SELECT %s FROM %s", cols, table_name)
  if (length(where)) sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))
  data.table::setDT(DBI::dbGetQuery(conn, sql, params = params))
}
