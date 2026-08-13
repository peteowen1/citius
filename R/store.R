#' Write a result corpus as a partitioned parquet dataset
#'
#' `.rds` forces the whole corpus into memory before a single row can be
#' filtered. At the 300k rows this project started with that was merely wasteful;
#' at the ~8.5M rows the career harvests produce it is the dominant cost, and it
#' has already OOM-killed a backtest once.
#'
#' Measured on 8.6M rows, running the query the backtest actually issues
#' ("these events, this date window, these 11 columns"):
#'
#' | store | per query | across 825 backtest meets |
#' |-------|-----------|---------------------------|
#' | `.rds` | 46.1s | ~10.6 hours |
#' | parquet | 8.7s | 120 min |
#' | **parquet partitioned by `event_id`** | **0.39s** | **5 min** |
#'
#' Partitioning wins because every query in this pipeline filters on `event_id`,
#' so all but a handful of files are skipped without being opened. It costs
#' roughly 2x the disk (586 MB against 273 MB) since many small files compress
#' worse than one large one — an easy trade for 118x.
#'
#' @param results A results `data.table`.
#' @param path Destination directory. Overwritten.
#' @param partition_by Column to partition on. `event_id` unless you have
#'   measured a better one for your access pattern.
#' @return `path`, invisibly.
#' @examples
#' \dontrun{
#' write_results_store(champs, "citiusdata/data/athletics_store")
#' }
#' @export
write_results_store <- function(results, path, partition_by = "event_id") {
  .require_arrow("write_results_store")
  # `as.data.table()` on an already-valid data.table is a FULL DEEP COPY, not the
  # no-op it looks like -- so `copy(as.data.table(x))` costs TWO copies of a
  # 6.6M-row corpus where one is needed. Documented in
  # C:/dev/.claude/rules/r-datatable-gotchas.md, and this is the function whose
  # whole purpose is making corpus I/O cheap. One copy, because the `:=` below
  # must not mutate the caller's table.
  dt <- data.table::copy(if (data.table::is.data.table(results)) results else
                         data.table::as.data.table(results))
  if (!partition_by %in% names(dt)) {
    cli::cli_abort("{.arg partition_by} column {.field {partition_by}} not found.")
  }
  # An NA partition value becomes a "__HIVE_DEFAULT_PARTITION__" directory whose
  # contents do not round-trip as NA. Unmatched events -- relays, age-group
  # specs, off-track races -- are a real category worth keeping, so label them
  # explicitly and restore the NA on read.
  dt[is.na(get(partition_by)), (partition_by) := "__unmatched__"]

  # Write to a temp sibling and swap in AFTER the write succeeds. The previous
  # order -- unlink the live store, then write -- meant any mid-write failure
  # (disk full, killed process, bad partition value) destroyed the only copy of
  # the corpus and left an incomplete replacement, with nothing detecting it.
  # The expensive, fallible step must finish before the old store is touched.
  #
  # THE INVARIANT, kept on every path below including compound failures: a
  # known-good store always exists at `path`, `.old-write` or `.tmp-write`,
  # and the abort message says which. Review 2026-08-14 reproduced a two-
  # failure sequence (crash between the renames, then the retry's own rename
  # failing) where a first version deleted the only surviving copy and then
  # claimed "previous store restored" -- a false message on a total loss.
  old <- paste0(path, ".old-write")
  # A previous call that crashed between its two renames leaves `path` missing
  # and the good store under `.old-write`. Recover it BEFORE anything else --
  # the first version unconditionally unlink()ed `old` here, destroying the
  # only good copy three lines above the branch that would have needed it.
  if (!dir.exists(path) && dir.exists(old)) {
    cli::cli_warn("Recovering store at {.path {path}} from an interrupted previous write.")
    if (!file.rename(old, path)) {
      cli::cli_abort(c(
        "A previous write crashed mid-swap and the backup cannot be moved back.",
        i = "The last good store is at {.path {old}}; restore it manually before writing."
      ))
    }
  }
  tmp <- paste0(path, ".tmp-write")
  unlink(tmp, recursive = TRUE)
  # Clean the temp dir on a mid-write throw too -- otherwise it lingers until
  # the next call. The error itself still propagates untouched.
  tryCatch(
    arrow::write_dataset(dt, tmp, partitioning = partition_by, format = "parquet"),
    error = function(e) { unlink(tmp, recursive = TRUE); stop(e) }
  )
  if (!dir.exists(tmp) || !length(list.files(tmp, recursive = TRUE))) {
    unlink(tmp, recursive = TRUE)
    cli::cli_abort("Write to {.path {tmp}} produced no files; existing store left untouched.")
  }
  # Only now may a stale backup be cleared: this call is about to repopulate it.
  if (dir.exists(path)) {
    unlink(old, recursive = TRUE)
    if (!file.rename(path, old)) {
      unlink(tmp, recursive = TRUE)
      cli::cli_abort(c(
        "Could not move the existing store aside; it is left untouched.",
        i = "Something may be holding files open under {.path {path}}."
      ))
    }
  }
  if (!file.rename(tmp, path)) {
    # Put the old store back before failing. If even that fails, say exactly
    # where the surviving copies are and DELETE NOTHING -- `tmp` holds the
    # freshly validated new store and must outlive a failed swap.
    restored <- dir.exists(old) && file.rename(old, path)
    if (restored) {
      unlink(tmp, recursive = TRUE)
      cli::cli_abort("Could not move the new store into place; previous store restored.")
    }
    cli::cli_abort(c(
      "Could not move the new store into place, and the previous store could not be restored.",
      i = "The new store is intact at {.path {tmp}}; the previous one, if any, at {.path {old}}.",
      i = "Nothing was deleted. Resolve the lock and rename one of them to {.path {path}}."
    ))
  }
  unlink(old, recursive = TRUE)
  invisible(path)
}

#' Read from a partitioned result store
#'
#' Filters and column selection are pushed down to the files, so a query touches
#' only the partitions it needs and never materialises the rest.
#'
#' @section Dates:
#' Pass real `Date` objects. Arrow cannot subtract a number from a timestamp, so
#' `date >= cut - 4380` fails inside a query with
#' `NotImplemented: subtract_checked` — compute the bound in R first. That is why
#' this takes `from`/`to` rather than a filter expression.
#'
#' @param path Store directory written by [write_results_store()].
#' @param events Event ids to read, or `NULL` for all.
#' @param from,to Inclusive date bounds as `Date`, or `NULL`.
#' @param columns Columns to return, or `NULL` for all.
#' @return A `data.table`.
#' @examples
#' \dontrun{
#' read_results_store("citiusdata/data/athletics_store",
#'                    events = "AT-100Metres-M",
#'                    from = as.Date("2016-01-01"), to = as.Date("2024-08-01"))
#' }
#' @export
read_results_store <- function(path, events = NULL, from = NULL, to = NULL,
                               columns = NULL) {
  .require_arrow("read_results_store")
  if (!dir.exists(path)) cli::cli_abort("No store at {.path {path}}.")
  ds <- arrow::open_dataset(path)
  q <- ds
  if (!is.null(events)) q <- dplyr::filter(q, event_id %in% events)
  if (!is.null(from))   q <- dplyr::filter(q, date >= from)
  if (!is.null(to))     q <- dplyr::filter(q, date <= to)
  if (!is.null(columns)) {
    missing <- setdiff(columns, names(ds))
    if (length(missing)) cli::cli_abort("Column{?s} not in store: {.field {missing}}.")
    q <- dplyr::select(q, dplyr::all_of(columns))
  }
  out <- data.table::as.data.table(dplyr::collect(q))
  if ("event_id" %in% names(out)) {
    out[event_id == "__unmatched__", event_id := NA_character_]
  }
  out[]
}

#' Fail with an installation hint when arrow is absent
#'
#' `arrow` sits in Suggests -- most of the package works without it -- so the
#' store functions must not die with a raw "there is no package called 'arrow'"
#' on an install that legitimately skipped it.
#' @keywords internal
#' @noRd
.require_arrow <- function(caller) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.fn {caller}} needs the {.pkg arrow} package.",
      i = 'Install it with {.code install.packages("arrow")}.'
    ))
  }
}
