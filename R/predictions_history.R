# Dated, append-only history of published predictions (issue #32).
#
# The "latest" predictions file is overwritten on every publish, so once a
# version is replaced nothing can be scored against what the model said at the
# time. This keeps one parquet per publish on its own release tag and never
# rewrites one. Reuses vb_publish() for the upload so the safe-upload,
# verification and manifest behaviour is the same as every other release.

#' Columns every predictions-history file carries
#' @keywords internal
PREDICTION_HISTORY_COLS <- c("verse", "event", "market", "selection",
                             "probability", "published_utc", "model_version")

#' Release tag holding the dated prediction history
#' @keywords internal
PREDICTION_HISTORY_TAG <- "predictions-history"

#' File name for a prediction-history publish
#'
#' `predictions_<YYYYMMDDTHHMMSSZ>.parquet`, UTC, to the second.
#' @param published a `POSIXct` publish time.
#' @return a single string.
#' @keywords internal
#' @export
prediction_history_name <- function(published = Sys.time()) {
  stopifnot(inherits(published, "POSIXct"), length(published) == 1L, !is.na(published))
  paste0("predictions_", format(published, "%Y%m%dT%H%M%SZ", tz = "UTC"), ".parquet")
}

#' Build the table written to one prediction-history file
#'
#' Checks the minimum columns are present and sane, stamps `published_utc` and
#' `model_version`, and puts the minimum columns first (extra columns are kept).
#' `selection` is the model's own wording for the outcome and is not rewritten.
#' Any `published_utc` or `model_version` already on `predictions` is replaced:
#' the stamp is the publish, not whatever the caller carried in.
#'
#' @param predictions a data frame with `verse`, `event`, `market`,
#'   `selection` and `probability`.
#' @param model_version string identifying the model that produced them.
#' @param published a `POSIXct` publish time (stored as UTC).
#' @return a `data.table`.
#' @keywords internal
#' @export
prediction_history_table <- function(predictions, model_version,
                                     published = Sys.time()) {
  stopifnot(is.data.frame(predictions), inherits(published, "POSIXct"),
            length(published) == 1L, !is.na(published),
            is.character(model_version), length(model_version) == 1L,
            !is.na(model_version), nzchar(model_version))
  need <- setdiff(PREDICTION_HISTORY_COLS, c("published_utc", "model_version"))
  miss <- setdiff(need, names(predictions))
  if (length(miss)) {
    cli::cli_abort("predictions is missing column{?s} {.field {miss}}")
  }
  if (nrow(predictions) == 0L) cli::cli_abort("predictions has no rows; nothing to publish")

  dt <- data.table::as.data.table(predictions)
  dt <- data.table::copy(dt)
  for (col in c("verse", "event", "market", "selection")) {
    v <- dt[[col]]
    if (anyNA(v) || any(!nzchar(as.character(v)))) {
      cli::cli_abort("column {.field {col}} has missing or empty values")
    }
  }
  p <- dt$probability
  if (!is.numeric(p) || anyNA(p) || any(p < 0 | p > 1)) {
    cli::cli_abort("column {.field probability} must be numeric, non-missing and within [0, 1]")
  }
  stale <- intersect(c("published_utc", "model_version"), names(dt))
  if (length(stale)) data.table::set(dt, j = stale, value = NULL)
  stamp <- as.POSIXct(as.numeric(published), origin = "1970-01-01", tz = "UTC")
  data.table::set(dt, j = "published_utc", value = rep(stamp, nrow(dt)))
  data.table::set(dt, j = "model_version", value = rep(model_version, nrow(dt)))
  data.table::setcolorder(dt, c(PREDICTION_HISTORY_COLS,
                                setdiff(names(dt), PREDICTION_HISTORY_COLS)))
  dt[]
}

#' Publish one dated prediction-history file, never overwriting an earlier one
#'
#' Writes `predictions_<YYYYMMDDTHHMMSSZ>.parquet` and uploads it to the
#' `predictions-history` release as a new asset. If an asset of that name is
#' already there it aborts rather than replace it: a correction is a new file
#' with a new time. The current "latest" file is a separate matter and is not
#' touched here.
#'
#' The existence check and the upload are not atomic, so two publishes in the
#' same second could race; publishes are minutes apart in practice.
#'
#' @inheritParams prediction_history_table
#' @param repo `owner/name` of the repo holding the release.
#' @param tag release tag, default `"predictions-history"`.
#' @param dry_run build and validate the file, check the name is free, upload
#'   nothing, and return the would-be asset name.
#' @return the asset name, invisibly.
#' @keywords internal
#' @export
publish_prediction_history <- function(predictions, repo,
                                       model_version = as.character(utils::packageVersion("citius")),
                                       published = Sys.time(),
                                       tag = PREDICTION_HISTORY_TAG,
                                       dry_run = FALSE) {
  rlang::check_installed(c("arrow", "piggyback", "gh", "digest"),
                         reason = "to publish prediction history")
  dt <- prediction_history_table(predictions, model_version, published)
  name <- prediction_history_name(published)

  existing <- tryCatch(vb_list_assets(repo, tag)$name, vb_error_absent = function(e) NULL)
  if (name %in% existing) {
    cli::cli_abort(c("{.val {name}} already exists on {repo}@{tag}; a published file is never rewritten.",
                     i = "A correction is a new file with a new publish time."),
                   class = c("vb_error_integrity", "vb_error"))
  }
  if (dry_run) return(invisible(name))

  dir <- tempfile("pred_history_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  path <- file.path(dir, name)
  arrow::write_parquet(dt, path)

  if (is.null(existing)) {
    cli::cli_alert_info("Creating release {.val {tag}} on {repo}")
    piggyback::pb_new_release(repo = repo, tag = tag)
  }
  vb_publish(path, repo = repo, tag = tag,
             rows = stats::setNames(nrow(dt), name), carry_forward = TRUE)
  invisible(name)
}

#' Run a live publish, then record the dated history copy
#'
#' The one place the history step is wired to a live publish. `live` is the
#' caller's own upload (for the athletics cards: the R2 upload in
#' citiusdata's `export_athletics_blog.R`). History is written only after
#' `live` returns without error, and in its own `tryCatch`, so a history
#' failure is reported as a warning and never fails the live publish. The
#' timestamp that names the history file is taken once, after the live upload
#' has succeeded.
#'
#' @param live a function with no arguments that does the live upload. It must
#'   signal an error on failure. Its value is returned.
#' @param predictions,repo,model_version,tag passed to
#'   [publish_prediction_history()].
#' @param history_fn the history publisher; a seam for tests.
#' @return the value of `live()`; the history asset name is attached as the
#'   `"history"` attribute when it was written.
#' @keywords internal
#' @export
publish_with_history <- function(live, predictions, repo,
                                 model_version = as.character(utils::packageVersion("citius")),
                                 tag = PREDICTION_HISTORY_TAG,
                                 history_fn = publish_prediction_history) {
  stopifnot(is.function(live))
  out <- live()  # an error here propagates: no live publish, no history
  name <- tryCatch(
    history_fn(predictions, repo = repo, model_version = model_version,
               published = Sys.time(), tag = tag),
    error = function(e) {
      cli::cli_alert_warning(
        "Prediction history NOT written ({conditionMessage(e)}); the live publish succeeded.")
      NULL
    })
  # attr<- on NULL is an error, and many upload functions return
  # invisible(NULL): setting it unguarded would throw AFTER both the live and
  # the history publish had succeeded (review, 2026-11-07).
  if (!is.null(name) && !is.null(out)) attr(out, "history") <- name
  out
}
