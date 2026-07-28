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
  dt <- data.table::copy(data.table::as.data.table(results))
  if (!partition_by %in% names(dt)) {
    cli::cli_abort("{.arg partition_by} column {.field {partition_by}} not found.")
  }
  # An NA partition value becomes a "__HIVE_DEFAULT_PARTITION__" directory whose
  # contents do not round-trip as NA. Unmatched events -- relays, age-group
  # specs, off-track races -- are a real category worth keeping, so label them
  # explicitly and restore the NA on read.
  dt[is.na(get(partition_by)), (partition_by) := "__unmatched__"]
  unlink(path, recursive = TRUE)
  arrow::write_dataset(dt, path, partitioning = partition_by, format = "parquet")
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
