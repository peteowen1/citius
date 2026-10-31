# load_store_table() replaces a "try DuckDB, else readRDS" block that was
# pasted into 23 citiusdata scripts. The pasted fallback ignored `columns=`
# and every other filter, so a caller asking for 3 columns got the whole .rds.

.lst_fixture <- function() {
  data.table::data.table(
    competition_id = c(1L, 1L, 2L, 3L),
    event_id = c("100m", "100m", "200m", "100m"),
    athlete_id = c("a", "b", "a", "c"),
    date = as.Date(c("2020-01-01", "2020-01-01", "2021-06-01", "2022-03-01")),
    place = c(1L, 2L, 1L, 1L),
    mark = c(10.1, 10.2, 20.5, 10.0)
  )
}

test_that("fallback to .rds applies columns and filters, and warns", {
  d <- .lst_fixture()
  rds <- tempfile(fileext = ".rds"); saveRDS(d, rds)
  # An empty DuckDB file: no championship_results table, so the DB load errors.
  db <- tempfile(fileext = ".duckdb")
  expect_warning(
    got <- load_store_table("championship_results", rds_path = rds,
                            columns = c("athlete_id", "event_id", "date"),
                            events = "100m", from = "2020-06-01", path = db),
    "falling back to"
  )
  expect_equal(names(got), c("athlete_id", "event_id", "date"))
  expect_equal(got$athlete_id, "c")
  expect_true(data.table::is.data.table(got))
})

test_that("fallback aborts on a column the .rds lacks", {
  rds <- tempfile(fileext = ".rds"); saveRDS(.lst_fixture(), rds)
  db <- tempfile(fileext = ".duckdb")
  expect_error(suppressWarnings(
    load_store_table("championship_results", rds_path = rds,
                     columns = "athlete_name", path = db)), "no column")
})

test_that("DuckDB path is used when the table is there, with the same filters", {
  d <- .lst_fixture()
  db <- tempfile(fileext = ".duckdb")
  conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = db)
  DBI::dbWriteTable(conn, "championship_results", d)
  DBI::dbDisconnect(conn, shutdown = TRUE)
  # A .rds with different contents proves the DB copy was the one read.
  rds <- tempfile(fileext = ".rds"); saveRDS(d[0], rds)
  got <- load_store_table("championship_results", rds_path = rds,
                          columns = c("athlete_id", "place"),
                          athlete_ids = "a", path = db)
  expect_equal(names(got), c("athlete_id", "place"))
  expect_equal(sort(got$place), c(1L, 1L))
})
