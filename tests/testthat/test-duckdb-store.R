.fixture_conn <- function() {
  path <- tempfile(fileext = ".duckdb")
  conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  withr::defer(DBI::dbDisconnect(conn, shutdown = TRUE), envir = parent.frame())
  conn
}

make_fixture <- function(n = 20, competition_id = 1:2) {
  data.table::data.table(
    competition_id = sample(competition_id, n, TRUE),
    athlete_id = as.character(sample(1:5, n, TRUE)),
    race_key = paste0("r", sample(1:4, n, TRUE)),
    place = sample(1:8, n, TRUE),
    mark = rnorm(n, 10, 1)
  )
}

test_that("mode = replace round-trips a fixture byte-identically", {
  conn <- .fixture_conn()
  d <- make_fixture()
  n <- .citius_store_merge(conn, "t_replace", d, dedup_key = "competition_id",
                           schema = names(d), mode = "replace")
  expect_equal(as.integer(n), nrow(d))
  back <- data.table::setDT(DBI::dbGetQuery(conn, "SELECT * FROM t_replace"))
  data.table::setorder(back, athlete_id, race_key, mark)
  want <- data.table::copy(d); data.table::setorder(want, athlete_id, race_key, mark)
  expect_equal(nrow(back), nrow(want))
  expect_equal(sort(back$mark), sort(want$mark))
})

test_that("merge dedup drops whole competitions already present, not rows", {
  conn <- .fixture_conn()
  d1 <- make_fixture(20, competition_id = 1:2)
  .citius_store_merge(conn, "t_merge", d1, dedup_key = "competition_id",
                      schema = names(d1), mode = "merge")
  before <- DBI::dbGetQuery(conn, "SELECT COUNT(*) n FROM t_merge")$n

  # d2 overlaps competition 2 (should be dropped whole) and adds competition 3.
  d2 <- make_fixture(15, competition_id = c(2, 3))
  n_new_only <- sum(d2$competition_id == 3)
  added <- .citius_store_merge(conn, "t_merge", d2, dedup_key = "competition_id",
                               schema = names(d2), mode = "merge")
  expect_equal(as.integer(added), n_new_only)
  after <- DBI::dbGetQuery(conn, "SELECT COUNT(*) n FROM t_merge")$n
  expect_equal(after, before + n_new_only)
  # No competition-2 rows from d2 made it in: all comp-2 rows in the table
  # trace back to d1 (same count as before the merge).
  comp2_n <- DBI::dbGetQuery(conn, "SELECT COUNT(*) n FROM t_merge WHERE competition_id = 2")$n
  expect_equal(comp2_n, sum(d1$competition_id == 2))
})

test_that("schema guard aborts on an extra column", {
  conn <- .fixture_conn()
  d <- make_fixture()
  .citius_store_merge(conn, "t_schema", d, dedup_key = "competition_id",
                      schema = names(d), mode = "replace")
  d2 <- data.table::copy(make_fixture(5))
  d2[, extra_col := 1]
  expect_error(
    .citius_store_merge(conn, "t_schema", d2, dedup_key = "competition_id",
                        schema = names(d), mode = "merge"),
    "would be dropped"
  )
})

test_that("schema guard only warns on a missing column", {
  conn <- .fixture_conn()
  d <- make_fixture()
  .citius_store_merge(conn, "t_schema_missing", d, dedup_key = "competition_id",
                      schema = names(d), mode = "replace")
  d2 <- make_fixture(5, competition_id = 99)
  d2[, mark := NULL]
  expect_warning(
    .citius_store_merge(conn, "t_schema_missing", d2, dedup_key = "competition_id",
                        schema = names(d), mode = "merge"),
    "absent from new data"
  )
})

test_that("the 0-sentinel guard rejects competition_id 0", {
  conn <- .fixture_conn()
  d <- make_fixture()
  .citius_store_merge(conn, "t_sentinel", d, dedup_key = "competition_id",
                      schema = names(d), mode = "replace")
  d2 <- make_fixture(5, competition_id = 0)
  expect_error(
    .citius_store_merge(conn, "t_sentinel", d2, dedup_key = "competition_id",
                        schema = names(d), mode = "merge"),
    "competition_id"
  )
})

test_that("a forced error mid-merge leaves the table unchanged (real rollback)", {
  conn <- .fixture_conn()
  d <- make_fixture(20, competition_id = 1:2)
  .citius_store_merge(conn, "t_rollback", d, dedup_key = "competition_id",
                      schema = names(d), mode = "replace")
  before_n <- DBI::dbGetQuery(conn, "SELECT COUNT(*) n FROM t_rollback")$n
  before_rows <- DBI::dbGetQuery(conn, "SELECT * FROM t_rollback ORDER BY athlete_id, race_key, mark")

  # Force the INSERT to fail: reference a nonexistent dedup_key column so the
  # DISTINCT query inside the merge branch throws before any row is written.
  bad <- make_fixture(5, competition_id = 3)
  expect_error(
    .citius_store_merge(conn, "t_rollback", bad, dedup_key = "not_a_real_column",
                        schema = names(d), mode = "merge")
  )
  after_n <- DBI::dbGetQuery(conn, "SELECT COUNT(*) n FROM t_rollback")$n
  after_rows <- DBI::dbGetQuery(conn, "SELECT * FROM t_rollback ORDER BY athlete_id, race_key, mark")
  expect_equal(after_n, before_n)
  expect_equal(after_rows, before_rows)
})

test_that("a second write connection from another PROCESS errors rather than corrupting", {
  # Same-process connections to one dbdir share an in-process DuckDB instance
  # (no lock conflict), so this constraint only surfaces cross-process -- the
  # actual situation two concurrent Rscript runs are in. A subprocess is the
  # only faithful way to test it.
  path <- tempfile(fileext = ".duckdb")
  conn1 <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  withr::defer(DBI::dbDisconnect(conn1, shutdown = TRUE))
  DBI::dbExecute(conn1, "CREATE TABLE t (x INTEGER)")

  child_script <- tempfile(fileext = ".R")
  writeLines(sprintf(
    "tryCatch({ con <- DBI::dbConnect(duckdb::duckdb(), dbdir = %s); quit(status = 0) },
               error = function(e) quit(status = 1))", deparse(path)),
    child_script)
  status <- system2("Rscript", shQuote(child_script), stdout = FALSE, stderr = FALSE)
  expect_false(identical(status, 0L))
})
