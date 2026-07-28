make_corpus <- function(n = 600) {
  set.seed(4)
  data.table::data.table(
    athlete_id = as.character(sample(1:80, n, TRUE)),
    event_id = sample(c("AT-100Metres-M", "AT-1500Metres-W", "AT-LongJump-M", NA), n, TRUE),
    date = as.Date("2020-01-01") + sample(0:2000, n, TRUE),
    perf = rnorm(n, -2.3, 0.05),
    place = sample(1:8, n, TRUE),
    round = sample(c("Final", "Round 1 - Heat"), n, TRUE),
    race_key = paste0("r", sample(1:60, n, TRUE)))
}

test_that("a corpus round-trips through the store unchanged", {
  d <- make_corpus()
  p <- file.path(tempdir(), "store_rt")
  write_results_store(d, p)
  back <- read_results_store(p)
  expect_equal(nrow(back), nrow(d))
  # Order is not preserved by partitioning, so compare as sets.
  expect_equal(sort(back$perf), sort(d$perf))
  expect_equal(sort(back$athlete_id), sort(d$athlete_id))
})

test_that("NA event_id survives the round trip", {
  # An NA partition value becomes __HIVE_DEFAULT_PARTITION__ and does not come
  # back as NA on its own. Unmatched events -- relays, age-group specs -- are a
  # real category, and losing them would silently shrink the corpus.
  d <- make_corpus()
  n_na <- sum(is.na(d$event_id))
  expect_gt(n_na, 0)
  p <- file.path(tempdir(), "store_na")
  write_results_store(d, p)
  back <- read_results_store(p)
  expect_equal(sum(is.na(back$event_id)), n_na)
  expect_false(any(back$event_id == "__unmatched__", na.rm = TRUE))
})

test_that("event and date filters return exactly what an in-memory filter would", {
  d <- make_corpus()
  p <- file.path(tempdir(), "store_f")
  write_results_store(d, p)
  ev <- "AT-100Metres-M"
  from <- as.Date("2021-01-01"); to <- as.Date("2023-01-01")
  got <- read_results_store(p, events = ev, from = from, to = to)
  want <- d[event_id == ev & date >= from & date <= to]
  expect_equal(nrow(got), nrow(want))
  expect_equal(sort(got$perf), sort(want$perf))
})

test_that("column selection returns only what was asked for", {
  d <- make_corpus()
  p <- file.path(tempdir(), "store_c")
  write_results_store(d, p)
  cols <- c("athlete_id", "event_id", "date", "perf")
  got <- read_results_store(p, columns = cols)
  expect_setequal(names(got), cols)
})

test_that("asking for a column the store lacks is an error, not a silent drop", {
  d <- make_corpus()
  p <- file.path(tempdir(), "store_m")
  write_results_store(d, p)
  expect_error(read_results_store(p, columns = c("perf", "not_a_column")),
               "not in store")
})

test_that("a missing store errors rather than returning nothing", {
  expect_error(read_results_store(file.path(tempdir(), "no_such_store")), "No store")
})

test_that("partitioning on an absent column is rejected", {
  expect_error(write_results_store(make_corpus(), file.path(tempdir(), "s"), "nope"),
               "not found")
})
