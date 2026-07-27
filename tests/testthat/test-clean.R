test_that("duplicate athlete-race records collapse to the best mark", {
  dt <- data.table::data.table(
    race_key = c("r1", "r1", "r1", "r2"),
    athlete_id = c("a", "a", "b", "a"),
    perf = to_perf(c(14.84, 16.22, 15.10, 16.00), 1L),
    age = 25
  )
  out <- clean_results(dt)
  expect_equal(nrow(out), 3L)                      # a+b in r1, a in r2
  # The kept mark is the better one on the oriented scale, not the first seen.
  expect_equal(out[race_key == "r1" & athlete_id == "a"]$perf, to_perf(16.22, 1L))
})

test_that("an athlete with only a no-mark is kept, not dropped", {
  # No-mark rates are a measured quantity; discarding NA rows would calibrate
  # every foul rate to zero.
  dt <- data.table::data.table(
    race_key = c("r1", "r1"), athlete_id = c("a", "b"),
    perf = c(NA_real_, to_perf(15.1, 1L)), age = 25
  )
  out <- clean_results(dt)
  expect_equal(nrow(out), 2L)
  expect_true(is.na(out[athlete_id == "a"]$perf))
})

test_that("a duplicated athlete keeps a real mark over a no-mark", {
  dt <- data.table::data.table(
    race_key = c("r1", "r1"), athlete_id = c("a", "a"),
    perf = c(NA_real_, to_perf(15.1, 1L)), age = 25
  )
  out <- clean_results(dt)
  expect_equal(nrow(out), 1L)
  expect_false(is.na(out$perf))
})

test_that("impossible ages are cleared but their marks are kept", {
  dt <- data.table::data.table(
    race_key = c("r1", "r2", "r3"), athlete_id = c("a", "b", "c"),
    perf = to_perf(c(10.1, 10.2, 10.3), -1L),
    age = c(25, 141.4, 8),
    birthdate = as.Date(c("2000-01-01", "1884-12-10", "2018-01-01"))
  )
  out <- clean_results(dt)
  expect_equal(nrow(out), 3L)                      # no row dropped
  expect_equal(sum(is.na(out$age)), 2L)            # 141.4 and 8 cleared
  expect_true(all(!is.na(out$perf)))               # marks untouched
  expect_true(is.na(out[athlete_id == "b"]$birthdate))
})

test_that("clean_results handles empty input and missing columns", {
  expect_equal(nrow(clean_results(data.table::data.table())), 0L)
  bare <- data.table::data.table(perf = 1:3)
  expect_equal(nrow(clean_results(bare)), 3L)
})
