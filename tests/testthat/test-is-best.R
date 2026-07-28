make_results <- function(n = 200, best_frac = 0.5) {
  set.seed(42)
  ath <- rep(sprintf("A%02d", 1:20), length.out = n)
  data.table::data.table(
    athlete_id = ath, event_id = "SW-100mFreestyle-M",
    competition_id = rep(sprintf("C%02d", 1:10), length.out = n),
    race_key = rep(sprintf("R%02d", 1:10), length.out = n),
    round = "Final", date = as.Date("2020-01-01") + seq_len(n),
    perf = -log(50 + stats::rnorm(n)),
    is_best = rep(c(TRUE, FALSE), c(round(n * best_frac), n - round(n * best_frac))))
}

test_that("variance estimators drop ranked-list rows", {
  r <- make_results()
  # A maximum is truncated at the good end, so its spread is not the athlete's
  # spread; including it understates sigma_within and makes favourites look
  # safer than they are.
  expect_message(calibrate(r), "ranked-list row")
  expect_message(fit_half_life(r), "ranked-list row")
})

test_that("the drop is announced, never silent", {
  # A silent filter is how a source quietly stops contributing without anyone
  # noticing it has.
  r <- make_results()
  expect_message(calibrate(r), "cannot support")
})

test_that("results without an is_best column are untouched", {
  r <- make_results()
  r[, is_best := NULL]
  expect_silent(citius:::.drop_best_only(r, "test()"))
  expect_equal(nrow(citius:::.drop_best_only(r, "test()")), nrow(r))
})

test_that("an all-FALSE is_best column does not trigger the message", {
  r <- make_results(best_frac = 0)
  expect_silent(citius:::.drop_best_only(r, "test()"))
})

test_that("NA in is_best is treated as not-a-best rather than dropped", {
  # Rows from feeds that predate the flag must not silently vanish.
  r <- make_results(best_frac = 0)
  r[1:10, is_best := NA]
  expect_equal(nrow(citius:::.drop_best_only(r, "test()")), nrow(r))
})
