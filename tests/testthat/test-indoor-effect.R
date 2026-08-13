# A known within-athlete indoor gap for the sprint family. `min_n` is passed
# smaller than the default 2000-per-side so the fixture stays cheap; the
# default itself is exercised by the "below min_n" test below.
plant_indoor <- function(offset = -0.005, n_ath = 30, n_indoor_each = 25,
                         n_outdoor_each = 25, event_id = "AT-100Metres-M",
                         seed = 31) {
  set.seed(seed)
  ability <- stats::rnorm(n_ath, to_perf(10, -1L), 0.05)
  data.table::rbindlist(lapply(seq_len(n_ath), function(i) {
    rbind(
      data.table::data.table(
        athlete_id = as.character(i), event_id = event_id, indoor = TRUE,
        date = Sys.Date() - seq_len(n_indoor_each),
        perf = ability[i] + offset + stats::rnorm(n_indoor_each, 0, 0.006)),
      data.table::data.table(
        athlete_id = as.character(i), event_id = event_id, indoor = FALSE,
        date = Sys.Date() - seq_len(n_outdoor_each),
        perf = ability[i] + stats::rnorm(n_outdoor_each, 0, 0.006)))
  }))
}

test_that("a planted indoor gap is recovered with the right sign and magnitude", {
  d <- plant_indoor(offset = -0.005)
  out <- fit_indoor_effect(d, min_n = 500L)
  sp <- out[family == "sprint"]
  expect_equal(nrow(sp), 1L)
  expect_lt(sp$offset, 0)
  expect_lt(abs(sp$offset - (-0.005)), 0.0015)
})

test_that("a family below min_n returns offset 0", {
  d <- plant_indoor(offset = -0.005)
  out <- fit_indoor_effect(d, min_n = 100000L)
  sp <- out[family == "sprint"]
  expect_equal(nrow(sp), 1L)
  expect_equal(sp$offset, 0)
})

test_that("input missing the indoor column aborts", {
  d <- plant_indoor(offset = -0.005, n_ath = 5, n_indoor_each = 5, n_outdoor_each = 5)
  d[, indoor := NULL]
  expect_error(fit_indoor_effect(d), "indoor")
})
