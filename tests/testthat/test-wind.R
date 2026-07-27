plant_wind <- function(beta = 0.0025, n_ath = 60, n_each = 40,
                       event_id = "AT-100Metres-M", sigma = 0.010, seed = 5) {
  set.seed(seed)
  ability <- stats::rnorm(n_ath, to_perf(10, -1L), 0.02)
  data.table::rbindlist(lapply(seq_len(n_ath), function(i) {
    w <- stats::runif(n_each, -3, 3)
    data.table::data.table(
      athlete_id = as.character(i), event_id = event_id,
      date = Sys.Date() - seq_len(n_each), round = "F", tier = "OW",
      wind = w,
      perf = ability[i] + beta * w + stats::rnorm(n_each, 0, sigma))
  }))
}

test_that("the planted wind coefficient is recovered", {
  d <- plant_wind(beta = 0.0025)
  w <- fit_wind_effect(d)
  expect_equal(nrow(w), 1)
  expect_lt(abs(w$beta - 0.0025), 0.0006)
})

test_that("a larger planted effect gives a larger coefficient", {
  small <- fit_wind_effect(plant_wind(beta = 0.001))
  big <- fit_wind_effect(plant_wind(beta = 0.005))
  expect_lt(small$beta, big$beta)
})

test_that("ability differences do not leak into the coefficient", {
  # Athletes have a wide ability spread but an identical wind response. A
  # cross-athlete fit would be contaminated; a within-athlete one is not.
  d <- plant_wind(beta = 0.0025)
  expect_lt(abs(fit_wind_effect(d)$beta - 0.0025), 0.0006)
})

test_that("events without wind data are simply absent", {
  d <- plant_wind()
  d[, wind := NA_real_]
  expect_equal(nrow(fit_wind_effect(d)), 0)
})

test_that("events with too few marks are not fitted", {
  d <- plant_wind(n_ath = 3, n_each = 5)
  expect_equal(nrow(fit_wind_effect(d)), 0)
})

test_that("extreme wind readings are excluded as recording errors", {
  d <- plant_wind(beta = 0.0025)
  d <- rbind(d, data.table::data.table(
    athlete_id = "1", event_id = "AT-100Metres-M", date = Sys.Date(),
    round = "F", tier = "OW", wind = c(50, -50), perf = to_perf(c(9, 12), -1L)))
  w <- fit_wind_effect(d)
  expect_lt(abs(w$beta - 0.0025), 0.0006)
})

test_that("adjustment removes the wind effect from marks", {
  d <- plant_wind(beta = 0.0025)
  w <- fit_wind_effect(d)
  adj <- adjust_wind(d, w)
  # After adjustment there should be no residual relationship with wind
  adj[, dev := perf - mean(perf), by = athlete_id]
  slope <- unname(stats::coef(stats::lm(dev ~ wind, data = adj))[2])
  expect_lt(abs(slope), 0.0004)
})

test_that("adjustment shrinks within-athlete spread", {
  # The whole point: variation charged to the athlete is reattributed to the
  # conditions, so sigma falls.
  d <- plant_wind(beta = 0.004, sigma = 0.006)
  before <- d[, .(s = stats::sd(perf)), by = athlete_id][, mean(s)]
  adj <- adjust_wind(d, fit_wind_effect(d))
  after <- adj[, .(s = stats::sd(perf)), by = athlete_id][, mean(s)]
  expect_lt(after, before)
})

test_that("rows without wind are left untouched", {
  d <- plant_wind(beta = 0.0025)
  w <- fit_wind_effect(d)
  d2 <- data.table::copy(d)
  d2[1:10, wind := NA_real_]
  adj <- adjust_wind(d2, w)
  expect_equal(adj$perf[1:10], d2$perf[1:10])
  expect_equal(adj$wind_adj[1:10], rep(0, 10))
})

test_that("an empty wind_effect leaves everything unchanged", {
  d <- plant_wind()
  adj <- adjust_wind(d, fit_wind_effect(d[0]))
  expect_equal(adj$perf, d$perf)
})
