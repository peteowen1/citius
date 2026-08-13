# A known slope beta on a within-athlete-varying covariate x, planted on top of
# an athlete ability spread. `confound` optionally makes an athlete's mean x
# correlate with their ability -- the cross-sectional trap fit_numeric_effect()
# must not fall into, because it fits within athlete-event.
plant_numeric <- function(beta = 0.01, n_ath = 60, n_each = 40, sigma = 0.01,
                          confound = 0, seed = 21) {
  set.seed(seed)
  ability <- stats::rnorm(n_ath, to_perf(10, -1L), 0.05)
  data.table::rbindlist(lapply(seq_len(n_ath), function(i) {
    x_mean <- confound * (ability[i] - mean(ability))
    x <- x_mean + stats::rnorm(n_each, 0, 1)
    data.table::data.table(
      athlete_id = as.character(i), event_id = "AT-100Metres-M",
      x = x, date = Sys.Date() - seq_len(n_each),
      perf = ability[i] + beta * x + stats::rnorm(n_each, 0, sigma))
  }))
}

test_that("a planted numeric slope is recovered", {
  d <- plant_numeric(beta = 0.01)
  f <- fit_numeric_effect(d, "x")
  expect_equal(nrow(f), 1L)
  expect_lt(abs(f$beta[1] - 0.01), 0.003)
})

test_that("an athlete-level confound in the covariate does not bias the within-athlete slope", {
  # Better athletes run systematically higher x on average. A cross-sectional
  # regression would absorb that into its slope; the within-athlete estimator
  # must not, because xdev is centred per athlete-event and the confound is
  # constant within that group.
  d <- plant_numeric(beta = 0.01, confound = 20)
  f <- fit_numeric_effect(d, "x")
  expect_lt(abs(f$beta[1] - 0.01), 0.004)

  # Confirm the confound is real: the naive cross-sectional slope is well off
  # the planted beta, so a correct recovery above is not a coincidence of a
  # weak confound.
  naive <- stats::coef(stats::lm(perf ~ x, data = d))[["x"]]
  expect_gt(abs(naive - 0.01), 0.01)
})

test_that("adjust_numeric then refitting recovers a beta near zero", {
  d <- plant_numeric(beta = 0.01)
  f <- fit_numeric_effect(d, "x")
  adj <- adjust_numeric(d, f, "x")
  f2 <- fit_numeric_effect(adj, "x")
  expect_lt(abs(f2$beta[1]), 0.003)
})

test_that("a NULL numeric_effect gives numeric_adj all zero", {
  d <- plant_numeric(beta = 0.01, n_ath = 5, n_each = 5)
  adj <- adjust_numeric(d, NULL, "x")
  expect_true(all(adj$numeric_adj == 0))
})

test_that("an empty numeric_effect gives numeric_adj all zero", {
  d <- plant_numeric(beta = 0.01, n_ath = 5, n_each = 5)
  empty <- data.table::data.table(event_id = character(), beta = numeric(),
                                  n = integer(), r2 = numeric())
  adj <- adjust_numeric(d, empty, "x")
  expect_true(all(adj$numeric_adj == 0))
})

test_that("a missing covariate column gives numeric_adj all zero", {
  d <- plant_numeric(beta = 0.01, n_ath = 5, n_each = 5)
  f <- fit_numeric_effect(d, "x")
  adj <- adjust_numeric(d, f, "nonexistent")
  expect_true(all(adj$numeric_adj == 0))
})
