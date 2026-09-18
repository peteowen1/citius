fake_params <- function(has_wind = TRUE) {
  list(event_id = "AT-Test-M", has_indoor = TRUE, indoor_coef = -0.002,
       wind = if (has_wind) list(grid = seq(-6, 8, by = 0.1), curve = 0.005 * seq(-6, 8, by = 0.1)) else NULL,
       altitude = list(grid_m = c(0, 100, 1000, 2000), curve = c(0, 0.0005, 0.002, 0.004), max_m = 2000),
       var_race = 5e-5, var_resid = 2e-4)
}

test_that("adjust_conditions interpolates, clamps, and treats NA as no correction", {
  p <- fake_params()
  a <- adjust_conditions(p, wind = c(2, -1, NA, 20), alt_m = c(0, 550, NA, 5000), indoor = c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(a$wind_adj, c(0.01, -0.005, 0, 0.04), tolerance = 1e-9)   # 20 m/s clamps to the +8 end
  expect_equal(a$venue_adj[2], 0.0005 + (550 - 100) / 900 * 0.0015)        # linear between grid points
  expect_equal(a$venue_adj[c(1, 3)], c(0, 0))
  expect_equal(a$venue_adj[4], 0.004)                                       # 5000 m clamps to the top
  expect_equal(a$indoor_adj, c(0, 0, -0.002, 0))
})

test_that("an event without a wind term gives zero wind correction", {
  a <- adjust_conditions(fake_params(has_wind = FALSE), wind = c(3, -3), alt_m = 0)
  expect_equal(a$wind_adj, c(0, 0))
})

test_that("race_shock is the shrunk field mean and shrinks harder for small fields", {
  k <- 5e-5; e <- 2e-4
  r <- rep(0.01, 4)
  expect_equal(race_shock(r, k, e), 0.01 * 4 * k / (4 * k + e))
  expect_lt(race_shock(rep(0.01, 2), k, e), race_shock(rep(0.01, 20), k, e))
  expect_lt(race_shock(rep(0.01, 20), k, e), 0.01)
})

test_that("one athlete pulling up cannot drag the whole field", {
  k <- 5e-5; e <- 2e-4
  field <- c(0.002, -0.006, -0.005, 0.013)
  clean <- race_shock(field, k, e)
  with_blowup <- race_shock(c(field, -0.169), k, e)
  uncapped    <- race_shock(c(field, -0.169), k, e, cap_sd = Inf)
  expect_lt(abs(with_blowup - clean), 0.006)
  expect_gt(abs(uncapped - clean), 0.015)
})

test_that("race_shock ignores athletes with no expectation and is 0 when nobody has one", {
  k <- 5e-5; e <- 2e-4
  expect_equal(race_shock(c(0.01, NA, 0.01), k, e), race_shock(c(0.01, 0.01), k, e))
  expect_equal(race_shock(c(NA, NA), k, e), 0)
  expect_equal(race_shock(numeric(), k, e), 0)
})

test_that("race_shock_loo never uses an athlete's own residual", {
  k <- 5e-5; e <- 2e-4
  r <- c(0.01, 0.02, NA, -0.5)
  loo <- race_shock_loo(r, k, e)
  expect_length(loo, 4)
  expect_equal(loo[3], race_shock(r, k, e))                 # no expectation: sees the whole field
  expect_equal(loo[1], race_shock(r[-1], k, e))             # own residual excluded
  expect_equal(loo[4], race_shock(c(0.01, 0.02), k, e))     # the blowup does not shape its own shock
  expect_equal(race_shock_loo(c(0.01, NA), k, e), c(0, race_shock(0.01, k, e)))  # nobody else -> 0
  expect_lt(race_shock_loo(0.05, k, e), 1e-12)               # lone runner: a PB stays a PB
})

test_that("conditions_params returns NULL for an event with no file", {
  expect_null(conditions_params("AT-Nothing-M", tempdir()))
})
