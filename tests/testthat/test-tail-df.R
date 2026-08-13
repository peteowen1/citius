# fit_tail_df() accepts either raw results or the list produced by
# decompose_races(); the fast path used here plants residuals directly so
# the fixture is a known-df sample rather than something re-derived through
# the whole decomposition.
plant_tail <- function(df = 5, n = 20000L, event_id = "AT-100Metres-M", seed = 41) {
  set.seed(seed)
  resid <- if (is.infinite(df)) stats::rnorm(n) else stats::rt(n, df = df)
  data.table::data.table(resid = resid, event_id = event_id, shared = TRUE)
}

test_that("a planted heavy-tailed df = 5 sample recovers a low best-fit df", {
  # REGRESSION for the scale inversion fixed in R/calibrate.R on 2026-08-14.
  #
  # fit_tail_df() standardises to z = resid / sd(resid), a unit-variance
  # residual, so the expected tail mass of candidate v is
  # `2 * pt(-probes * sqrt(v / (v - 2)), df = v)` -- probe MULTIPLIED by the
  # t scale. The formula shipped for weeks with the reciprocal (divide), and
  # under it this very fixture fitted as df ~ 30 with the true v = 5 ranked
  # WORST of all ten candidates (verified on 2M exact-scale draws: multiply
  # gives relative err 0.029 at v = 5; divide gave 3.37). The direction was
  # the dangerous one -- genuinely heavy tails reported as nearly normal,
  # thinning the simulator's tails in exactly the way the function exists to
  # prevent. Planted-NORMAL input is unaffected (see the test below), which is
  # why no earlier test caught it: this file is the first to plant a genuinely
  # heavy-tailed sample. If this test ever fails again, suspect that formula
  # first.
  out <- fit_tail_df(list(data = plant_tail(df = 5)))
  expect_gt(nrow(out), 0)
  expect_lte(out$df[1], 8)
})

test_that("planted normal residuals recover a high (near-normal) best-fit df", {
  out <- fit_tail_df(list(data = plant_tail(df = Inf)))
  expect_gt(nrow(out), 0)
  expect_gte(out$df[1], 30)
})

test_that("empty input returns the empty table", {
  empty <- list(data = data.table::data.table(
    resid = numeric(), event_id = character(), shared = logical()))
  out <- fit_tail_df(empty)
  expect_equal(nrow(out), 0)
  expect_equal(names(out), c("df", "err"))
})

test_that("fewer than 100 usable z-scores returns the empty table", {
  out <- fit_tail_df(list(data = plant_tail(df = 5, n = 80L)))
  expect_equal(nrow(out), 0)
})
