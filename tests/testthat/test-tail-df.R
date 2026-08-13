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
  # KNOWN FAILING as of 2026-08-14, and deliberately left asserting the CORRECT
  # behaviour rather than the observed one -- see the note below for why.
  #
  # fit_tail_df() standardises to z = resid / sd(resid), i.e. a unit-variance
  # residual, then compares against `expected <- 2 * pt(-probes / sqrt(v / (v
  # - 2)), df = v)` for each candidate v. That divides the probe by
  # sqrt(v/(v-2)); matching a unit-variance scaled-t (as simulate_event()
  # builds it: `noise <- rt(df) * sqrt((df-2)/df)`) requires MULTIPLYING by
  # sqrt(v/(v-2)) instead -- the two are reciprocals of each other, and the
  # formula in R/calibrate.R uses the wrong one.
  #
  # Verified empirically: for a genuine unit-variance scaled t(5) sample (2M
  # draws, exact known scale factor, no sample-sd estimation noise), the
  # *correct* formula (multiply) matches the observed P(|Z|>k) closely at
  # v = 5 (relative err 0.029, the best of all candidates) while the formula
  # actually in the package (divide) picks v = 30 (err 0.285) and rates the
  # true v = 5 candidate as the WORST fit of all ten (err 3.37). The direction
  # of the bug is the dangerous one: genuinely heavy-tailed input gets
  # reported as nearly normal, silently thinning the simulator's tails in
  # exactly the way this function exists to prevent (see its own docstring on
  # the old hard-coded df = 6). Planted-normal input is unaffected by the bug
  # (see the test below), which is exactly why it went uncaught -- there was
  # no prior test planting a genuinely heavy-tailed sample.
  #
  # Left asserting correct recovery, not the observed df ~30, because a test
  # that encodes the bug as "expected" would cement it -- the opposite of what
  # every "plant a known effect and recover it" test in this suite is for.
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
