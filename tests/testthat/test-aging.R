skip_if_not_installed("mgcv")

# Athletes observed at many ages, with a planted quadratic age curve peaking at
# a known age. Ability varies across athletes so the curve must be recovered
# from within-athlete variation, not from comparing athletes to each other.
synthetic_careers <- function(n_athletes = 120, peak = 26, curvature = 0.0004,
                              event_id = "AT-100Metres-M", sigma = 0.004,
                              age_lo = 19, age_hi = 34, seed = 12) {
  set.seed(seed)
  ability <- stats::rnorm(n_athletes, to_perf(10, -1L), 0.03)
  data.table::rbindlist(lapply(seq_len(n_athletes), function(i) {
    ages <- seq(age_lo, age_hi, by = 0.5)
    data.table::data.table(
      athlete_id = as.character(i), event_id = event_id,
      date = Sys.Date() - seq_along(ages), age = ages,
      round = "F", tier = "OW",
      perf = ability[i] - curvature * (ages - peak)^2 +
        stats::rnorm(length(ages), 0, sigma))
  }))
}

test_that("the planted peak age is recovered", {
  ag <- fit_aging_curve(synthetic_careers(peak = 26))
  expect_s3_class(ag, "citius_aging")
  sprint <- ag$peaks[family == "sprint"]
  expect_true(sprint$peak_identified)
  expect_lt(abs(sprint$peak_age - 26), 1.5)
})

test_that("a different planted peak moves the estimate", {
  young <- fit_aging_curve(synthetic_careers(peak = 22, age_lo = 17, age_hi = 32))
  old <- fit_aging_curve(synthetic_careers(peak = 31, age_lo = 22, age_hi = 38))
  expect_lt(young$peaks[family == "sprint"]$peak_age,
            old$peaks[family == "sprint"]$peak_age)
})

test_that("ability differences between athletes do not leak into the curve", {
  # Athletes are given a strong ability spread but an identical age curve. A
  # cross-sectional fit would be contaminated; a within-athlete one is not.
  wide <- synthetic_careers(peak = 26)
  ag <- fit_aging_curve(wide)
  expect_lt(abs(ag$peaks[family == "sprint"]$peak_age - 26), 1.5)
})

test_that("a peak at the edge of support is flagged, not reported as fact", {
  # Observation stops at 28 while the true peak is 34: the curve is still
  # rising where the data ends, so the peak is not identified.
  suppressWarnings({
    ag <- fit_aging_curve(synthetic_careers(peak = 34, age_lo = 18, age_hi = 28))
  })
  expect_false(ag$peaks[family == "sprint"]$peak_identified)
})

test_that("fitting warns when a peak is unidentified", {
  expect_warning(
    fit_aging_curve(synthetic_careers(peak = 34, age_lo = 18, age_hi = 28)),
    "not identified"
  )
})

test_that("sparse tails cannot set the peak", {
  # A handful of results at age 40 must not outvote thousands in the twenties.
  main <- synthetic_careers(peak = 26, age_lo = 20, age_hi = 30)
  tail_rows <- data.table::data.table(
    athlete_id = rep(c("t1", "t2"), each = 3), event_id = "AT-100Metres-M",
    date = Sys.Date() - 1:6, age = rep(c(39, 40, 41), 2),
    round = "F", tier = "OW",
    perf = to_perf(10, -1L) + c(0, 0.05, 0.10, 0, 0.05, 0.10))
  ag <- suppressWarnings(fit_aging_curve(rbind(main, tail_rows)))
  expect_lt(ag$peaks[family == "sprint"]$peak_age, 32)
})

test_that("age_adjustment is zero at peak and negative away from it", {
  ag <- fit_aging_curve(synthetic_careers(peak = 26))
  peak <- ag$peaks[family == "sprint"]$peak_age
  expect_equal(age_adjustment(peak, "AT-100Metres-M", ag), 0, tolerance = 1e-6)
  expect_lt(age_adjustment(peak - 6, "AT-100Metres-M", ag), 0)
  expect_lt(age_adjustment(peak + 6, "AT-100Metres-M", ag), 0)
})

test_that("age_adjustment degrades to no correction without a curve", {
  expect_equal(age_adjustment(c(22, 30), "AT-100Metres-M", NULL), c(0, 0))
  expect_equal(age_adjustment(25, "SW-100mFreestyle-M", .empty_aging()), 0)
})

test_that("project_ability shifts a young athlete toward peak", {
  ag <- fit_aging_curve(synthetic_careers(peak = 26))
  ab <- data.table::data.table(
    athlete_id = "x", event_id = "AT-100Metres-M",
    ability = to_perf(10, -1L), sigma = 0.01,
    age_ref = 20, age_now = 26)
  out <- project_ability(ab, ag)
  expect_gt(out$ability, ab$ability)   # improving toward peak
  expect_gt(out$age_shift, 0)
})

test_that("project_ability requires the age columns", {
  ag <- fit_aging_curve(synthetic_careers())
  bad <- data.table::data.table(athlete_id = "x", event_id = "AT-100Metres-M",
                                ability = 1, sigma = 0.01)
  expect_error(project_ability(bad, ag), "missing required")
})

test_that("empty input returns an empty curve object", {
  ag <- fit_aging_curve(synthetic_careers()[0])
  expect_s3_class(ag, "citius_aging")
  expect_equal(nrow(ag$peaks), 0)
})

test_that("estimate_ability reports the weighted mean age, not the career mean", {
  # An athlete with a long junior record plus recent senior form: recency
  # weighting means the estimate reflects the recent races, so age_ref must sit
  # near the recent ages, not the midpoint of the whole career.
  today <- as.Date("2026-07-30")
  h <- data.table::data.table(
    athlete_id = "x", event_id = "AT-100Metres-M",
    date = c(today - (8:5) * 365, today - (60:1) * 7),
    tier = "OW", round = "F"
  )
  h[, age := 18 + as.numeric(date - min(date)) / 365.25]
  h[, perf := to_perf(10, -1L)]

  ab <- estimate_ability(h, as_of = today, adjust_context = FALSE, half_life = 365)
  expect_true("age_ref" %in% names(ab))
  expect_gt(ab$age_ref, mean(h$age))       # weighted toward recent, older ages
  expect_lt(abs(ab$age_ref - max(h$age)), 2)
})

test_that("project_ability warns on an implausibly large shift", {
  # Steep curve spanning the junior years, so a career-mean age_ref produces a
  # shift big enough to trip the guard — which is exactly the real failure:
  # a junior-heavy career mean projected all the way to peak.
  ag <- fit_aging_curve(synthetic_careers(peak = 26, curvature = 0.0012,
                                          age_lo = 16, age_hi = 32))
  bad <- data.table::data.table(
    athlete_id = "x", event_id = "AT-100Metres-M",
    ability = to_perf(10, -1L), sigma = 0.01,
    age_ref = 16, age_now = 26)          # career-mean-age mistake
  rlang::reset_warning_verbosity("citius_big_age_shift")
  expect_warning(project_ability(bad, ag), "age_ref")
})

test_that("a correctly-referenced projection produces a small shift", {
  ag <- fit_aging_curve(synthetic_careers(peak = 26))
  ok <- data.table::data.table(
    athlete_id = "x", event_id = "AT-100Metres-M",
    ability = to_perf(10, -1L), sigma = 0.01,
    age_ref = 24.5, age_now = 26)
  out <- project_ability(ok, ag)
  expect_lt(abs(out$age_shift), 0.02)
})

test_that("fit_aging_curve tolerates data that already has a family column", {
  # Reusing enriched data used to produce "cannot coerce type 'closure'": the
  # merge made family.x/family.y and the NSE fell through to stats::family.
  d <- synthetic_careers(peak = 26)
  d[, family := "sprint"]
  ag <- fit_aging_curve(d)
  expect_s3_class(ag, "citius_aging")
  expect_lt(abs(ag$peaks[family == "sprint"]$peak_age - 26), 1.5)
})

test_that("first differences recover an aging curve that centring flattens", {
  # Centring within athlete-event is fixed effects on an UNBALANCED panel: the
  # deviation at a given age depends on how much of the career was observed, so
  # the tails -- where the span mix is most skewed -- get flattened. Plant a
  # known curve on a deliberately unbalanced panel and check both estimators.
  set.seed(11)
  true_eff <- function(a) -0.004 * (a - 26)^2 / 4
  rows <- data.table::rbindlist(lapply(1:900, function(i) {
    start <- sample(16:32, 1); span <- sample(2:12, 1)
    ages <- start:min(start + span, 38)
    data.table::data.table(
      athlete_id = as.character(i), event_id = "AT-100Metres-M",
      age = ages, date = as.Date("2000-01-01") + (ages - 16) * 365,
      perf = to_perf(10.2, -1L) + stats::rnorm(1, 0, 0.02) + true_eff(ages) +
        stats::rnorm(length(ages), 0, 0.008))
  }))
  truth16 <- true_eff(16) - true_eff(26)

  at <- function(f, a) {
    cv <- data.table::as.data.table(f$curves)
    cv[which.min(abs(age - a))]$effect
  }
  cen <- fit_aging_curve(rows, min_results = 50L, method = "centred")
  dif <- fit_aging_curve(rows, min_results = 50L, method = "difference")

  # Differencing must recover more of the planted tail than centring does.
  expect_gt(abs(at(dif, 16)), abs(at(cen, 16)))
  # And it must land near the true peak, which centring misses.
  expect_lt(abs(dif$peaks$peak_age[1] - 26), 1.5)
  # Sanity: it should not overshoot the planted effect wildly.
  expect_lt(abs(at(dif, 16)), abs(truth16) * 1.5)
})
