# A corpus whose tier codes are all mid ("D") or low ("F") -- no OW/GW/GL row
# anywhere, so the "top" reference for the tier offset does not exist and
# estimate_context_effects() has to fall back to the max.
plant_no_top_tier <- function(n_ath = 40, n_each = 30, seed = 71) {
  set.seed(seed)
  ability <- stats::rnorm(n_ath, to_perf(10, -1L), 0.05)
  data.table::rbindlist(lapply(seq_len(n_ath), function(i) {
    tier <- sample(c("D", "F"), n_each, replace = TRUE, prob = c(0.7, 0.3))
    round <- sample(c("F", "H1"), n_each, replace = TRUE)
    data.table::data.table(
      athlete_id = as.character(i), event_id = "AT-100Metres-M",
      tier = tier, round = round, date = Sys.Date() - seq_len(n_each),
      perf = ability[i] + ifelse(tier == "F", -0.01, 0) +
        stats::rnorm(n_each, 0, 0.008))
  }))
}

test_that("tier offsets stay finite with no top-tier rows, referenced to the max", {
  d <- plant_no_top_tier()
  ctx <- estimate_context_effects(d)
  expect_true(is.numeric(ctx$tier))
  expect_true(all(is.finite(ctx$tier)))
  # No "top" class present, so the fallback references the fastest existing
  # context (the max) instead -- everything else must come out <= that,
  # i.e. <= 0 after subtraction, and the reference itself is exactly 0.
  expect_true(any(abs(ctx$tier) < 1e-9))
  expect_true(all(ctx$tier <= 1e-9))
})

test_that("estimate_ability(adjust_context = TRUE) stays finite on a no-top-tier corpus", {
  d <- plant_no_top_tier()
  ab <- estimate_ability(d, adjust_context = TRUE, calibration = NULL)
  expect_gt(nrow(ab), 0)
  expect_true(all(is.finite(ab$ability)))
})
