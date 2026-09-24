# The banded altitude adjustment inside estimate_ability().
#
# Pinned: the band edges, that a matched band moves ability by its beta and
# only for the rows at that altitude, that an unknown altitude is left alone
# rather than treated as sea level, and that a calibration whose keys match
# nothing says so (commit 3cf03a5) instead of going quietly inert.

test_that(".altitude_band puts each edge in the upper band and NA stays NA", {
  expect_equal(.altitude_band(c(0, 199.9, 200, 799, 800, 1500, 2199, 2200, 4000, NA, Inf)),
               c("<200", "<200", "200-800", "200-800", "800-1500", "1500-2200",
                 "1500-2200", ">2200", ">2200", NA, NA))
})

alt_cal <- function(beta = 0.02, family = "sprint", sex = "M", band = ">2200") {
  list(altitude = data.table::data.table(family = family, sex = sex, has_cr = FALSE,
                                         band = band, beta = beta))
}

test_that("a matched band moves the athlete's rating by beta; sea-level marks do not move", {
  h <- synthetic_history(n_athletes = 12, n_each = 20)
  h[, alt_m := ifelse(athlete_id == "1", 2400, 50)]
  # The layer lives in the adjust_context pass. The base run carries the same
  # table at beta = 0, so every other adjustment is identical between the two.
  base <- estimate_ability(h, calibration = alt_cal(0))
  adj  <- estimate_ability(h, calibration = alt_cal(0.02))
  data.table::setkey(base, athlete_id); data.table::setkey(adj, athlete_id)
  # every mark of athlete 1 was at >2200m, so the level drops by exactly beta
  expect_equal(adj["1"]$ability_raw - base["1"]$ability_raw, -0.02, tolerance = 1e-6)
  # nobody else raced at altitude
  expect_equal(adj[athlete_id != "1"]$ability_raw, base[athlete_id != "1"]$ability_raw, tolerance = 1e-12)
})

test_that("an unknown altitude is left alone, not corrected as if it were sea level", {
  h <- synthetic_history(n_athletes = 12, n_each = 20)
  h[, alt_m := ifelse(athlete_id == "1", NA_real_, 2400)]
  base <- estimate_ability(h, calibration = alt_cal(0))
  adj  <- estimate_ability(h, calibration = alt_cal(0.02))
  data.table::setkey(base, athlete_id); data.table::setkey(adj, athlete_id)
  expect_equal(adj["1"]$ability_raw, base["1"]$ability_raw, tolerance = 1e-12)
})

test_that("a calibration whose keys match no row warns instead of doing nothing quietly", {
  h <- synthetic_history(n_athletes = 12, n_each = 20)
  h[, alt_m := 2400]
  expect_warning(estimate_ability(h, calibration = alt_cal(0.02, family = "Sprint")),
                 "matched no rows")
})
