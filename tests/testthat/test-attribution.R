# The attribution converts percentage-point shares into expected golds and then
# partitions the gain. Both steps are arithmetic that must hold exactly, and a
# partition that does not sum back to the whole is the classic way an
# attribution table quietly lies.

golds_from_share <- function(share_pct, sport_golds) share_pct / 100 * sport_golds

test_that("shares convert to golds and back", {
  expect_equal(golds_from_share(50, 40), 20)
  expect_equal(golds_from_share(0, 40), 0)
  expect_equal(golds_from_share(100, 7), 7)
})

test_that("the gain is the difference of two gold counts, not of two shares", {
  # A share difference alone is meaningless across sports of different size:
  # +10 pp of a 40-gold sport is four golds, of a 2-gold sport is 0.2.
  big   <- golds_from_share(30, 40) - golds_from_share(20, 40)
  small <- golds_from_share(30, 2)  - golds_from_share(20, 2)
  expect_equal(big, 4)
  expect_equal(small, 0.2)
  expect_gt(big, small)
})

test_that("the four attribution components sum to the total gain", {
  # The observed split, per host edition.
  parts <- c(ind_access = 1.47, ind_perf = 7.05,
             team_access = 0.30, team_perf = 0.22)
  without <- 18.25; actual <- 27.29
  expect_equal(sum(parts), actual - without, tolerance = 0.02)
})

test_that("shares of the gain sum to 1 exactly, and to 100 within rounding", {
  parts <- c(1.47, 7.05, 0.30, 0.22)
  expect_equal(sum(parts / sum(parts)), 1)
  # Rounded to whole percents these are 16 / 78 / 3 / 2 = 99, not 100. That is
  # rounding, not a missing component -- assert the tolerance rather than
  # nudging a number to make the table look tidy.
  pct <- round(100 * parts / sum(parts))
  expect_lte(abs(sum(pct) - 100), length(parts) / 2)
})

test_that("gain per 100 available reorders categories against absolute gain", {
  # This is why the rate column exists: measured sports gain the most gold
  # outright and the least per gold available. A summary that reported only
  # one of the two would support the opposite conclusion.
  d <- data.frame(
    band      = c("measured", "judged"),
    available = c(95.85, 25.82),
    gained    = c(3.94, 2.01))
  d$rate <- 100 * d$gained / d$available
  expect_gt(d$gained[d$band == "measured"], d$gained[d$band == "judged"])
  expect_lt(d$rate[d$band == "measured"],  d$rate[d$band == "judged"])
})

test_that("a category with no golds available contributes no gain", {
  expect_equal(golds_from_share(50, 0), 0)
})
