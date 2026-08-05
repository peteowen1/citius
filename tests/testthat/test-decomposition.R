# The access/performance split of a host's team-sport gain is an identity, not
# a model. It is tested here because the arithmetic is easy to get subtly wrong
# and the two terms are the headline result.
#
#   E[share | home] - E[share | away]
#     = (P_home - P_away) * S_away        <- access: turning up more
#     + P_home * (S_home - S_away)        <- performance: winning more once in
#
# where P is the probability the nation is in the tournament and S the share it
# takes when it is (share is 0 when it is not).

decompose <- function(p_home, p_away, s_home, s_away) {
  list(access = (p_home - p_away) * s_away,
       performance = p_home * (s_home - s_away))
}

test_that("the two terms sum to the observed gain", {
  p_home <- 0.957; p_away <- 0.655; s_home <- 18.70; s_away <- 15.32
  d <- decompose(p_home, p_away, s_home, s_away)
  observed <- p_home * s_home - p_away * s_away
  expect_equal(d$access + d$performance, observed, tolerance = 1e-9)
})

test_that("equal participation puts the whole gain on performance", {
  d <- decompose(p_home = 0.9, p_away = 0.9, s_home = 20, s_away = 12)
  expect_equal(d$access, 0)
  expect_equal(d$performance, 0.9 * 8)
})

test_that("equal shares put the whole gain on access", {
  d <- decompose(p_home = 1.0, p_away = 0.6, s_home = 15, s_away = 15)
  expect_equal(d$performance, 0)
  expect_equal(d$access, 0.4 * 15)
})

test_that("the observed split is roughly 60/40 access to performance", {
  # Guards the headline: if a data change flips this, the write-up is stale.
  d <- decompose(0.957, 0.655, 18.70, 15.32)
  frac_access <- d$access / (d$access + d$performance)
  expect_gt(frac_access, 0.5)
  expect_lt(frac_access, 0.7)
})

test_that("a host that never turns up away still shows a finite gain", {
  # p_away = 0 is the extreme access case and must not divide by anything.
  d <- decompose(p_home = 1, p_away = 0, s_home = 20, s_away = 0)
  expect_equal(d$access, 0)
  expect_equal(d$performance, 20)
  expect_true(is.finite(d$access + d$performance))
})
