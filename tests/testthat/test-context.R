plant_context <- function(effects = c(a = 0.008, b = 0, c = -0.008),
                          n_ath = 50, n_each = 30, sigma = 0.006, seed = 11) {
  set.seed(seed)
  ability <- stats::rnorm(n_ath, to_perf(10, -1L), 0.02)
  lv <- names(effects)
  data.table::rbindlist(lapply(seq_len(n_ath), function(i) {
    x <- sample(lv, n_each, replace = TRUE)
    data.table::data.table(
      athlete_id = as.character(i), event_id = "AT-100Metres-M",
      surface = x, date = Sys.Date() - seq_len(n_each),
      perf = ability[i] + unname(effects[x]) + stats::rnorm(n_each, 0, sigma))
  }))
}

test_that("planted level effects are recovered", {
  d <- plant_context()
  f <- fit_context_effect(d, "surface")
  expect_equal(nrow(f), 3)
  spread <- max(f$effect) - min(f$effect)
  expect_lt(abs(spread - 0.016), 0.004)
})

test_that("levels come out in the right order", {
  f <- fit_context_effect(plant_context(), "surface")
  data.table::setorderv(f, "effect", -1)
  expect_equal(f$level, c("a", "b", "c"))
})

test_that("effects are centred and add no intercept", {
  f <- fit_context_effect(plant_context(), "surface")
  expect_lt(abs(stats::weighted.mean(f$effect, f$n)), 1e-8)
})

test_that("ability does not leak into the effect", {
  # Strong athletes appear disproportionately on surface "a".
  set.seed(4)
  n <- 60
  ability <- stats::rnorm(n, to_perf(10, -1L), 0.03)
  strong <- order(ability, decreasing = TRUE)[1:20]
  d <- data.table::rbindlist(lapply(seq_len(n), function(i) {
    p <- if (i %in% strong) 0.85 else 0.15
    x <- sample(c("a", "b"), 40, replace = TRUE, prob = c(p, 1 - p))
    data.table::data.table(
      athlete_id = as.character(i), event_id = "AT-100Metres-M", surface = x,
      date = Sys.Date() - seq_len(40),
      perf = ability[i] + ifelse(x == "a", 0.005, -0.005) + stats::rnorm(40, 0, 0.006))
  }))
  f <- fit_context_effect(d, "surface")
  spread <- f[level == "a"]$effect - f[level == "b"]$effect
  expect_lt(abs(spread - 0.010), 0.004)
})

test_that("pooling across events works", {
  d <- plant_context()
  d2 <- data.table::copy(d)[, event_id := "AT-200Metres-M"]
  f <- fit_context_effect(rbind(d, d2), "surface", by_event = FALSE)
  expect_true(all(is.na(f$event_id)))
  expect_equal(nrow(f), 3)
})

test_that("rare levels are excluded", {
  d <- plant_context()
  d <- rbind(d, data.table::data.table(
    athlete_id = "1", event_id = "AT-100Metres-M", surface = "rare",
    date = Sys.Date(), perf = to_perf(9, -1L)))
  expect_false("rare" %in% fit_context_effect(d, "surface")$level)
})

test_that("adjustment removes the signal", {
  d <- plant_context()
  adj <- adjust_context(d, fit_context_effect(d, "surface"), "surface")
  adj[, dev := perf - mean(perf), by = athlete_id]
  expect_lt(max(abs(adj[, .(m = mean(dev)), by = surface]$m)), 0.003)
})

test_that("unknown levels pass through untouched", {
  d <- plant_context()
  f <- fit_context_effect(d, "surface")
  d2 <- data.table::copy(d)[1:5, surface := "unseen"]
  adj <- adjust_context(d2, f, "surface")
  expect_equal(adj$perf[1:5], d2$perf[1:5])
})

test_that("a missing covariate returns empty rather than erroring", {
  expect_equal(nrow(fit_context_effect(plant_context(), "nonexistent")), 0)
})

# Season phase. Validated out of sample before adoption (0.66% relative RMSE on
# 2020+ top-tier finals from pre-2020 offsets) -- the check that per-family tier
# offsets failed and venue effects nearly failed.

test_that("a planted seasonal curve is recovered within athlete", {
  set.seed(9)
  # Every athlete peaks in July and is 2% down in January, on top of a large
  # ability spread that a cross-sectional fit would absorb.
  rows <- data.table::rbindlist(lapply(1:200, function(i) {
    ab <- stats::rnorm(1, 2.3, 0.05)
    m <- rep(3:11, 2)
    data.table::data.table(
      athlete_id = as.character(i), event_id = "AT-100Metres-M",
      tier = "OW", round = "F", indoor = FALSE, venue_country = "GBR",
      date = as.Date(sprintf("2021-%02d-15", m)),
      perf = ab - 0.02 * ((m - 7) / 4)^2 + stats::rnorm(length(m), 0, 0.003))
  }))
  s <- fit_season_effect(rows, min_n = 50L)
  sp <- s[family == "sprint" & hemi == "N"]
  expect_gt(nrow(sp), 4)
  expect_equal(sp$month[which.max(sp$offset)], 7L)
  # March and November sit either side of the peak and must both be below it.
  expect_lt(sp$offset[sp$month == 3L], max(sp$offset))
  expect_lt(sp$offset[sp$month == 11L], max(sp$offset))
})

test_that("northern winter outdoor marks are excluded, not fitted as form", {
  # Jan/Feb northern competition is 95-98% indoor and the outdoor remnant is the
  # European winter throwing circuit -- cold, not early-season form. Planting a
  # large January outdoor penalty must NOT produce a January offset.
  set.seed(10)
  base <- data.table::rbindlist(lapply(1:150, function(i) {
    data.table::data.table(
      athlete_id = as.character(i), event_id = "AT-100Metres-M", tier = "OW",
      round = "F", indoor = FALSE, venue_country = "GBR",
      date = as.Date(sprintf("2021-%02d-15", 5:10)),
      perf = 2.3 + stats::rnorm(6, 0, 0.003))
  }))
  winter <- data.table::copy(base)[, `:=`(date = as.Date("2021-01-15"),
                                          perf = perf - 0.05)]
  s <- fit_season_effect(rbind(base, winter), min_n = 50L)
  expect_false(1L %in% s[family == "sprint" & hemi == "N"]$month)
})

test_that("offsets are centred so they shift phase, not level", {
  set.seed(11)
  rows <- data.table::rbindlist(lapply(1:200, function(i) {
    data.table::data.table(
      athlete_id = as.character(i), event_id = "AT-100Metres-M", tier = "OW",
      round = "F", indoor = FALSE, venue_country = "GBR",
      date = as.Date(sprintf("2021-%02d-15", 4:10)),
      perf = 2.3 + stats::rnorm(7, 0, 0.01))
  }))
  s <- fit_season_effect(rows, min_n = 50L)
  expect_equal(stats::weighted.mean(s$offset, s$n), 0, tolerance = 1e-9)
})

test_that("missing venue_country degrades to a single calendar", {
  set.seed(12)
  rows <- data.table::rbindlist(lapply(1:150, function(i) {
    data.table::data.table(
      athlete_id = as.character(i), event_id = "AT-100Metres-M", tier = "OW",
      round = "F", indoor = FALSE,
      date = as.Date(sprintf("2021-%02d-15", 4:10)),
      perf = 2.3 + stats::rnorm(7, 0, 0.01))
  }))
  s <- fit_season_effect(rows, min_n = 50L)
  expect_true(all(s$hemi == "N"))
})
