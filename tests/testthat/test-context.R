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

# Championship effect. Round and tier offsets reference "final" and "top", so a
# top-tier final gets a zero adjustment BY CONSTRUCTION and "global championship
# vs Diamond League final" is currently inexpressible. It is not zero, and the
# sign flips by family.

champ_history <- function(n = 200, gap = 0.02, seed = 5) {
  set.seed(seed)
  data.table::rbindlist(lapply(seq_len(n), function(i) {
    ab <- stats::rnorm(1, 2.3, 0.05)
    data.table::rbindlist(list(
      data.table::data.table(athlete_id = as.character(i), event_id = "AT-100Metres-M",
                             tier = "GL", round = "F", date = Sys.Date() - 1:6,
                             perf = ab + stats::rnorm(6, 0, 0.003)),
      data.table::data.table(athlete_id = as.character(i), event_id = "AT-100Metres-M",
                             tier = "OW", round = "F", date = Sys.Date() - 7:12,
                             perf = ab + gap + stats::rnorm(6, 0, 0.003))))
  }))
}

test_that("a planted championship gap is recovered", {
  ce <- fit_championship_effect(champ_history(gap = 0.02), min_n = 50L)
  expect_equal(nrow(ce[family == "sprint"]), 1L)
  expect_equal(ce[family == "sprint"]$offset, 0.02, tolerance = 0.004)
})

test_that("a negative gap is recovered too, since the sign flips by family", {
  ce <- fit_championship_effect(champ_history(gap = -0.015), min_n = 50L)
  expect_lt(ce[family == "sprint"]$offset, 0)
})

test_that("round and tier are held constant, so heats cannot leak in", {
  # A first version compared championships against ALL other marks and absorbed
  # the round effect, producing offsets like throw +5.33%. Adding slow heats must
  # not move the estimate.
  h <- champ_history(gap = 0.02)
  heats <- data.table::copy(h[tier == "GL"])[, `:=`(round = "H", perf = perf - 0.05)]
  with_heats <- fit_championship_effect(rbind(h, heats), min_n = 50L)
  expect_equal(with_heats[family == "sprint"]$offset,
               fit_championship_effect(h, min_n = 50L)[family == "sprint"]$offset,
               tolerance = 1e-9)
})

test_that("only athletes seen in both contexts identify the gap", {
  # Championship-only athletes contribute their ability, not a comparison, so
  # adding a very fast one must not inflate the offset.
  h <- champ_history(gap = 0.02)
  ringer <- data.table::data.table(
    athlete_id = "zz", event_id = "AT-100Metres-M", tier = "OW", round = "F",
    date = Sys.Date() - 1:6, perf = 2.9)
  expect_equal(fit_championship_effect(rbind(h, ringer), min_n = 50L)$offset,
               fit_championship_effect(h, min_n = 50L)$offset, tolerance = 1e-9)
})

test_that("the round trip is an identity for an all-championship record", {
  # estimate_ability() removes the offset from championship history and
  # project_championship() adds it back. An athlete whose record is entirely
  # championships must land exactly where they started.
  ce <- data.table::data.table(family = "sprint", offset = 0.02, n = 999L)
  only_champ <- data.table::data.table(
    athlete_id = rep(c("a", "b"), each = 6), event_id = "AT-100Metres-M",
    tier = "OW", round = "F", date = Sys.Date() - rep(1:6, 2),
    perf = to_perf(10, -1L) + stats::rnorm(12, 0, 0.002))
  plain <- estimate_ability(only_champ, as_of = Sys.Date(), adjust_context = FALSE)
  round_trip <- project_championship(
    estimate_ability(only_champ, as_of = Sys.Date(), adjust_context = TRUE,
                     calibration = list(championship = ce)),
    list(championship = ce))
  m <- merge(plain[, .(athlete_id, a0 = ability)],
             round_trip[, .(athlete_id, a1 = ability)], by = "athlete_id")
  expect_equal(m$a1, m$a0, tolerance = 1e-8)
})

test_that("an athlete with no championship record is moved by the full offset", {
  ce <- data.table::data.table(family = "sprint", offset = 0.02, n = 999L)
  no_champ <- data.table::data.table(
    athlete_id = rep(c("a", "b"), each = 6), event_id = "AT-100Metres-M",
    tier = "GL", round = "F", date = Sys.Date() - rep(1:6, 2),
    perf = to_perf(10, -1L) + stats::rnorm(12, 0, 0.002))
  base <- estimate_ability(no_champ, as_of = Sys.Date(), adjust_context = TRUE,
                           calibration = list(championship = ce))
  moved <- project_championship(base, list(championship = ce))
  expect_equal(moved$ability - base$ability, rep(0.02, nrow(base)), tolerance = 1e-9)
})

test_that("no championship table is a no-op, not an error", {
  ab <- data.table::data.table(athlete_id = "a", event_id = "AT-100Metres-M",
                               ability = 2.3)
  expect_equal(project_championship(ab, NULL)$ability, 2.3)
  expect_equal(project_championship(ab, list(championship = NULL))$ability, 2.3)
})

test_that("project_tier puts ability back onto the tier being predicted", {
  # estimate_ability(adjust_context = TRUE) SUBTRACTS the tier offset to reach a
  # top-tier footing. Nothing added it back, so every race was predicted as
  # though it were a top-tier final. Measured cost on 3,696 backtest finals:
  # lower-tier races -1.46% and pro -0.41% against championship races at ~0.
  cal <- structure(list(tier = data.table::data.table(
    tier_class = c("top", "high", "mid", "low"),
    offset = c(0, -0.00695, -0.00982, -0.01689),
    precision = c(1.208, 1.053, 1.091, 0.971))), class = "citius_calibration")
  ab <- data.table::data.table(athlete_id = c("a", "b"),
                               event_id = "AT-5000Metres-M",
                               ability = c(-6.0, -6.1))

  # Top tier is the reference: nothing moves.
  expect_equal(project_tier(ab, "OW", cal)$ability, ab$ability)

  # A low-tier race is genuinely slower, so the prediction must come down --
  # by HALF the measured offset, because the offsets are fitted on all history
  # while the races predicted are a faster, selected subset. Applying them in
  # full overshoots low tier from -0.98% bias to +0.71%.
  low <- project_tier(ab, "F", cal)
  expect_true(all(low$ability < ab$ability))
  expect_equal(unique(round(low$ability - ab$ability, 6)), -0.008445)
  expect_equal(unique(round(project_tier(ab, "F", cal, shrink = 1)$ability -
                             ab$ability, 5)), -0.01689)

  # Without measured offsets there is nothing to add back, and guessing would be
  # worse than doing nothing.
  expect_equal(project_tier(ab, "F", NULL)$ability, ab$ability)

  # It is the exact inverse of what estimate_ability() removed.
  expect_equal(project_tier(project_tier(ab, "F", cal, shrink = 1), "F", cal,
                            shrink = 1)$ability,
               ab$ability + 2 * -0.01689)
})

test_that("fit_coasting_trait estimates shrunk coasting deviations for heats", {
  dt <- data.table::data.table(
    athlete_id = rep(c("a", "b"), each = 4),
    event_id = "AT-400Metres-M",
    round = c("Heat 1", "Heat 2", "Final", "Final", "Heat 1", "Heat 2", "Final", "Final"),
    tier = "OW",
    perf = c(2.0, 2.0, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1) # athlete 'a' runs slower in heats (coasted)
  )
  ct <- fit_coasting_trait(dt, min_heats = 2, shrink_k = 2)
  expect_equal(nrow(ct), 2L)
  expect_true("coasting_trait" %in% names(ct))
  a_trait <- ct[athlete_id == "a"]$coasting_trait
  b_trait <- ct[athlete_id == "b"]$coasting_trait

  # Shrinkage targets the POPULATION heat deviation, not zero (fixed 2026-08-13).
  # Here that population value is mean(-0.1, -0.1, 0, 0) = -0.05, so athlete 'b',
  # who shows no coasting across only two heats, is pulled halfway to it:
  # -0.05 + (2/(2+2)) * (0 - -0.05) = -0.025.
  #
  # This test previously asserted `b_trait == 0`, which encoded the old
  # shrink-to-zero form -- a prior belief that nobody coasts, which is false and
  # measurably so. Shrinking to zero drove the fitted trait mean BELOW the pooled
  # heat offset on the real corpus (-0.0025 against -0.00649), making every
  # athlete look like less of a coaster than the average athlete.
  expect_lt(a_trait, 0)
  expect_equal(b_trait, -0.025)
  # The one that matters: real evidence of coasting must land further from the
  # population than an athlete with none.
  expect_lt(a_trait, b_trait)
})

test_that("fit_coasting_trait references FINALS, not the athlete's overall mean", {
  # Two athletes with identical heat-vs-final gaps (-0.1) but different round
  # MIXES. Referenced to the athlete's overall mean the two disagree, because a
  # heat-heavy record drags its own reference down. Referenced to finals they
  # agree, which is the property that lets the trait compose with the
  # final-referenced round offset in estimate_ability(). (fixed 2026-08-13)
  mk <- function(id, n_heat, n_final) {
    data.table::data.table(
      athlete_id = id, event_id = "AT-400Metres-M", tier = "OW",
      round = c(rep("Heat 1", n_heat), rep("Final", n_final)),
      perf = c(rep(2.0, n_heat), rep(2.1, n_final)))
  }
  dt <- rbind(mk("heavy", 6, 2), mk("light", 2, 6))
  ct <- fit_coasting_trait(dt, min_heats = 2, shrink_k = 0)  # k = 0: no shrinkage
  expect_equal(ct[athlete_id == "heavy"]$coasting_trait,
               ct[athlete_id == "light"]$coasting_trait)
  expect_equal(ct[athlete_id == "heavy"]$coasting_trait, -0.1)
})

test_that("an athlete with heats but no final gets no trait rather than a made-up one", {
  dt <- data.table::data.table(
    athlete_id = c(rep("nofinal", 3), rep("hasfinal", 4)),
    event_id = "AT-400Metres-M", tier = "OW",
    round = c("Heat 1", "Heat 2", "Heat 3", "Heat 1", "Heat 2", "Final", "Final"),
    perf = c(2.0, 2.0, 2.0, 2.0, 2.0, 2.1, 2.1))
  ct <- fit_coasting_trait(dt, min_heats = 2, shrink_k = 2)
  # "How much easier than their final" is undefined without a final. Defaulting
  # such an athlete to 0 would put a fabricated trait on exactly the
  # thin-evidence athletes shrinkage exists to protect.
  expect_false("nofinal" %in% ct$athlete_id)
  expect_true("hasfinal" %in% ct$athlete_id)
})

