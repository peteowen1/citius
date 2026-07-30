synthetic_history <- function(n_athletes = 12, n_each = 20, sigma = 0.01,
                              event_id = "AT-100Metres-M", seed = 99) {
  set.seed(seed)
  true_ability <- to_perf(seq(9.80, 10.30, length.out = n_athletes), -1L)
  data.table::rbindlist(lapply(seq_len(n_athletes), function(i) {
    data.table::data.table(
      athlete_id = as.character(i),
      event_id = event_id,
      date = Sys.Date() - sample(1:900, n_each, replace = TRUE),
      perf = true_ability[i] + stats::rnorm(n_each, 0, sigma),
      tier = "OW",
      round = "F"
    )
  }))
}

test_that("ability recovers the true ordering", {
  h <- synthetic_history()
  ab <- estimate_ability(h, adjust_context = FALSE)
  data.table::setorder(ab, -ability)
  expect_equal(ab$athlete_id[1], "1")
  expect_equal(ab$athlete_id[nrow(ab)], "12")
})

test_that("sparse histories are shrunk harder than deep ones", {
  h <- rbind(
    synthetic_history(n_athletes = 10, n_each = 40),
    data.table::data.table(athlete_id = "sparse", event_id = "AT-100Metres-M",
                           date = Sys.Date() - 30, perf = to_perf(9.50, -1L),
                           tier = "OW", round = "F")
  )
  ab <- estimate_ability(h, adjust_context = FALSE)
  sparse <- ab[ab$athlete_id == "sparse", ]
  deep <- ab[ab$athlete_id == "1", ]
  expect_gt(sparse$shrinkage, deep$shrinkage)
  # A single blazing result must not be taken at face value
  expect_lt(perf_to_mark(sparse$ability, -1L), 9.50 + 0.6)
  expect_gt(perf_to_mark(sparse$ability, -1L), 9.50)
})

test_that("recent results dominate stale ones", {
  h <- data.table::data.table(
    athlete_id = "x", event_id = "AT-100Metres-M",
    date = c(Sys.Date() - 30, Sys.Date() - 2000),
    perf = to_perf(c(9.90, 10.50), -1L),
    tier = "OW", round = "F"
  )
  ab <- estimate_ability(h, adjust_context = FALSE, half_life = 365)
  expect_lt(perf_to_mark(ab$ability, -1L), 10.2)
})

test_that("result_weight decays by exactly a half per half-life", {
  today <- as.Date("2026-07-27")
  w <- result_weight(c(today, today - 540), tier = "OW", round = "F",
                     as_of = today, half_life = 540)
  expect_equal(w[2] / w[1], 0.5, tolerance = 1e-8)
})

test_that("context weights are flat until a calibration measures them", {
  # A guessed weighting is not more honest than none. Without measured
  # precisions every context counts the same and only recency applies.
  today <- as.Date("2026-07-27")
  expect_equal(result_weight(today, "OW", "F", as_of = today),
               result_weight(today, "F", "H3", as_of = today))
})

test_that("a calibration downweights the noisier context", {
  # Plant heats as far noisier than finals; precision weighting must follow.
  set.seed(41)
  n <- 40
  races <- rbind(
    data.table::data.table(race_key = paste0("F", 1:n), round = "F", tier = "OW"),
    data.table::data.table(race_key = paste0("H", 1:n), round = "H1", tier = "OW")
  )
  d <- races[rep(seq_len(nrow(races)), each = 6)]
  d[, athlete_id := as.character(rep_len(1:6, .N))]
  d[, event_id := "AT-100Metres-M"]
  d[, date := Sys.Date() - 1]
  noise <- ifelse(d$round == "F", 0.002, 0.020)
  d[, perf := to_perf(9.9, -1L) + stats::rnorm(.N, 0, noise)]

  cal <- calibrate(d, min_races = 4L)
  today <- Sys.Date()
  expect_lt(result_weight(today, "OW", "H1", as_of = today, calibration = cal),
            result_weight(today, "OW", "F", as_of = today, calibration = cal))
})

test_that("tactical trimming removes slow championship races, not fast ones", {
  # A tactical event where most races are honest but a few are sit-and-kick
  set.seed(11)
  honest <- to_perf(stats::rnorm(20, 215, 2), -1L)
  tactical <- to_perf(c(240, 245, 250), -1L)
  h <- data.table::data.table(
    athlete_id = "x", event_id = "AT-1500Metres-M",
    date = Sys.Date() - 1:23, perf = c(honest, tactical),
    tier = "OW", round = "F"
  )
  trimmed <- estimate_ability(h, trim_tactical = 0.25, adjust_context = FALSE)
  untrimmed <- estimate_ability(h, trim_tactical = 0, adjust_context = FALSE)

  expect_gt(trimmed$ability, untrimmed$ability)   # trimmed estimate is faster
  expect_lt(trimmed$sigma, untrimmed$sigma)       # and less noisy
})

test_that("trimming does not touch non-tactical events", {
  h <- synthetic_history(event_id = "AT-100Metres-M")
  a <- estimate_ability(h, trim_tactical = 0.25, adjust_context = FALSE)
  b <- estimate_ability(h, trim_tactical = 0, adjust_context = FALSE)
  data.table::setorder(a, athlete_id); data.table::setorder(b, athlete_id)
  expect_equal(a$ability, b$ability)
})

test_that("round labels classify by the most specific pattern, not the last one", {
  # Regression: .round_class() is a sequence of overwrites, so the last match
  # wins. Round labels NEST -- the feed's real semi-final label is
  # "Semifinal - Heat", containing HEAT, SEMI and FINAL. With "final" applied
  # last, every semi-final classified as a FINAL: 14,764 results, 4.79% of the
  # harvest, pooled into the reference context all other offsets are measured
  # against. Nothing errored; the semi bucket was simply empty.
  expect_equal(citius:::.round_class("Semifinal - Heat"), "semi")
  expect_equal(citius:::.round_class(c("Semi Final", "Semi-Final", "Semifinal")),
               rep("semi", 3))
  expect_equal(citius:::.round_class("Quarter-Final"), "quarter")
  # The plain cases must not regress while fixing the nested ones.
  expect_equal(citius:::.round_class(c("Final", "Round 1 - Heat", "SF", "QF")),
               c("final", "heat", "semi", "quarter"))
  expect_equal(citius:::.round_class(c("Combined - Group", NA)), c("other", "other"))
})

test_that("context effects recover a planted round offset", {
  set.seed(21)
  base <- to_perf(9.90, -1L)
  h <- data.table::data.table(
    athlete_id = rep(as.character(1:10), each = 10),
    event_id = "AT-100Metres-M",
    date = Sys.Date() - 1:100,
    round = rep(c("F", "H1"), 50),
    tier = "OW"
  )
  # Heats are planted as 1% slower
  h[, perf := base + ifelse(round == "H1", -0.01, 0) + stats::rnorm(.N, 0, 0.002)]
  ctx <- estimate_context_effects(h)
  expect_lt(abs(unname(ctx$round[["heat"]]) - (-0.01)), 0.003)
  expect_equal(unname(ctx$round[["final"]]), 0)
})

test_that("context adjustment raises ability toward final-equivalent", {
  set.seed(31)
  base <- to_perf(9.90, -1L)
  h <- data.table::data.table(
    athlete_id = rep(as.character(1:10), each = 10),
    event_id = "AT-100Metres-M",
    date = Sys.Date() - 1:100,
    round = rep(c("F", "H1"), 50),
    tier = "OW"
  )
  h[, perf := base + ifelse(round == "H1", -0.01, 0) + stats::rnorm(.N, 0, 0.002)]
  with_adj <- estimate_ability(h, adjust_context = TRUE)
  without <- estimate_ability(h, adjust_context = FALSE)
  expect_gt(mean(with_adj$ability), mean(without$ability))
})

test_that("empty input returns an empty frame with the right columns", {
  ab <- estimate_ability(synthetic_history()[0])
  expect_equal(nrow(ab), 0)
  expect_true(all(c("athlete_id", "event_id", "ability", "sigma") %in% names(ab)))
})

test_that("fit_half_life recovers a planted decay rate", {
  # Ability drifts over time; a short half-life should win when form changes
  # fast, a long one when it is stable.
  set.seed(77)
  drifting <- data.table::rbindlist(lapply(1:60, function(i) {
    d <- Sys.Date() - seq(0, 1400, by = 40)
    data.table::data.table(
      athlete_id = as.character(i), event_id = "AT-100Metres-M",
      date = d, tier = "OW", round = "F",
      perf = to_perf(10, -1L) + cumsum(stats::rnorm(length(d), 0, 0.004)))
  }))
  stable <- data.table::rbindlist(lapply(1:60, function(i) {
    d <- Sys.Date() - seq(0, 1400, by = 40)
    data.table::data.table(
      athlete_id = as.character(i), event_id = "AT-100Metres-M",
      date = d, tier = "OW", round = "F",
      perf = to_perf(10, -1L) + stats::rnorm(length(d), 0, 0.004))
  }))

  hl_drift <- fit_half_life(drifting)
  hl_stable <- fit_half_life(stable)
  expect_lt(hl_drift$half_life, hl_stable$half_life)
})

test_that("a half-life on the grid edge is flagged and replaced", {
  set.seed(5)
  d <- data.table::rbindlist(lapply(1:40, function(i) {
    dd <- Sys.Date() - seq(0, 800, by = 40)
    data.table::data.table(
      athlete_id = as.character(i), event_id = "AT-100Metres-M",
      date = dd, tier = "OW", round = "F",
      perf = to_perf(10, -1L) + stats::rnorm(length(dd), 0, 0.005))
  }))
  # Grid so narrow the optimum must sit on an edge
  hl <- fit_half_life(d, candidates = c(3000, 3650))
  expect_false(all(hl$identified))
})

test_that("estimate_ability accepts a fitted half-life table", {
  h <- synthetic_history(n_athletes = 20, n_each = 12)
  tbl <- data.table::data.table(family = "sprint", half_life = 120,
                                mae = 0.01, n = 100L, identified = TRUE)
  ab_tbl <- estimate_ability(h, half_life = tbl, adjust_context = FALSE)
  ab_num <- estimate_ability(h, half_life = 120, adjust_context = FALSE)
  data.table::setorder(ab_tbl, athlete_id); data.table::setorder(ab_num, athlete_id)
  expect_equal(ab_tbl$ability, ab_num$ability)
})

test_that("stale athletes shrink to the event mean without a cutoff", {
  # Shrinking on total weight rather than n_eff is what makes this work: many
  # old results carry little evidence, so the estimate regresses on its own.
  today <- as.Date("2026-07-30")
  recent <- data.table::data.table(
    athlete_id = "recent", event_id = "AT-100Metres-M",
    date = today - seq(10, 300, by = 20), tier = "OW", round = "F",
    perf = to_perf(10.10, -1L))
  stale <- data.table::data.table(
    athlete_id = "stale", event_id = "AT-100Metres-M",
    date = today - seq(4000, 4600, by = 40), tier = "OW", round = "F",
    perf = to_perf(9.85, -1L))          # much faster, but ancient
  others <- data.table::rbindlist(lapply(1:20, function(i)
    data.table::data.table(athlete_id = paste0("o", i), event_id = "AT-100Metres-M",
      date = today - seq(10, 300, by = 20), tier = "OW", round = "F",
      perf = to_perf(10.20, -1L) + stats::rnorm(15, 0, 0.005))))

  ab <- estimate_ability(rbind(recent, stale, others), as_of = today,
                         half_life = 180, adjust_context = FALSE)
  expect_gt(ab[athlete_id == "stale"]$shrinkage,
            ab[athlete_id == "recent"]$shrinkage)
  # The stale athlete must not out-rank the active one on ancient form
  expect_lt(ab[athlete_id == "stale"]$ability, ab[athlete_id == "recent"]$ability)
})

test_that("wind is stripped from ability, and the local name does not shadow `w`", {
  # Plant a known wind coefficient and check estimate_ability() recovers ability
  # despite it. The trap this guards: `dt` carries a column `w` (the recency and
  # precision weight), so a local variable named `w` is silently shadowed inside
  # `dt[, ...]` and data.table uses the COLUMN. That subtracted beta * weight
  # instead of beta * wind -- a constant 0.4% level shift with the spread
  # untouched, invisible to any ranking test.
  set.seed(4)
  n_ath <- 30; beta <- 0.0045
  ability <- stats::rnorm(n_ath, to_perf(10, -1L), 0.02)
  rows <- data.table::rbindlist(lapply(seq_len(200), function(r) {
    who <- sample(n_ath, 8); wind <- stats::rnorm(1, 0, 1.3)
    data.table::data.table(
      race_key = paste0("r", r), athlete_id = as.character(who),
      event_id = "AT-100Metres-M", date = Sys.Date() - r,
      round = "F", tier = "OW", wind = wind,
      perf = ability[who] + beta * wind + stats::rnorm(8, 0, 0.008))
  }))
  cal <- calibrate(rows, min_races = 5L)
  expect_equal(cal$wind$beta[1], beta, tolerance = 0.1)

  truth <- data.table::data.table(athlete_id = as.character(seq_len(n_ath)),
                                  true = ability)
  err <- function(cl) {
    m <- merge(estimate_ability(rows, as_of = Sys.Date(), calibration = cl),
               truth, by = "athlete_id")
    stats::sd(m$ability - m$true)
  }
  cal_off <- cal; cal_off$wind <- NULL
  # Removing a real covariate must SHARPEN the estimate, not merely shift it.
  expect_lt(err(cal), err(cal_off) * 0.9)

  # A shadowed `w` shows up as a level shift with the spread unchanged, so test
  # the level explicitly too.
  m <- merge(estimate_ability(rows, as_of = Sys.Date(), calibration = cal),
             truth, by = "athlete_id")
  expect_lt(abs(mean(m$ability - m$true)), 0.001)
})

test_that("apply_momentum accepts every input form and respects shrinkage", {
  ab <- data.table::data.table(
    athlete_id = c("a", "b", "c"), event_id = "AT-ShotPut-M",
    ability = c(1, 1, 1), shrinkage = c(0, 0, 0.5))
  cal <- list(momentum = data.table::data.table(family = "throw", beta = 0.005))

  r1 <- apply_momentum(ab, c(a = 4, b = 0, c = 4), cal)
  r2 <- apply_momentum(ab, data.table::data.table(
    athlete_id = c("a", "b", "c"), momentum = c(4, 0, 4)), cal)
  r3 <- apply_momentum(ab, data.table::data.table(
    athlete_id = c("a", "b", "c"), momentum_now = c(4, 0, 4)), cal)
  expect_equal(r1$ability, r2$ability)
  expect_equal(r1$ability, r3$ability)

  # Zero momentum must not move the estimate.
  expect_equal(r1$ability[2], 1)
  # A half-shrunk athlete gets half the shift: applying a form adjustment to a
  # number that is mostly the event mean would adjust the population.
  expect_equal((r1$ability[3] - 1) * 2, r1$ability[1] - 1)
  # No momentum table on the calibration is a no-op, not an error.
  expect_equal(apply_momentum(ab, c(a = 4), list())$ability, ab$ability)
})

# Per-family context offsets, and the shrinkage that decides how far to trust
# them. Shipping them raw was measurably WORSE than omitting them (arm `cstack`,
# 2026-07-30): road and walk improved ~80% while throw overcorrected a +0.60%
# bias into +2.05%.

# Athletes with marks in a reference context and one other, where the gap
# between the two is a planted per-family constant.
two_context <- function(n_ath = 400, gap_a = 0.03, gap_b = 0.01, sigma = 0.002,
                        seed = 7) {
  set.seed(seed)
  ev <- c(a = "AT-100Metres-M", b = "AT-ShotPut-M")
  data.table::rbindlist(lapply(seq_len(n_ath), function(i) {
    fam <- if (i %% 2L == 0L) "a" else "b"
    gap <- if (fam == "a") gap_a else gap_b
    ability <- stats::rnorm(1, 2.3, 0.05)
    data.table::rbindlist(list(
      data.table::data.table(
        athlete_id = as.character(i), event_id = ev[[fam]], tier = "OW",
        round = "F", date = as.Date("2020-01-01") + 1:6,
        perf = ability + stats::rnorm(6, 0, sigma)),
      data.table::data.table(
        athlete_id = as.character(i), event_id = ev[[fam]], tier = "F",
        round = "F", date = as.Date("2021-01-01") + 1:6,
        perf = ability - gap + stats::rnorm(6, 0, sigma))))
  }))
}

test_that("per-family offsets carry raw and shrink_k so the adjustment is auditable", {
  ctx <- estimate_context_effects(two_context(), min_cell = 50L, per_family = TRUE)
  skip_if(is.null(ctx$tier_family) || !nrow(ctx$tier_family))
  expect_true(all(c("raw", "shrink_k", "offset", "n") %in% names(ctx$tier_family)))
})

test_that("a shrunk offset lies between its raw value and the pooled one", {
  ctx <- estimate_context_effects(two_context(), min_cell = 50L, per_family = TRUE)
  tf <- data.table::as.data.table(ctx$tier_family)
  skip_if(!nrow(tf) || !all(is.finite(tf$shrink_k)) || all(tf$shrink_k == 0))
  pooled <- ctx$tier[match(tf$tier_class, names(ctx$tier))]
  # offset must never overshoot raw, which is what applying it unshrunk did.
  expect_true(all(abs(tf$offset - pooled) <= abs(tf$raw - pooled) + 1e-9))
})

test_that("shrink = FALSE leaves the offsets raw", {
  on_ <- estimate_context_effects(two_context(), min_cell = 50L, shrink = TRUE, per_family = TRUE)
  off <- estimate_context_effects(two_context(), min_cell = 50L, shrink = FALSE, per_family = TRUE)
  skip_if(is.null(off$tier_family) || !nrow(off$tier_family))
  expect_false("shrink_k" %in% names(off$tier_family))
  expect_true("shrink_k" %in% names(on_$tier_family))
})

test_that("the shrinkage fitter falls back to pooled when it cannot validate", {
  # Too little data to hold out a context: the safe fallback is Inf (pooled
  # only), never an unvalidated per-family offset.
  tiny <- two_context(n_ath = 5)
  tiny[, `:=`(family = "a", tier_class = "low", resid = 0.01)]
  expect_equal(
    citius:::.fit_context_shrink(tiny, "tier_class",
                                 data.table::data.table(tier_class = "low", eff = 0)),
    Inf)
})

# sigma is fitted across the pooled history but the forecast targets a top-tier
# final, which is a narrower slice of conditions for field events and a wider one
# for road. Measured ratios track the model's dispersion error closely (cor 0.80
# across families), so the correction is applied to the sigma estimate_ability()
# RETURNS -- the column simulate_event() reads. An earlier attempt that widened
# calibration$events$sigma_within instead was bit-for-bit inert.

test_that("fit_sigma_context recovers a planted championship/pooled ratio", {
  set.seed(3)
  n <- 300
  rows <- data.table::rbindlist(lapply(seq_len(n), function(i) {
    ab <- stats::rnorm(1, 2.3, 0.05)
    data.table::rbindlist(list(
      # Everyday racing: wide spread.
      data.table::data.table(athlete_id = as.character(i), event_id = "AT-100Metres-M",
                             tier = "C", round = "H", date = Sys.Date() - 1:10,
                             perf = ab + stats::rnorm(10, 0, 0.02)),
      # Championship finals: half the spread.
      data.table::data.table(athlete_id = as.character(i), event_id = "AT-100Metres-M",
                             tier = "OW", round = "F", date = Sys.Date() - 11:20,
                             perf = ab + stats::rnorm(10, 0, 0.01))))
  }))
  sc <- fit_sigma_context(rows, min_n = 100L)
  sprint <- sc[family == "sprint"]
  expect_equal(nrow(sprint), 1L)
  # Pooled spread mixes the two regimes, so it is sqrt(mean(0.02^2, 0.01^2)) and
  # the recoverable ratio is 0.01 over that, ~0.63 -- NOT the 0.5 ratio of the
  # two sds. Asserting 0.5 here would be asserting the wrong quantity.
  expect_equal(sprint$ratio, 0.01 / sqrt((0.02^2 + 0.01^2) / 2), tolerance = 0.08)
})

test_that("a family with too few championship marks is left alone", {
  set.seed(4)
  rows <- data.table::data.table(
    athlete_id = rep(as.character(1:50), each = 6), event_id = "AT-100Metres-M",
    tier = "C", round = "H", date = Sys.Date() - 1:6,
    perf = 2.3 + stats::rnorm(300, 0, 0.02))
  sc <- fit_sigma_context(rows, min_n = 500L)
  expect_true(all(sc$ratio == 1))     # ratio of 1 leaves sigma untouched
})

test_that("sigma_context reaches the sigma estimate_ability returns", {
  h <- data.table::data.table(
    athlete_id = rep(c("a", "b"), each = 8), event_id = "AT-100Metres-M",
    tier = "OW", round = "F", date = Sys.Date() - rep(1:8, 2),
    perf = to_perf(10, -1L) + stats::rnorm(16, 0, 0.01))
  base <- estimate_ability(h, as_of = Sys.Date(), adjust_context = FALSE)
  cal <- list(sigma_context = data.table::data.table(family = "sprint", ratio = 0.5))
  scaled <- estimate_ability(h, as_of = Sys.Date(), adjust_context = FALSE,
                             calibration = cal)
  m <- merge(base[, .(athlete_id, s0 = sigma)], scaled[, .(athlete_id, s1 = sigma)],
             by = "athlete_id")
  expect_equal(m$s1, m$s0 * 0.5, tolerance = 1e-8)
  # A ratio of 1 must be an exact no-op, so the feature can be disabled cleanly.
  cal1 <- list(sigma_context = data.table::data.table(family = "sprint", ratio = 1))
  none <- estimate_ability(h, as_of = Sys.Date(), adjust_context = FALSE,
                           calibration = cal1)
  expect_equal(none$sigma, base$sigma)
})

test_that("one corrupt mark cannot buy an athlete a win probability", {
  # Regression, 2026-07-31. A Commonwealth Games entrant had three recorded 100m
  # marks -- 10.86, 17.33, 10.70 -- and the 17.33 is impossible. It gave him
  # sigma 0.1397 against a field median of 0.0115, and because a race is decided
  # by the BEST draw, the simulator turned that spread into 19% gold: second
  # favourite, on a predicted mark of 10.97. Nothing errored. The published card
  # simply had an athlete who could not break 10.7 as a live contender.
  #
  # The estimator must take its scale from the good side, where a mark that bad
  # cannot reach.
  ev <- "AT-100Metres-M"
  dates <- as.Date(c("2022-06-08", "2024-06-21", "2026-05-12"))
  mk <- function(id, marks) data.table::data.table(
    athlete_id = id, event_id = ev, date = dates, round = "F", tier = "GL",
    perf = to_perf(marks, -1L))
  bad   <- mk("BAD",   c(10.70, 17.33, 10.86))
  clean <- mk("CLEAN", c(10.70, 10.78, 10.86))

  ab <- estimate_ability(rbind(bad, clean), as_of = as.Date("2026-07-23"),
                         half_life = 365, adjust_context = FALSE)
  s_bad <- ab[athlete_id == "BAD"]$sigma
  s_cln <- ab[athlete_id == "CLEAN"]$sigma

  # The corrupt history must not produce a wildly wider athlete than the clean
  # one built from the same two good marks on the same dates.
  expect_lt(s_bad, 4 * s_cln)

  # And it must not out-rank a genuinely faster field.
  fld <- rbind(
    ab[, .(athlete_id, event_id, ability, sigma, ability_se)],
    data.table::data.table(athlete_id = paste0("R", 1:5), event_id = ev,
                           ability = to_perf(seq(9.95, 10.15, length.out = 5), -1L),
                           sigma = 0.0172, ability_se = 0.002))
  mp <- medal_probs(simulate_event(fld, n_sims = 20000, seed = 3))
  expect_lt(mp[athlete_id == "BAD"]$p_gold, min(mp[grepl("^R", athlete_id)]$p_gold))
})
