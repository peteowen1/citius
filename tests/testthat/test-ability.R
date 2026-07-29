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
