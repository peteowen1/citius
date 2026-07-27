make_ability <- function(times = c(9.80, 9.90, 10.00), sigma = 0.008,
                         event_id = "AT-100Metres-M") {
  data.table::data.table(
    athlete_id = letters[seq_along(times)],
    event_id = event_id,
    ability = to_perf(times, -1L),
    sigma = sigma
  )
}

test_that("the better athlete wins more often", {
  sim <- simulate_event(make_ability(), n_sims = 5000, seed = 1)
  mp <- medal_probs(sim)
  expect_equal(mp$athlete_id[1], "a")
  expect_gt(mp$p_gold[1], mp$p_gold[2])
})

test_that("probabilities are coherent", {
  sim <- simulate_event(make_ability(), n_sims = 5000, seed = 2)
  mp <- medal_probs(sim)
  expect_equal(sum(mp$p_gold), 1, tolerance = 1e-8)
  expect_true(all(mp$p_gold <= mp$p_medal))
  expect_true(all(mp$p_medal <= 1))
})

test_that("a wider sigma makes the favourite less certain", {
  tight <- medal_probs(simulate_event(make_ability(sigma = 0.004), n_sims = 20000, seed = 3))
  loose <- medal_probs(simulate_event(make_ability(sigma = 0.020), n_sims = 20000, seed = 3))
  expect_gt(tight[tight$athlete_id == "a", ]$p_gold,
            loose[loose$athlete_id == "a", ]$p_gold)
})

test_that("a shared additive shock leaves placings unchanged", {
  # A shock common to the whole field cancels out of every pairwise comparison,
  # so it cannot move medal probabilities. It only affects absolute marks.
  with_cond <- medal_probs(simulate_event(make_ability(), n_sims = 40000,
                                          condition_sd = 0.02, seed = 7))
  without <- medal_probs(simulate_event(make_ability(), n_sims = 40000,
                                        condition_sd = 0, seed = 7))
  data.table::setorder(with_cond, athlete_id)
  data.table::setorder(without, athlete_id)
  expect_equal(with_cond$p_gold, without$p_gold, tolerance = 0.02)
})

test_that("a shared shock does widen the distribution of marks", {
  wide <- simulate_event(make_ability(), n_sims = 40000, condition_sd = 0.02, seed = 8)
  narrow <- simulate_event(make_ability(), n_sims = 40000, condition_sd = 0, seed = 8)
  expect_gt(stats::sd(wide$perf[, 1]), stats::sd(narrow$perf[, 1]))
})

test_that("prob_better_than is monotone in the threshold", {
  sim <- simulate_event(make_ability(), n_sims = 20000, seed = 4)
  expect_gt(prob_better_than(sim, 10.00, "any"), prob_better_than(sim, 9.70, "any"))
})

test_that("fouls rank an athlete last without reading as a slow mark", {
  ab <- make_ability(times = c(8.90, 8.50), event_id = "AT-LongJump-M")
  ab$ability <- to_perf(c(8.90, 8.50), 1L)
  sim <- simulate_event(ab, n_sims = 4000, foul_prob = 0.5, seed = 5)
  expect_true(any(!is.finite(sim$perf)))
  # Median mark must ignore the fouls rather than average them in
  expect_true(all(is.finite(medal_probs(sim)$median_mark)))
})

test_that("simulate_event rejects an unusable field", {
  expect_error(simulate_event(make_ability(times = 9.8), n_sims = 100), "at least 2")
  bad <- make_ability()[, c("athlete_id", "event_id")]
  expect_error(simulate_event(bad, n_sims = 100), "missing required")
})

test_that("swimming carries a smaller shared shock than sprinting", {
  expect_lt(race_conditions("SW-100mFreestyle-M"), race_conditions("AT-100Metres-M"))
})

test_that("no-mark probability applies to track events, not just field", {
  # A distance runner failing to finish and a vaulter fouling out are the same
  # thing for ranking. Championship distance races carry ~10% DNF rates.
  ab <- data.table::data.table(
    athlete_id = letters[1:6], event_id = "AT-10000Metres-M",
    ability = to_perf(seq(1560, 1600, length.out = 6), -1L), sigma = 0.02)
  sim <- simulate_event(ab, n_sims = 5000, foul_prob = 0.2, seed = 3)
  expect_true(any(!is.finite(sim$perf)))
  # A DNF must rank last, never read as a slow time
  expect_true(all(is.finite(medal_probs(sim)$median_mark)))
})

test_that("a calibrated no-mark rate is picked up for any event type", {
  d <- data.table::data.table(
    race_key = rep(paste0("r", 1:40), each = 6),
    athlete_id = as.character(rep_len(1:12, 240)),
    event_id = "AT-PoleVault-M", date = Sys.Date() - 1,
    round = "F", tier = "OW",
    mark = 5.5)
  d[, perf := to_perf(mark, 1L)]
  d[sample(.N, 24), perf := NA_real_]        # 10% no-marks
  cal <- calibrate(d, min_races = 5L)
  expect_gt(cal$events$foul_rate[1], 0.05)
  expect_lt(cal$events$foul_rate[1], 0.15)
})

test_that("simulate_event warns when no-mark rate was never measured", {
  # The warning is rate-limited to once per session so it does not spam long
  # runs; reset it so this test sees it regardless of what ran before.
  rlang::reset_warning_verbosity("citius_no_foul_rate")
  ab <- data.table::data.table(
    athlete_id = letters[1:4], event_id = "AT-PoleVault-M",
    ability = to_perf(c(6.0, 5.9, 5.8, 5.7), 1L), sigma = 0.02)
  expect_warning(simulate_event(ab, n_sims = 500), "no-mark rate")
})
