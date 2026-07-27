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

test_that("fouled athletes get distinct placings, never a shared one", {
  # Regression: the tie-break offset for fouls used to be added to a -1e300
  # sentinel, where a double's ULP is ~1e284 -- so it was annihilated and every
  # fouled athlete collapsed onto a single tied rank. The ranker then handed
  # out duplicate placings, and once enough of a small field fouled, that tied
  # rank was 3 or better and EVERY fouled athlete was credited with a medal.
  # Nothing errored; only the invariants below catch it.
  for (n in c(2L, 8L, 13L)) {
    ab <- make_ability(times = seq(5.9, 5.2, length.out = n), event_id = "AT-PoleVault-M")
    ab$ability <- to_perf(seq(5.9, 5.2, length.out = n), 1L)
    for (fp in c(0.15, 0.9)) {
      sim <- simulate_event(ab, n_sims = 1500, foul_prob = fp, seed = 5)
      # Every row a strict permutation of 1:n -- no duplicates, none skipped.
      expect_true(all(apply(sim$rank, 1L, function(r) identical(unname(sort(r)), seq_len(n)))))
      # Exactly min(n, 3) medals per race, however many athletes fouled.
      expect_equal(sum(medal_probs(sim)$p_medal), min(n, 3), tolerance = 1e-8)
    }
  }
})

test_that("fouls are ordered at random, not by entry order", {
  # Identical athletes fouling at 50%: any systematic advantage to whoever is
  # listed first shows up as a gradient in p_gold across entry order.
  ab <- make_ability(times = rep(5.8, 8L), event_id = "AT-PoleVault-M")
  ab$ability <- to_perf(rep(5.8, 8L), 1L)
  mp <- medal_probs(simulate_event(ab, n_sims = 30000, foul_prob = 0.5, seed = 9))
  p <- mp[order(match(mp$athlete_id, ab$athlete_id))]$p_gold
  expect_lt(abs(stats::cor(p, seq_along(p))), 0.6)
  expect_lt(diff(range(p)), 0.02)
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

test_that("ability uncertainty widens the outcome distribution", {
  # A point estimate treated as exact makes the simulator over-confident.
  base <- data.table::data.table(
    athlete_id = letters[1:6], event_id = "AT-100Metres-M",
    ability = to_perf(seq(9.85, 10.10, length.out = 6), -1L),
    sigma = 0.006)
  certain <- base[, ability_se := 0]
  uncertain <- data.table::copy(base)[, ability_se := 0.012]

  a <- medal_probs(simulate_event(certain, n_sims = 20000, foul_prob = 0, seed = 4))
  b <- medal_probs(simulate_event(uncertain, n_sims = 20000, foul_prob = 0, seed = 4))
  data.table::setorder(a, athlete_id); data.table::setorder(b, athlete_id)

  # The favourite must be less certain once ability itself is uncertain
  expect_gt(max(a$p_gold), max(b$p_gold))
})

test_that("zero ability_se reproduces the old behaviour", {
  ab <- data.table::data.table(
    athlete_id = letters[1:4], event_id = "AT-100Metres-M",
    ability = to_perf(c(9.9, 10.0, 10.1, 10.2), -1L), sigma = 0.01,
    ability_se = 0)
  no_col <- data.table::copy(ab)[, ability_se := NULL]
  a <- medal_probs(simulate_event(ab, n_sims = 8000, foul_prob = 0, seed = 6))
  b <- medal_probs(simulate_event(no_col, n_sims = 8000, foul_prob = 0, seed = 6))
  data.table::setorder(a, athlete_id); data.table::setorder(b, athlete_id)
  expect_equal(a$p_gold, b$p_gold, tolerance = 0.02)
})

test_that("estimate_ability reports a larger SE for sparser histories", {
  h <- rbind(
    data.table::data.table(athlete_id = "deep", event_id = "AT-100Metres-M",
      date = Sys.Date() - seq(10, 400, by = 20), tier = "OW", round = "F",
      perf = to_perf(10, -1L) + stats::rnorm(20, 0, 0.01)),
    data.table::data.table(athlete_id = "thin", event_id = "AT-100Metres-M",
      date = Sys.Date() - c(20, 40), tier = "OW", round = "F",
      perf = to_perf(10, -1L) + stats::rnorm(2, 0, 0.01)),
    data.table::rbindlist(lapply(1:8, function(i)
      data.table::data.table(athlete_id = paste0("o", i), event_id = "AT-100Metres-M",
        date = Sys.Date() - seq(10, 300, by = 30), tier = "OW", round = "F",
        perf = to_perf(10.2, -1L) + stats::rnorm(10, 0, 0.01)))))
  ab <- estimate_ability(h, adjust_context = FALSE, half_life = 365)
  expect_gt(ab[athlete_id == "thin"]$ability_se, ab[athlete_id == "deep"]$ability_se)
})
