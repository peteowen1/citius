# Context-conditional shared-shock sd (2026-09-06). A calibration may carry a
# `condition_sd_context` table keyed by event/family x meet_tier x round_class;
# race_conditions() and simulate_event() read it only when given a context.

fake_cal <- function(with_context = TRUE) {
  ev <- data.table::data.table(event_id = c("AT-100Metres-M", "AT-200Metres-M"),
                               condition_sd = c(0.020, 0.021), calibrated = TRUE,
                               foul_rate = 0)
  ctx <- data.table::data.table(
    level       = c("event",          "family",  "family"),
    event_id    = c("AT-100Metres-M", NA,        NA),
    family      = c("sprint",         "sprint",  "sprint"),
    meet_tier   = c("T1_elite",       "T1_elite", "T2_strong"),
    round_class = c("final",          "final",   "heat"),
    n_races     = c(300L, 900L, 400L),
    cond_sd_raw = c(0.011, 0.013, 0.018),
    cond_sd     = c(0.012, 0.014, 0.019))
  structure(list(events = ev, tail_df = 6,
                 condition_sd_context = if (with_context) ctx else NULL),
            class = "citius_calibration")
}

test_that("race_conditions returns the event cell, then the family cell, then the event-wide value", {
  cal <- fake_cal()
  # event cell
  expect_equal(race_conditions("AT-100Metres-M", cal, list(meet_tier = "T1_elite", round_class = "final")), 0.012)
  # no event cell for the 200m -> family cell
  expect_equal(race_conditions("AT-200Metres-M", cal, list(meet_tier = "T1_elite", round_class = "final")), 0.014)
  # a context with no cell at any level -> event-wide value
  expect_equal(race_conditions("AT-100Metres-M", cal, list(meet_tier = "T3_development", round_class = "semi")), 0.020)
  # no context -> event-wide value, byte-identical to the old behaviour
  expect_equal(race_conditions("AT-100Metres-M", cal), 0.020)
  expect_equal(race_conditions("AT-100Metres-M", cal, NULL), 0.020)
  # NA fields in the context -> event-wide value
  expect_equal(race_conditions("AT-100Metres-M", cal, list(meet_tier = NA, round_class = "final")), 0.020)
  # calibration without the table -> event-wide value even with a context
  expect_equal(race_conditions("AT-100Metres-M", fake_cal(FALSE), list(meet_tier = "T1_elite", round_class = "final")), 0.020)
})

test_that("simulate_event uses the context cell and records it, and is unchanged without one", {
  cal <- fake_cal()
  ab <- data.table::data.table(athlete_id = paste0("a", 1:6), event_id = "AT-100Metres-M",
                               ability = -log(seq(9.9, 10.15, length.out = 6)),
                               sigma = 0.008)
  s0 <- simulate_event(ab, n_sims = 500, calibration = cal, seed = 3L)
  s1 <- simulate_event(ab, n_sims = 500, calibration = cal, seed = 3L,
                       context = list(meet_tier = "T1_elite", round_class = "final"))
  expect_equal(s0$settings$condition_sd, 0.020)
  expect_equal(s1$settings$condition_sd, 0.012)
  expect_null(s0$settings$context)
  expect_equal(s1$settings$context$meet_tier, "T1_elite")
  # a smaller shared shock: the spread of simulated perf is narrower
  expect_lt(stats::sd(s1$perf[, 1]), stats::sd(s0$perf[, 1]))
  # and the field's ORDER is untouched: same seed, same ranks (a shared shock
  # cancels from every pairwise comparison, whatever its size)
  expect_identical(s0$rank, s1$rank)
})

test_that("spread_scales apply only with a context, and sigma_marks drives the mark distribution", {
  cal <- fake_cal()
  cal$spread_scales <- data.table::data.table(family = "sprint", k_shared = 0.5, k_indiv = 2, n_races = 100L, n_rows = 800L)
  ab <- data.table::data.table(athlete_id = paste0("a", 1:6), event_id = "AT-100Metres-M",
                               ability = -log(seq(9.9, 10.15, length.out = 6)),
                               sigma = 0.008, sigma_marks = 0.004)
  s0 <- simulate_event(ab, n_sims = 500, calibration = cal, seed = 5L)
  s1 <- simulate_event(ab, n_sims = 500, calibration = cal, seed = 5L,
                       context = list(meet_tier = "T1_elite", round_class = "final"))
  expect_equal(s0$settings$k_shared, 1); expect_equal(s0$settings$condition_sd, 0.020)
  expect_equal(s1$settings$k_shared, 0.5); expect_equal(s1$settings$condition_sd, 0.012 * 0.5)
  expect_equal(s1$settings$k_indiv, 2)
  expect_true(s1$settings$sigma_marks_used)
  # perf_std exists without ability_peak now that sigma_marks is present
  expect_false(is.null(s0$perf_std))
  # the ranking never sees sigma_marks or k_indiv: identical to a run without them
  ab2 <- data.table::copy(ab)[, sigma_marks := NULL]
  s2 <- simulate_event(ab2, n_sims = 500, calibration = cal, seed = 5L,
                       context = list(meet_tier = "T1_elite", round_class = "final"))
  expect_identical(s1$rank, s2$rank)
  expect_null(s2$perf_std)
})

test_that("estimate_ability emits sigma_marks between sigma_raw and the event target", {
  set.seed(9)
  h <- data.table::rbindlist(lapply(1:8, function(a) data.table::data.table(
    athlete_id = paste0("a", a), event_id = "AT-100Metres-M",
    date = as.Date("2026-06-01") - sample(10:600, 15),
    perf = -log(10 + a * 0.03 + rnorm(15, 0, 0.04 * a)))))
  ab <- estimate_ability(h, as_of = as.Date("2026-06-01"))
  expect_true("sigma_marks" %in% names(ab))
  expect_true(all(is.finite(ab$sigma_marks) & ab$sigma_marks > 0))
  # heavy shrinkage: the marks spread varies less across athletes than sigma_raw does
  expect_lt(stats::sd(ab$sigma_marks) / mean(ab$sigma_marks), stats::sd(ab$sigma_raw) / mean(ab$sigma_raw))
})

test_that("the excess strip removes (1 - beta) * (c_r - expected) and nothing when beta = 1", {
  set.seed(4)
  h <- data.table::rbindlist(lapply(1:6, function(a) data.table::data.table(
    athlete_id = paste0("a", a), event_id = "AT-100Metres-M",
    date = as.Date("2026-06-01") - (1:6) * 30,
    race_key = paste0("r", 1:6), round = "Final", tier = "A",
    perf = -log(10 + a * 0.02 + rnorm(6, 0, 0.02)))))
  race <- data.table::data.table(race_key = paste0("r", 1:6), event_id = "AT-100Metres-M",
                                 c_r = c(0.03, 0.01, 0.01, 0.01, 0.01, 0.01), n_in_race = 50L,
                                 round = "Final", tier = "A")
  ev <- data.table::data.table(event_id = "AT-100Metres-M", sigma_within = 0.01, condition_sd = 0.015,
                               tactical_index = 0, calibrated = TRUE, foul_rate = 0)
  expected <- data.table::data.table(event_id = "AT-100Metres-M", tier_class = "high", round_class = "final",
                                     e_cell = 0.01, n_cell = 100L)
  mk <- function(beta) structure(list(events = ev, race = race, tail_df = 6,
                                      race_shock = list(beta = beta, expected = expected, by_tier = NULL)),
                                 class = "citius_calibration")
  off  <- estimate_ability(h, as_of = as.Date("2026-06-01"), calibration = mk(0), adjust_race = FALSE)
  b1   <- estimate_ability(h, as_of = as.Date("2026-06-01"), calibration = mk(1), adjust_race = TRUE)
  b0   <- estimate_ability(h, as_of = as.Date("2026-06-01"), calibration = mk(0), adjust_race = TRUE)
  # beta = 1: nothing stripped, identical to adjust_race = FALSE
  expect_equal(b1$ability, off$ability)
  # beta = 0: only race r1 (excess +0.02) is stripped; the other five sit exactly
  # on the cell expectation and are untouched, so every ability drops
  expect_true(all(b0$ability < off$ability))
  # and by less than the full excess, because the strip is shrunk by field size
  expect_true(all(off$ability - b0$ability < 0.02))
})
