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
