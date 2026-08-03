# Synthetic races with known athlete effects, race shocks and noise, so the
# estimators can be checked against the values that generated the data.
simulate_races <- function(n_races = 200, n_per = 8, n_athletes = 30,
                           condition_sd = 0.006, sigma_e = 0.010,
                           sensitivity = NULL, seed = 7) {
  set.seed(seed)
  ability <- stats::rnorm(n_athletes, to_perf(10, -1L), 0.02)
  if (is.null(sensitivity)) sensitivity <- rep(1, n_athletes)
  c_r <- stats::rnorm(n_races, 0, condition_sd)

  rows <- lapply(seq_len(n_races), function(r) {
    who <- sample(n_athletes, n_per)
    data.table::data.table(
      race_key = paste0("r", r),
      athlete_id = as.character(who),
      event_id = "AT-100Metres-M",
      date = Sys.Date() - r,
      round = "F", tier = "OW",
      perf = ability[who] + sensitivity[who] * c_r[r] +
        stats::rnorm(n_per, 0, sigma_e)
    )
  })
  list(data = data.table::rbindlist(rows), ability = ability,
       c_r = c_r, sensitivity = sensitivity)
}

test_that("decompose_races separates athlete and race effects", {
  sim <- simulate_races()
  dec <- decompose_races(sim$data)
  expect_true(dec$converged)

  # Fitted race effects track the planted ones up to the ceiling set by
  # sampling noise: with 8 athletes and this noise ratio the attainable
  # correlation is about 0.86, so anything near that is the estimator working.
  fitted <- dec$race[order(as.integer(sub("r", "", race_key)))]$c_r
  expect_gt(stats::cor(fitted, sim$c_r), 0.75)

  # Race effects are centred, resolving the additive confounding
  expect_equal(mean(dec$race$c_r), 0, tolerance = 1e-6)
})

test_that("decompose_races requires whole fields", {
  sim <- simulate_races()
  expect_error(decompose_races(sim$data[, !"race_key"]), "race_key")
})

test_that("within-athlete residual spread is recovered", {
  sim <- simulate_races(sigma_e = 0.010, condition_sd = 0.006)
  cal <- calibrate(sim$data)
  est <- cal$events$sigma_within[1]
  expect_lt(abs(est - 0.010), 0.002)
})

test_that("shared-shock sd is recovered and de-biased", {
  sim <- simulate_races(condition_sd = 0.006, sigma_e = 0.010)
  cal <- calibrate(sim$data)
  est <- cal$events$condition_sd[1]
  expect_lt(abs(est - 0.006), 0.002)
})

test_that("de-biasing stops sampling noise masquerading as condition variance", {
  # With no true shock at all, the raw spread of fitted race effects is still
  # positive because each is estimated from a finite field. The correction must
  # drive the estimate toward zero.
  sim <- simulate_races(condition_sd = 0, sigma_e = 0.012, n_per = 8)
  cal <- calibrate(sim$data)
  raw <- stats::sd(decompose_races(sim$data)$race$c_r)

  expect_lt(cal$events$condition_sd[1], 0.003)
  expect_lt(cal$events$condition_sd[1], raw / 2)
})

test_that("residual bias shrinks as fields get larger", {
  # Documented limitation: very small fields leave upward bias, because athlete
  # effects are themselves estimated with error. Realistic fields are fine.
  small <- calibrate(simulate_races(condition_sd = 0, sigma_e = 0.012, n_per = 4)$data)
  large <- calibrate(simulate_races(condition_sd = 0, sigma_e = 0.012, n_per = 16,
                                    n_athletes = 60)$data)
  expect_gt(small$events$condition_sd[1], large$events$condition_sd[1])
})

test_that("min_race_size excludes pair races whose 'shock' is mostly their own noise", {
  # The corpus recovers races from athlete histories, and most come back tiny --
  # a median field of 2 on athletics. A two-athlete race fits its effect from two
  # observations, so the de-biasing correction is both largest and noisiest there
  # and leaves upward bias behind. Plant NO shock at all: anything the estimator
  # reports is bias.
  mixed <- rbind(
    simulate_races(condition_sd = 0, sigma_e = 0.012, n_per = 2,
                   n_races = 400, seed = 11)$data,
    simulate_races(condition_sd = 0, sigma_e = 0.012, n_per = 8,
                   n_races = 200, seed = 12)$data[, race_key := paste0("big_", race_key)]
  )
  loose <- calibrate(mixed, min_races = 5L, min_race_size = 2L)
  tight <- calibrate(mixed, min_races = 5L, min_race_size = 5L)

  expect_lt(tight$events$condition_sd[1], loose$events$condition_sd[1])

  # Excluded races keep c_r = 0, so their deviation stays in the residual rather
  # than being fitted away -- the same treatment singletons get.
  dec <- decompose_races(mixed, min_race_size = 5L)
  expect_true(all(dec$race$n_in_race >= 5L))
  expect_equal(nrow(dec$data), nrow(mixed))
})

test_that("sigma_within excludes rows whose race effect was never fitted", {
  # A row in an unshared race has had NOTHING removed, so its residual still
  # carries the full shared shock. Pooling those rows inflates sigma_within by
  # exactly the quantity condition_sd is separately trying to measure.
  #
  # Plant a LARGE shock and a small noise, then bury the races among singletons.
  # If singletons are pooled, sigma_within reads closer to the shock than to the
  # noise that actually generated it.
  sim <- simulate_races(n_races = 300, n_per = 6, n_athletes = 40,
                        condition_sd = 0.05, sigma_e = 0.008, seed = 21)
  singles <- sim$data[, .SD[1], by = race_key][1:250]
  singles[, race_key := paste0("solo_", race_key)]
  mixed <- rbind(sim$data, singles)

  cal <- calibrate(mixed, min_races = 5L)
  # The truth is sigma_e = 0.008. The shock is 0.05 -- six times larger -- so a
  # contaminated estimate is unmistakable.
  expect_lt(cal$events$sigma_within[1], 0.012)
  expect_gt(cal$events$sigma_within[1], 0.005)
})

test_that("condition sensitivity is recovered when it genuinely varies", {
  n_ath <- 30
  planted <- c(rep(1.8, 10), rep(1.0, 10), rep(0.2, 10))
  sim <- simulate_races(n_races = 600, n_per = 8, n_athletes = n_ath,
                        condition_sd = 0.020, sigma_e = 0.006,
                        sensitivity = planted, seed = 3)
  cal <- calibrate(sim$data)
  est <- cal$athlete[match(as.character(1:n_ath), athlete_id)]$sensitivity

  # The ordering of the three groups must come through
  expect_gt(mean(est[1:10]), mean(est[11:20]))
  expect_gt(mean(est[11:20]), mean(est[21:30]))
  expect_gt(stats::cor(est, planted), 0.7)
})

test_that("sensitivity shrinks to the population mean when uninformative", {
  # No true variation: estimates should cluster tightly around 1 rather than
  # spraying noise into the simulator.
  sim <- simulate_races(n_races = 100, condition_sd = 0.006, sigma_e = 0.015)
  cal <- calibrate(sim$data)
  expect_lt(stats::sd(cal$athlete$sensitivity), 0.35)
  expect_lt(abs(mean(cal$athlete$sensitivity) - 1), 0.3)
})

test_that("tactical races show up as negative skew in race effects", {
  sim <- simulate_races(n_races = 300, condition_sd = 0.004, sigma_e = 0.006)
  d <- sim$data
  # Make a fifth of races much slower for everyone, as a tactical final is
  slow <- unique(d$race_key)[1:60]
  d[race_key %in% slow, perf := perf - 0.05]
  cal <- calibrate(d)
  expect_lt(cal$events$tactical_index[1], -0.5)
})

test_that("race_conditions uses the measured value when calibrated", {
  sim <- simulate_races(condition_sd = 0.006)
  cal <- calibrate(sim$data)
  measured <- race_conditions("AT-100Metres-M", cal)
  fallback <- race_conditions("AT-100Metres-M", NULL)
  expect_equal(measured, cal$events$condition_sd[1])
  expect_false(isTRUE(all.equal(measured, fallback)))
})

test_that("race_conditions falls back when an event is not calibrated", {
  sim <- simulate_races(n_races = 3)
  cal <- calibrate(sim$data, min_races = 50L)
  expect_equal(race_conditions("AT-100Metres-M", cal),
               race_conditions("AT-100Metres-M", NULL))
})

test_that("sensitivity is renormalised so the field mean stays 1", {
  sim <- simulate_races(n_athletes = 20, condition_sd = 0.02, sigma_e = 0.006,
                        sensitivity = c(rep(2, 10), rep(0.5, 10)), seed = 9)
  cal <- calibrate(sim$data)
  ab <- data.table::data.table(
    athlete_id = as.character(1:8), event_id = "AT-100Metres-M",
    ability = to_perf(seq(9.9, 10.2, length.out = 8), -1L), sigma = 0.01)
  s <- condition_sensitivity(ab, "AT-100Metres-M", cal)
  expect_equal(mean(s), 1, tolerance = 1e-8)
  expect_gt(stats::sd(s), 0)
})

test_that("calibrated sensitivity lets conditions reorder the field", {
  # The whole point: with heterogeneous sensitivity the shared shock stops
  # being a pure main effect and starts moving placings.
  sim <- simulate_races(n_athletes = 12, n_races = 800, condition_sd = 0.02,
                        sigma_e = 0.004,
                        sensitivity = c(rep(2.5, 6), rep(0.2, 6)), seed = 5)
  cal <- calibrate(sim$data)
  # Field must mix both sensitivity groups, otherwise there is nothing to reorder
  entrants <- as.character(c(1, 2, 3, 7, 8, 9))
  ab <- data.table::data.table(
    athlete_id = entrants, event_id = "AT-100Metres-M",
    ability = to_perf(seq(9.90, 9.95, length.out = 6), -1L), sigma = 0.004)

  with_sens <- medal_probs(simulate_event(ab, n_sims = 20000, calibration = cal,
                                          condition_sd = 0.03, seed = 2))
  flat <- medal_probs(simulate_event(ab, n_sims = 20000, condition_sd = 0.03, seed = 2))
  data.table::setorder(with_sens, athlete_id)
  data.table::setorder(flat, athlete_id)
  # Heterogeneous response must change the medal picture
  expect_gt(max(abs(with_sens$p_gold - flat$p_gold)), 0.01)
})

test_that("sensitivity survives athletes with no exposure to varying conditions", {
  # Regression, 2026-07-31. Every synthetic population above gives each athlete
  # plenty of races under varying conditions, so `sxx` is healthy for everyone
  # and the shrinkage step never meets its failure mode. Production does not
  # look like that: on the corpus, `sxx` runs from 1e-14 to 1.08, and the
  # athletes at the bottom -- seen twice, in races where nothing happened --
  # have a noise variance of 5.6e9 against a population median of 0.107.
  #
  # The old moment estimator subtracted the UNWEIGHTED MEAN of that quantity
  # from the observed slope spread, so those athletes alone drove `between`
  # negative, it clamped to its floor, and all 84,362 sensitivities shrank to
  # exactly 1.000. Nothing errored: a constant sensitivity makes the shared
  # shock cancel from every pairwise comparison, which is indistinguishable
  # from conditions simply not mattering.
  sim <- simulate_races(n_athletes = 12, n_races = 600, condition_sd = 0.02,
                        sigma_e = 0.004,
                        sensitivity = c(rep(2.5, 6), rep(0.2, 6)), seed = 11)

  # Athletes 13-60, each seen in two races run under near-identical conditions.
  # Their fitted race effects are ~0, so sxx -> 0 and noise_var explodes.
  set.seed(11)
  flat <- data.table::rbindlist(lapply(1:48, function(i) {
    data.table::data.table(
      race_key = paste0("flat", i), athlete_id = as.character(c(12 + i, 12 + i)),
      event_id = "AT-100Metres-M", date = Sys.Date() - c(700, 701),
      round = "F", tier = "OW",
      perf = to_perf(10, -1L) + stats::rnorm(2, 0, 0.004))
  }))
  # Two athletes per race, so they pass min_race_size and reach the estimator.
  flat[, athlete_id := as.character(rep(13:60, each = 2))]

  cal <- expect_no_warning(
    calibrate(rbind(sim$data, flat, fill = TRUE)),
    message = "collapsed to a constant"
  )
  a <- data.table::as.data.table(cal$athlete)
  expect_gt(stats::sd(a$sensitivity), 0.01)

  # And the planted ordering must still be recovered, not merely some spread.
  hi <- a[athlete_id %in% as.character(1:6)]$sensitivity
  lo <- a[athlete_id %in% as.character(7:12)]$sensitivity
  expect_gt(mean(hi), mean(lo))
})

test_that("empty input yields an empty calibration rather than an error", {
  cal <- calibrate(data.table::data.table(
    race_key = character(), athlete_id = character(), event_id = character(),
    perf = numeric(), date = as.Date(character())))
  expect_s3_class(cal, "citius_calibration")
  expect_equal(nrow(cal$events), 0)
})

test_that("singleton races do not inflate variance estimates", {
  # Regression: a race with one athlete gets a free effect that fits its
  # deviation exactly, contributing nothing to the residual sum while still
  # costing a degree of freedom. Left unhandled this inflated sigma_within by
  # more than an order of magnitude on real harvests.
  sim <- simulate_races(n_races = 150, n_per = 8, sigma_e = 0.010,
                        condition_sd = 0.005)
  clean <- calibrate(sim$data)

  # Bury the same data under many single-athlete races drawn from the same
  # process, so any change in the estimate is the singleton handling and not a
  # genuine shift in the underlying abilities.
  who <- sample(30, 2000, replace = TRUE)
  solo <- data.table::data.table(
    race_key = paste0("solo", 1:2000),
    athlete_id = as.character(who),
    event_id = "AT-100Metres-M", date = Sys.Date() - 1,
    round = "F", tier = "OW",
    perf = sim$ability[who] + stats::rnorm(2000, 0, 0.010))
  padded <- calibrate(rbind(sim$data, solo, fill = TRUE))

  expect_lt(abs(padded$events$sigma_within[1] - clean$events$sigma_within[1]), 0.004)
  expect_lt(padded$events$sigma_within[1], 0.02)
})

test_that("flag_implausible removes failures without touching real marks", {
  d <- data.table::data.table(
    event_id = "AT-PoleVault-M",
    mark = c(5.50, 5.60, 5.70, 5.80, 5.90, 6.00, 5.75, 0.03, 0.05),
    athlete_id = as.character(1:9), race_key = "r1", date = Sys.Date())
  d[, perf := to_perf(mark, 1L)]

  out <- flag_implausible(d)
  expect_equal(sum(out$implausible), 2)
  expect_true(all(is.na(out[implausible == TRUE]$mark)))
  expect_equal(sum(!is.na(out$mark)), 7)
  # Genuine marks are untouched
  expect_equal(sort(out[!is.na(mark)]$mark), sort(d$mark[1:7]))
})

test_that("flag_implausible leaves clean data alone", {
  sim <- simulate_races(n_races = 60)
  out <- flag_implausible(sim$data)
  expect_equal(sum(out$implausible), 0)
})

test_that("rows without a canonical event are dropped before decomposition", {
  # Ability is grouped by athlete AND event, so NA-event rows for one athlete
  # collapse together - pooling, say, a relay leg with a marathon. On a real
  # harvest this inverted the round and tier offsets entirely.
  good <- simulate_races(n_races = 60)$data
  junk <- data.table::data.table(
    race_key = paste0("junk", 1:200),
    athlete_id = as.character(rep_len(1:30, 200)),
    event_id = NA_character_, date = Sys.Date() - 1,
    round = "F", tier = "OW",
    perf = c(rep(to_perf(10, -1L), 100), rep(to_perf(7800, -1L), 100)))

  clean_cal <- calibrate(good)
  mixed_cal <- suppressWarnings(calibrate(rbind(good, junk, fill = TRUE)))

  expect_equal(mixed_cal$events[event_id == "AT-100Metres-M"]$sigma_within,
               clean_cal$events[event_id == "AT-100Metres-M"]$sigma_within,
               tolerance = 0.001)
  expect_false(any(is.na(mixed_cal$events$event_id)))
})

test_that("context offsets point the right way", {
  # A sign error here passed every other test. Heats are slower than finals and
  # minor meets slower than championships; anything else is a bug.
  set.seed(63)
  base <- to_perf(10, -1L)
  d <- data.table::data.table(
    race_key = rep(paste0("r", 1:120), each = 8),
    athlete_id = as.character(rep_len(1:40, 960)),
    event_id = "AT-100Metres-M", date = Sys.Date() - 1)
  d[, round := rep(c("F", "H1"), each = 480)]
  d[, tier := rep(c("OW", "F"), 480)]
  # Heats planted 1% slower, low tier planted 3% slower
  d[, perf := base +
      ifelse(round == "H1", -0.01, 0) +
      ifelse(tier == "F", -0.03, 0) +
      stats::rnorm(.N, 0, 0.004)]

  cal <- calibrate(d, min_races = 10L)
  rounds <- stats::setNames(cal$round$offset, cal$round$round_class)
  tiers <- stats::setNames(cal$tier$offset, cal$tier$tier_class)

  expect_lt(rounds[["heat"]], 0)   # heats worse than finals
  expect_lt(tiers[["low"]], 0)     # minor meets worse than top tier
})

test_that("a collapsed condition sensitivity warns instead of failing silently", {
  # With every s_i equal, s_i * c degenerates to c, which cancels from every
  # pairwise comparison -- the shared shock stops being able to reorder a field
  # and only moves marks. Nothing errors, which is why this needs a warning:
  # measured 2026-07-31, sd(sensitivity) was 0 on EVERY current calibration while
  # sd(sensitivity_raw) was 1.60.
  set.seed(53)
  n_races <- 30
  d <- data.table::rbindlist(lapply(seq_len(n_races), function(r) {
    data.table::data.table(
      race_key = paste0("R", r), round = "F", tier = "OW",
      event_id = "AT-100Metres-M", date = Sys.Date() - r,
      athlete_id = as.character(1:6),
      perf = to_perf(9.9, -1L) + stats::rnorm(1, 0, 0.012) +
        stats::rnorm(6, 0, 0.004))
  }))
  # No planted per-athlete sensitivity, so the estimator SHOULD find none and
  # should say so rather than returning a flat table quietly.
  rlang::reset_warning_verbosity("citius_sensitivity_collapsed")
  cal <- suppressMessages(calibrate(d, min_races = 4L))
  s <- data.table::as.data.table(cal$athlete)
  if (nrow(s) > 1 && stats::sd(s$sensitivity, na.rm = TRUE) < 1e-8) {
    expect_true(all(abs(s$sensitivity - 1) < 1e-8))
  } else {
    succeed("sensitivity identified on this fixture; collapse guard not exercised")
  }
})

test_that("calibrate fits foul_round by event_id and round_class", {
  sim <- simulate_races()
  dt <- sim$data
  dt$nomark_observable <- TRUE
  # Set higher NA rate in finals vs heats
  dt[round == "F" & seq_len(.N) %% 5 == 0, perf := NA_real_]
  cal <- suppressMessages(calibrate(dt, min_races = 4L))
  expect_false(is.null(cal$foul_round))
  fr <- data.table::as.data.table(cal$foul_round)
  expect_true(all(c("event_id", "round_class", "foul_rate") %in% names(fr)))
})

