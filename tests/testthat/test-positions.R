make8 <- function() data.table::data.table(
  athlete_id = as.character(1:8), event_id = "AT-100Metres-M",
  ability = to_perf(seq(9.9, 10.3, length.out = 8), -1L),
  sigma = 0.012, ability_se = 0.005)

test_that("position probabilities sum to 1 per athlete", {
  sim <- simulate_event(make8(), n_sims = 4000, foul_prob = 0, seed = 1)
  p <- position_probs(sim)
  expect_equal(p[, sum(prob), by = athlete_id]$V1, rep(1, 8), tolerance = 1e-9)
})

test_that("each position is awarded exactly once across the field", {
  sim <- simulate_event(make8(), n_sims = 4000, foul_prob = 0, seed = 2)
  p <- position_probs(sim)
  expect_equal(p[!is.na(position), sum(prob), by = position]$V1, rep(1, 8),
               tolerance = 1e-9)
})

test_that("position 1 matches medal_probs gold", {
  sim <- simulate_event(make8(), n_sims = 6000, foul_prob = 0, seed = 3)
  p <- position_probs(sim)[position == 1L]
  m <- medal_probs(sim)
  j <- merge(p[, .(athlete_id, prob)], m[, .(athlete_id, p_gold)], by = "athlete_id")
  expect_equal(j$prob, j$p_gold, tolerance = 1e-9)
})

test_that("cumulative top-3 matches medal probability", {
  sim <- simulate_event(make8(), n_sims = 6000, foul_prob = 0, seed = 4)
  p <- position_probs(sim)[position == 3L]
  m <- medal_probs(sim)
  j <- merge(p[, .(athlete_id, cum_prob)], m[, .(athlete_id, p_medal)], by = "athlete_id")
  expect_equal(j$cum_prob, j$p_medal, tolerance = 1e-9)
})

test_that("the stronger athlete is likelier to finish first and less likely last", {
  sim <- simulate_event(make8(), n_sims = 6000, foul_prob = 0, seed = 5)
  w <- position_probs(sim, wide = TRUE)
  expect_gt(w[athlete_id == "1"]$pos_1, w[athlete_id == "8"]$pos_1)
  expect_lt(w[athlete_id == "1"]$pos_8, w[athlete_id == "8"]$pos_8)
})

test_that("positions beyond max_position pool into one NA row", {
  ab <- data.table::data.table(
    athlete_id = as.character(1:12), event_id = "AT-100Metres-M",
    ability = to_perf(seq(9.9, 10.5, length.out = 12), -1L),
    sigma = 0.012, ability_se = 0.005)
  sim <- simulate_event(ab, n_sims = 3000, foul_prob = 0, seed = 6)
  p <- position_probs(sim, max_position = 4L)
  expect_equal(p[, sum(prob), by = athlete_id]$V1, rep(1, 12), tolerance = 1e-9)
  expect_true(all(p[is.na(position)]$prob > 0))
})

test_that("stage_probs tracks a field shrinking through the rounds", {
  ab <- data.table::data.table(
    athlete_id = as.character(1:24), event_id = "AT-100Metres-M",
    ability = to_perf(seq(9.9, 10.4, length.out = 24), -1L),
    sigma = 0.012, ability_se = 0.005)
  s <- stage_probs(ab,
                   fields = list(entry = as.character(1:24),
                                 contested = as.character(1:20),
                                 final = as.character(1:8)),
                   structure = list(list(races = 3, advance = 2, fastest_losers = 2),
                                    list(races = 1)),
                   n_sims = 3000, seed = 7)
  expect_equal(data.table::uniqueN(s$stage), 3L)
  # Gold probability must sum to 1 within every stage.
  expect_equal(s[, sum(p_gold), by = stage]$V1, rep(1, 3), tolerance = 0.02)
  # The final stage has no qualification left.
  expect_true(all(s[stage == "final"]$p_final == 1))
  # Qualification risk must make p_final < 1 at the entry stage.
  expect_true(all(s[stage == "entry"]$p_final <= 1))
  expect_lt(min(s[stage == "entry"]$p_final), 1)
})

test_that("a strong rival withdrawing lifts the favourite", {
  # The withdrawal that matters is a CONTENDER, not a tail-ender. Dropping the
  # four weakest of 24 moves the favourite by -0.004, i.e. noise, because they
  # held no probability to redistribute. Glasgow's actual case was the high jump
  # favourite withdrawing, so test that shape.
  ab <- data.table::data.table(
    athlete_id = as.character(1:12), event_id = "AT-100Metres-M",
    ability = to_perf(seq(9.9, 10.2, length.out = 12), -1L),
    sigma = 0.012, ability_se = 0.005)
  s <- stage_probs(ab,
                   fields = list(entry = as.character(1:12),
                                 contested = as.character(setdiff(1:12, 2))),
                   n_sims = 8000, seed = 8)
  expect_gt(s[stage == "contested" & athlete_id == "1"]$p_gold,
            s[stage == "entry" & athlete_id == "1"]$p_gold)
  # And the withdrawn athlete simply stops appearing.
  expect_false("2" %in% s[stage == "contested"]$athlete_id)
})
