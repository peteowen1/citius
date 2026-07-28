make_field <- function(n, spread = 0.03) {
  data.table::data.table(
    athlete_id = as.character(seq_len(n)),
    event_id = "AT-100Metres-M",
    ability = to_perf(seq(9.9, 9.9 * (1 + spread), length.out = n), -1L),
    sigma = 0.010, ability_se = 0.004)
}

test_that("qualification uncertainty lowers medal probability", {
  ab <- make_field(24)
  direct <- medal_probs(simulate_event(ab[1:8], n_sims = 4000, foul_prob = 0, seed = 1))
  staged <- simulate_rounds(ab, list(list(races = 3, advance = 2, fastest_losers = 2),
                                     list(races = 1)), n_sims = 4000, seed = 1)
  # The favourite must be worse off when they still have to qualify than when
  # the final field is handed to them. Conditioning on a known final is exactly
  # the optimism this function exists to remove.
  expect_lt(staged[athlete_id == "1"]$p_gold, direct[athlete_id == "1"]$p_gold)
  expect_lt(staged[athlete_id == "1"]$p_final, 1)
})

test_that("probabilities are coherent", {
  ab <- make_field(24)
  r <- simulate_rounds(ab, list(list(races = 3, advance = 2, fastest_losers = 2),
                                list(races = 1)), n_sims = 4000, seed = 2)
  expect_equal(sum(r$p_gold), 1, tolerance = 0.02)
  expect_equal(sum(r$p_medal), 3, tolerance = 0.05)
  expect_true(all(r$p_gold <= r$p_medal + 1e-9))
  expect_true(all(r$p_medal <= r$p_final + 1e-9))
  # 3 races x 2 automatic + 2 fastest losers = 8 finalists.
  expect_equal(sum(r$p_final), 8, tolerance = 0.05)
})

test_that("a stronger athlete is likelier to reach the final", {
  ab <- make_field(24)
  r <- simulate_rounds(ab, list(list(races = 3, advance = 2, fastest_losers = 2),
                                list(races = 1)), n_sims = 4000, seed = 3)
  expect_gt(r[athlete_id == "1"]$p_final, r[athlete_id == "24"]$p_final)
  expect_gt(stats::cor(r$p_final, -as.integer(r$athlete_id)), 0.8)
})

test_that("form is carried across rounds, not redrawn", {
  # The load-bearing modelling choice. If ability_se and form_sd were redrawn
  # each round, a bad heat would be forgotten by the final and qualification
  # would be near-independent of finishing. Carrying them makes reaching the
  # final and winning it correlate strongly.
  ab <- make_field(24)
  r <- simulate_rounds(ab, list(list(races = 3, advance = 2, fastest_losers = 2),
                                list(races = 1)), n_sims = 6000, seed = 4)
  reached <- r[p_final > 0.05]
  expect_gt(stats::cor(reached$p_final, reached$p_gold), 0.7)
})

test_that("a single-round structure matches simulate_event", {
  ab <- make_field(8)
  a <- simulate_rounds(ab, list(list(races = 1)), n_sims = 8000, seed = 5)
  b <- medal_probs(simulate_event(ab, n_sims = 8000, foul_prob = 0, seed = 5))
  m <- merge(a[, .(athlete_id, p_gold)], b[, .(athlete_id, pg2 = p_gold)],
             by = "athlete_id")
  expect_lt(max(abs(m$p_gold - m$pg2)), 0.05)
})

test_that("three rounds run and stay coherent", {
  ab <- make_field(48)
  r <- simulate_rounds(ab, list(list(races = 6, advance = 3, fastest_losers = 6),
                                list(races = 3, advance = 2, fastest_losers = 2),
                                list(races = 1)), n_sims = 3000, seed = 6)
  expect_true(all(c("p_reach_r2", "p_reach_r3") %in% names(r)))
  expect_true(all(r$p_reach_r3 <= r$p_reach_r2 + 1e-9))
  expect_equal(sum(r$p_gold), 1, tolerance = 0.03)
  expect_equal(sum(r$p_final), 8, tolerance = 0.1)
})
