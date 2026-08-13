make_ability <- function(times = c(9.80, 9.90, 10.00, 10.10), sigma = 0.008,
                         event_id = "AT-100Metres-M") {
  data.table::data.table(
    athlete_id = letters[seq_along(times)],
    event_id = event_id,
    ability = to_perf(times, -1L),
    sigma = sigma
  )
}

# --- simulate_event() guards -------------------------------------------------

test_that("df = 2 aborts", {
  expect_error(simulate_event(make_ability(), n_sims = 100, df = 2),
               "must be a finite number greater than 2")
})

test_that("df = NA aborts", {
  expect_error(simulate_event(make_ability(), n_sims = 100, df = NA_real_),
               "must be a finite number greater than 2")
})

test_that("round_class is no longer a parameter and errors as unused", {
  expect_error(
    simulate_event(make_ability(), n_sims = 100, round_class = "heat"),
    "unused argument"
  )
})

test_that("a valid small sim with df = 5 returns a finite perf matrix and permutation ranks", {
  sim <- simulate_event(make_ability(), n_sims = 500, df = 5, foul_prob = 0, seed = 1)
  expect_true(all(is.finite(sim$perf)))
  n_ath <- ncol(sim$rank)
  expect_true(all(apply(sim$rank, 1L, function(r) identical(unname(sort(r)), seq_len(n_ath)))))
})

# --- score_predictions() -----------------------------------------------------

test_that("score_predictions returns an object classed citius_score", {
  d <- data.table::data.table(race_id = "r1", athlete_id = letters[1:4],
                              p_gold = c(0.4, 0.3, 0.2, 0.1))
  o <- data.table::data.table(race_id = "r1", athlete_id = letters[1:4],
                              hit = c(TRUE, FALSE, FALSE, FALSE))
  s <- score_predictions(d, o)
  expect_s3_class(s, "citius_score")
})

test_that("the print method runs without error", {
  d <- data.table::data.table(race_id = "r1", athlete_id = letters[1:4],
                              p_gold = c(0.4, 0.3, 0.2, 0.1))
  o <- data.table::data.table(race_id = "r1", athlete_id = letters[1:4],
                              hit = c(TRUE, FALSE, FALSE, FALSE))
  s <- score_predictions(d, o)
  expect_no_error(print(s))
})

test_that("outcomes covering only a subset of the field warn and the baseline keeps the full field size", {
  # 1 race, 4 predicted athletes; outcomes given for only 2, including the
  # winner. brier_baseline must be computed from the predictions-side field
  # size (4), not the matched-row count (2).
  preds <- data.table::data.table(
    race_id = "r1", athlete_id = c("a", "b", "c", "d"),
    p_gold = c(0.4, 0.3, 0.2, 0.1))
  outs <- data.table::data.table(
    race_id = "r1", athlete_id = c("a", "b"),
    hit = c(TRUE, FALSE))
  expect_warning(s <- score_predictions(preds, outs), "no matching outcome")

  matched <- merge(preds, outs, by = c("race_id", "athlete_id"))
  expected_baseline <- mean((1 / 4 - matched$hit)^2)
  expect_equal(s$overall$brier_baseline, expected_baseline, tolerance = 1e-9)
})

# --- .se_parse_date() ---------------------------------------------------------

test_that(".se_parse_date rolls two-digit years that would parse as future years back a century", {
  d <- citius:::.se_parse_date(c("05/06/65", "05/06/99", "05/06/24"))
  expect_equal(d[1], as.Date("1965-06-05"))
  expect_equal(d[2], as.Date("1999-06-05"))
  expect_equal(d[3], as.Date("2024-06-05"))
})
