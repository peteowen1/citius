make_case <- function(n_races = 40, field = 8, sharpness = 1, seed = 3) {
  set.seed(seed)
  data.table::rbindlist(lapply(seq_len(n_races), function(e) {
    strength <- sort(stats::runif(field), decreasing = TRUE)
    p <- strength^sharpness
    p <- p / sum(p)
    winner <- sample(field, 1, prob = p)
    data.table::data.table(
      race_id = paste0("E", e),
      athlete_id = paste0("E", e, "_a", seq_len(field)),
      prob = p,
      hit = seq_len(field) == winner)
  }))
}

test_that("a perfectly calibrated forecast beats the uniform baseline", {
  d <- make_case()
  s <- score_predictions(d[, .(race_id, athlete_id, p_gold = prob)],
                         d[, .(race_id, athlete_id, hit)])
  expect_gt(s$overall$brier_skill, 0)
  expect_lt(s$overall$brier, s$overall$brier_baseline)
})

test_that("a uniform forecast scores zero skill against the baseline", {
  d <- make_case()
  d[, prob := 1 / .N, by = race_id]
  s <- score_predictions(d[, .(race_id, athlete_id, p_gold = prob)],
                         d[, .(race_id, athlete_id, hit)])
  expect_equal(s$overall$brier_skill, 0, tolerance = 1e-8)
})

test_that("a confidently wrong forecast scores negative skill", {
  d <- make_case()
  # Reverse the probabilities: back the weakest athlete every time
  d[, prob := rev(prob), by = race_id]
  s <- score_predictions(d[, .(race_id, athlete_id, p_gold = prob)],
                         d[, .(race_id, athlete_id, hit)])
  expect_lt(s$overall$brier_skill, 0)
})

test_that("probabilities sum sensibly and the observed rate matches", {
  d <- make_case()
  s <- score_predictions(d[, .(race_id, athlete_id, p_gold = prob)],
                         d[, .(race_id, athlete_id, hit)])
  # One winner per race, so the observed rate is 1/field
  expect_equal(s$overall$observed_rate, 1 / 8, tolerance = 1e-8)
  expect_equal(s$overall$mean_prob, 1 / 8, tolerance = 1e-8)
})

test_that("reliability bins recover systematic overconfidence", {
  d <- make_case(n_races = 300)
  # Inflate every probability toward 1: claimed rates should exceed observed
  d[, prob := pmin(prob * 2.5, 0.99)]
  s <- score_predictions(d[, .(race_id, athlete_id, p_gold = prob)],
                         d[, .(race_id, athlete_id, hit)])
  r <- s$reliability[n >= 20]
  expect_true(mean(r$gap) < 0)     # observed below predicted
})

test_that("reliability of an honest forecast is near the diagonal", {
  d <- make_case(n_races = 400)
  s <- score_predictions(d[, .(race_id, athlete_id, p_gold = prob)],
                         d[, .(race_id, athlete_id, hit)])
  r <- s$reliability[n >= 30]
  expect_lt(max(abs(r$gap)), 0.15)
})

test_that("per-race skill is reported", {
  d <- make_case(n_races = 5)
  s <- score_predictions(d[, .(race_id, athlete_id, p_gold = prob)],
                         d[, .(race_id, athlete_id, hit)])
  expect_equal(nrow(s$by_race), 5)
  expect_true(all(c("brier", "skill", "field") %in% names(s$by_race)))
})

test_that("scoring requires race_id, not event_id", {
  # Guards the naming defect this dictionary exists to prevent: an earlier
  # version keyed on event_id while being handed a meet+event composite.
  d <- make_case(n_races = 3)
  bad_p <- data.table::copy(d)[, .(event_id = race_id, athlete_id, p_gold = prob)]
  bad_o <- data.table::copy(d)[, .(event_id = race_id, athlete_id, hit)]
  expect_error(score_predictions(bad_p, bad_o), "race_id")
})

test_that("scoring fails loudly when nothing matches", {
  d <- make_case(n_races = 2)
  o <- d[, .(race_id, athlete_id = paste0("other_", athlete_id), hit)]
  expect_error(score_predictions(d[, .(race_id, athlete_id, p_gold = prob)], o),
               "No predictions matched")
})

test_that("a missing probability column is an error, not a silent NA", {
  d <- make_case(n_races = 2)
  expect_error(
    score_predictions(d[, .(race_id, athlete_id)], d[, .(race_id, athlete_id, hit)]),
    "no column"
  )
})
