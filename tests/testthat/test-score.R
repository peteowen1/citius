make_case <- function(n_events = 40, field = 8, sharpness = 1, seed = 3) {
  set.seed(seed)
  data.table::rbindlist(lapply(seq_len(n_events), function(e) {
    strength <- sort(stats::runif(field), decreasing = TRUE)
    p <- strength^sharpness
    p <- p / sum(p)
    winner <- sample(field, 1, prob = p)
    data.table::data.table(
      event_id = paste0("E", e),
      athlete_id = paste0("E", e, "_a", seq_len(field)),
      prob = p,
      hit = seq_len(field) == winner)
  }))
}

test_that("a perfectly calibrated forecast beats the uniform baseline", {
  d <- make_case()
  s <- score_predictions(d[, .(event_id, athlete_id, p_gold = prob)],
                         d[, .(event_id, athlete_id, hit)])
  expect_gt(s$overall$brier_skill, 0)
  expect_lt(s$overall$brier, s$overall$brier_baseline)
})

test_that("a uniform forecast scores zero skill against the baseline", {
  d <- make_case()
  d[, prob := 1 / .N, by = event_id]
  s <- score_predictions(d[, .(event_id, athlete_id, p_gold = prob)],
                         d[, .(event_id, athlete_id, hit)])
  expect_equal(s$overall$brier_skill, 0, tolerance = 1e-8)
})

test_that("a confidently wrong forecast scores negative skill", {
  d <- make_case()
  # Reverse the probabilities: back the weakest athlete every time
  d[, prob := rev(prob), by = event_id]
  s <- score_predictions(d[, .(event_id, athlete_id, p_gold = prob)],
                         d[, .(event_id, athlete_id, hit)])
  expect_lt(s$overall$brier_skill, 0)
})

test_that("probabilities sum sensibly and the observed rate matches", {
  d <- make_case()
  s <- score_predictions(d[, .(event_id, athlete_id, p_gold = prob)],
                         d[, .(event_id, athlete_id, hit)])
  # One winner per event, so the observed rate is 1/field
  expect_equal(s$overall$observed_rate, 1 / 8, tolerance = 1e-8)
  expect_equal(s$overall$mean_prob, 1 / 8, tolerance = 1e-8)
})

test_that("reliability bins recover systematic overconfidence", {
  d <- make_case(n_events = 300)
  # Inflate every probability toward 1: claimed rates should exceed observed
  d[, prob := pmin(prob * 2.5, 0.99)]
  s <- score_predictions(d[, .(event_id, athlete_id, p_gold = prob)],
                         d[, .(event_id, athlete_id, hit)])
  r <- s$reliability[n >= 20]
  expect_true(mean(r$gap) < 0)     # observed below predicted
})

test_that("reliability of an honest forecast is near the diagonal", {
  d <- make_case(n_events = 400)
  s <- score_predictions(d[, .(event_id, athlete_id, p_gold = prob)],
                         d[, .(event_id, athlete_id, hit)])
  r <- s$reliability[n >= 30]
  expect_lt(max(abs(r$gap)), 0.15)
})

test_that("per-event skill is reported", {
  d <- make_case(n_events = 5)
  s <- score_predictions(d[, .(event_id, athlete_id, p_gold = prob)],
                         d[, .(event_id, athlete_id, hit)])
  expect_equal(nrow(s$by_event), 5)
  expect_true(all(c("brier", "skill", "field") %in% names(s$by_event)))
})

test_that("scoring fails loudly when nothing matches", {
  d <- make_case(n_events = 2)
  o <- d[, .(event_id, athlete_id = paste0("other_", athlete_id), hit)]
  expect_error(score_predictions(d[, .(event_id, athlete_id, p_gold = prob)], o),
               "No predictions matched")
})

test_that("a missing probability column is an error, not a silent NA", {
  d <- make_case(n_events = 2)
  expect_error(
    score_predictions(d[, .(event_id, athlete_id)], d[, .(event_id, athlete_id, hit)]),
    "no column"
  )
})
