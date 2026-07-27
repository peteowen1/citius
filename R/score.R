#' Score probabilistic predictions against outcomes
#'
#' Reports the two things that matter for a probability forecast — whether it is
#' *sharp* (confident) and whether it is *calibrated* (right as often as it
#' claims) — plus a skill comparison against the only defensible baseline.
#'
#' The baseline is deliberately uniform-within-event (`1/field_size`), not a
#' global constant. A model that beats "everyone equally likely" has learned
#' something about who is fast; one that does not has learned nothing, no matter
#' how good its Brier score looks in absolute terms. Absolute Brier scores are
#' close to meaningless on their own here, because an event with 30 entrants
#' scores far better than one with 8 purely from having lower base rates.
#'
#' @param predictions A `data.table` with `event_id`, `athlete_id` and a
#'   probability column.
#' @param outcomes A `data.table` with `event_id`, `athlete_id` and a logical
#'   `hit` marking what actually happened.
#' @param prob_col Name of the probability column.
#' @return A list with `overall` (Brier, log loss, skill against baseline),
#'   `by_event`, and `reliability` bins.
#' @seealso [reliability_table()]
#' @export
score_predictions <- function(predictions, outcomes, prob_col = "p_gold") {
  p <- data.table::as.data.table(predictions)
  o <- data.table::as.data.table(outcomes)
  if (!prob_col %in% names(p)) {
    cli::cli_abort("{.arg predictions} has no column {.field {prob_col}}.")
  }

  p[, athlete_id := as.character(athlete_id)]
  o[, athlete_id := as.character(athlete_id)]
  d <- merge(p[, .(event_id, athlete_id, prob = get(prob_col))],
             o[, .(event_id, athlete_id, hit)],
             by = c("event_id", "athlete_id"))
  if (!nrow(d)) {
    cli::cli_abort("No predictions matched an outcome; check {.field athlete_id} keys.")
  }

  d[, hit := as.integer(hit)]
  d[, field := .N, by = event_id]
  d[, base := 1 / field]

  eps <- 1e-15
  d[, brier := (prob - hit)^2]
  d[, brier_base := (base - hit)^2]
  d[, logloss := -(hit * log(pmax(prob, eps)) + (1 - hit) * log(pmax(1 - prob, eps)))]
  d[, logloss_base := -(hit * log(pmax(base, eps)) + (1 - hit) * log(pmax(1 - base, eps)))]

  by_event <- d[, .(field = data.table::first(field),
                    brier = mean(brier), brier_base = mean(brier_base),
                    hits = sum(hit)), by = event_id]
  by_event[, skill := 1 - brier / brier_base]

  overall <- list(
    n_predictions = nrow(d),
    n_events = data.table::uniqueN(d$event_id),
    brier = mean(d$brier),
    brier_baseline = mean(d$brier_base),
    brier_skill = 1 - mean(d$brier) / mean(d$brier_base),
    logloss = mean(d$logloss),
    logloss_baseline = mean(d$logloss_base),
    mean_prob = mean(d$prob),
    observed_rate = mean(d$hit)
  )

  list(overall = overall, by_event = by_event[], reliability = reliability_table(d))
}


#' Bin predictions to compare claimed against observed frequency
#'
#' A calibrated forecast that says 30% is right about 30% of the time. This bins
#' predictions and reports the observed hit rate in each bin, which is the only
#' way to see *how* a model is wrong: systematic overconfidence looks completely
#' different from noise, and a Brier score alone cannot distinguish them.
#'
#' Bins are equal-width rather than equal-count so that the crowded low-
#' probability region does not dominate; sparse bins are reported with their `n`
#' so thin evidence is visible rather than hidden.
#'
#' @param d A `data.table` with `prob` and integer `hit`.
#' @param bins Number of equal-width probability bins.
#' @return A `data.table` with one row per non-empty bin.
#' @export
reliability_table <- function(d, bins = 10L) {
  d <- data.table::as.data.table(d)
  if (!all(c("prob", "hit") %in% names(d))) {
    cli::cli_abort("{.arg d} must contain {.field prob} and {.field hit}.")
  }
  edges <- seq(0, 1, length.out = bins + 1L)
  d[, bin := cut(prob, breaks = edges, include.lowest = TRUE)]
  out <- d[, .(n = .N,
               mean_predicted = mean(prob),
               observed = mean(hit)), by = bin]
  out <- out[order(bin)]
  out[, gap := observed - mean_predicted]
  out[]
}


#' @export
print.citius_score <- function(x, ...) {
  o <- x$overall
  cli::cli_h3("citius scoring")
  cli::cli_text("{o$n_predictions} prediction{?s} across {o$n_events} event{?s}")
  cli::cli_text("Brier {signif(o$brier, 4)} vs baseline {signif(o$brier_baseline, 4)} (skill {signif(o$brier_skill, 3)})")
  cli::cli_text("Log loss {signif(o$logloss, 4)} vs baseline {signif(o$logloss_baseline, 4)}")
  invisible(x)
}
