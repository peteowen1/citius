#' Score probabilistic predictions against outcomes
#'
#' Reports the two things that matter for a probability forecast — whether it is
#' *sharp* (confident) and whether it is *calibrated* (right as often as it
#' claims) — plus a skill comparison against the only defensible baseline.
#'
#' The baseline is deliberately uniform-within-race (`1/field_size`), not a
#' global constant. A model that beats "everyone equally likely" has learned
#' something about who is fast; one that does not has learned nothing, no matter
#' how good its Brier score looks in absolute terms. Absolute Brier scores are
#' close to meaningless on their own here, because a race with 30 entrants
#' scores far better than one with 8 purely from having lower base rates.
#'
#' Report the **pooled** figures. Brier is a proper scoring rule and is meant to
#' be aggregated; summarising `by_race$skill` as a median is not a fair summary,
#' because the penalty for confidence is asymmetric. A model that knows the true
#' probabilities exactly still loses to the uniform baseline on roughly a third
#' of individual races.
#'
#' Scoring is per **race** — one running of a phase, e.g. the Paris 2024 men's
#' 100m final — not per event type. See `DICTIONARY.md`. The key is `race_id`
#' precisely so this cannot be confused again: an earlier version keyed on
#' `event_id` while being passed a meet-plus-event composite, which made a
#' report of 50 individual finals read as 50 event types.
#'
#' @param predictions A `data.table` with `race_id`, `athlete_id` and a
#'   probability column.
#' @param outcomes A `data.table` with `race_id`, `athlete_id` and a logical
#'   `hit` marking what actually happened.
#' @param prob_col Name of the probability column.
#' @return A list with `overall` (Brier, log loss, skill against baseline),
#'   `by_race`, and `reliability` bins.
#' @seealso [reliability_table()]
#' @export
score_predictions <- function(predictions, outcomes, prob_col = "p_gold") {
  p <- data.table::as.data.table(predictions)
  o <- data.table::as.data.table(outcomes)
  if (!prob_col %in% names(p)) {
    cli::cli_abort("{.arg predictions} has no column {.field {prob_col}}.")
  }
  for (nm in c("race_id")) {
    if (!nm %in% names(p) || !nm %in% names(o)) {
      cli::cli_abort(c(
        "{.arg predictions} and {.arg outcomes} must both have {.field race_id}.",
        i = "One race is one running of a phase - see {.file DICTIONARY.md}."
      ))
    }
  }

  p[, athlete_id := as.character(athlete_id)]
  o[, athlete_id := as.character(athlete_id)]
  d <- merge(p[, .(race_id, athlete_id, prob = get(prob_col))],
             o[, .(race_id, athlete_id, hit)],
             by = c("race_id", "athlete_id"))
  if (!nrow(d)) {
    cli::cli_abort("No predictions matched an outcome; check {.field athlete_id} keys.")
  }
  # The 1/field baseline was computed from the MATCHED rows, so an outcomes
  # table covering only part of a field silently shrank it and moved the skill
  # figure -- outcomes holding only winners made base = 1/1. Field size now
  # comes from the predictions side, counted before the inner join, and rows
  # lost to the join are loud rather than silent.
  if (nrow(d) < nrow(p)) {
    cli::cli_warn(c(
      "{nrow(p) - nrow(d)} of {nrow(p)} prediction{?s} had no matching outcome row and were dropped.",
      i = "Brier and log loss score only the matched rows; the baseline keeps the full predicted field size."
    ))
  }

  d[, hit := as.integer(hit)]
  # Field size for the 1/field baseline: the LARGER of the predicted and
  # outcome-side counts per race. Predictions-only undercounts when the model
  # forecast a subset of the true field (outcomes carry entrants nobody
  # predicted); outcomes-only undercounts in the mirror case the warning above
  # covers. Either subset silently flatters or sandbags skill; max of the two
  # is the best available estimate of the true field, and disagreement is loud.
  pf <- p[, .(field_p = .N), by = race_id]
  of <- o[, .(field_o = .N), by = race_id]
  fld <- merge(pf, of, by = "race_id", all.x = TRUE)
  fld[is.na(field_o), field_o := 0L]
  if (any(fld$field_o > fld$field_p)) {
    n_bigger <- sum(fld$field_o > fld$field_p)
    cli::cli_warn(c(
      "Outcomes list more athletes than predictions in {n_bigger} race{?s}.",
      i = "The baseline uses the outcome-side field size there; predictions cover only part of the field."
    ))
  }
  fld[, field := pmax(field_p, field_o)]
  d[fld, on = "race_id", field := i.field]
  d[, base := 1 / field]

  eps <- 1e-15
  d[, brier := (prob - hit)^2]
  d[, brier_base := (base - hit)^2]
  d[, logloss := -(hit * log(pmax(prob, eps)) + (1 - hit) * log(pmax(1 - prob, eps)))]
  d[, logloss_base := -(hit * log(pmax(base, eps)) + (1 - hit) * log(pmax(1 - base, eps)))]

  by_race <- d[, .(field = data.table::first(field),
                   brier = mean(brier), brier_base = mean(brier_base),
                   hits = sum(hit)), by = race_id]
  by_race[, skill := 1 - brier / brier_base]

  overall <- list(
    n_predictions = nrow(d),
    n_races = data.table::uniqueN(d$race_id),
    brier = mean(d$brier),
    brier_baseline = mean(d$brier_base),
    brier_skill = 1 - mean(d$brier) / mean(d$brier_base),
    logloss = mean(d$logloss),
    logloss_baseline = mean(d$logloss_base),
    mean_prob = mean(d$prob),
    observed_rate = mean(d$hit)
  )

  # Classed, so print.citius_score actually dispatches -- it existed for a
  # plain list nothing ever tagged, i.e. dead code from day one.
  structure(
    list(overall = overall, by_race = by_race[], reliability = reliability_table(d)),
    class = "citius_score"
  )
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
  cli::cli_text("{o$n_predictions} prediction{?s} across {o$n_races} race{?s}")
  cli::cli_text("Brier {signif(o$brier, 4)} vs baseline {signif(o$brier_baseline, 4)} (skill {signif(o$brier_skill, 3)})")
  cli::cli_text("Log loss {signif(o$logloss, 4)} vs baseline {signif(o$logloss_baseline, 4)}")
  invisible(x)
}
