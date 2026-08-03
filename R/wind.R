#' Estimate the effect of wind on marks
#'
#' Wind is measured and recorded for sprints, hurdles and horizontal jumps, and
#' is currently absorbed into `sigma` as though it were random noise. It is not
#' noise — it is an observed covariate, and a large one: measured within-athlete
#' on men's 100m marks, a legal −2 to +2 m/s swing moves performance by about
#' three quarters of the athlete's entire race-to-race variability.
#'
#' Coefficients are estimated per event, not per family, because the effect
#' differs sharply within a family — wind matters enormously over 100m, much
#' less over 200m where half the race runs the bend, and not at all over 400m.
#' Events without wind readings simply get no coefficient.
#'
#' Estimation is within-athlete: each mark is centred on its own athlete-event
#' mean before regressing on wind, so the coefficient cannot absorb the fact
#' that better athletes race in different conditions.
#'
#' @param results Canonical results with `wind` and `perf`.
#' @param min_n Minimum wind-carrying marks for an event to be fitted.
#' @param max_wind Ignore readings beyond this magnitude; extreme values are
#'   usually recording errors and would lever the slope.
#' @return A `data.table` with `event_id`, `beta` (log-units per m/s), `n` and
#'   `r2`.
#' @seealso [adjust_wind()]
#' @export
fit_wind_effect <- function(results, min_n = 200L, max_wind = 6) {
  dt <- data.table::as.data.table(results)
  need <- c("wind", "perf", "event_id", "athlete_id")
  if (!all(need %in% names(dt))) {
    return(data.table::data.table(event_id = character(), beta = numeric(),
                                  n = integer(), r2 = numeric()))
  }
  dt <- dt[!is.na(wind) & !is.na(perf) & !is.na(event_id) & abs(wind) <= max_wind]
  if (!nrow(dt)) {
    return(data.table::data.table(event_id = character(), beta = numeric(),
                                  n = integer(), r2 = numeric()))
  }

  dt[, athlete_id := as.character(athlete_id)]
  # Centre within athlete-event: removes ability, so the slope is the effect of
  # wind on the *same* athlete rather than a comparison across athletes.
  dt[, dev := perf - mean(perf), by = .(athlete_id, event_id)]
  # BOTH sides must be centred within athlete-event, not just the outcome.
  # Regressing a within-centred `dev` on a RAW `wind` is not the within
  # estimator: with y_it = a_i + b*x_it + e_it it recovers
  # b * Var(x - xbar_i) / Var(x), attenuated by however much athletes differ in
  # mean wind exposure -- and they do, because athletes cluster by home circuit
  # and venues have systematically different wind climates. fit_numeric_effect()
  # in context.R already centres both; this did not.
  dt[, wdev := wind - mean(wind), by = .(athlete_id, event_id)]

  out <- dt[, {
    if (.N < min_n || stats::sd(wdev) < 0.2) {
      .(beta = NA_real_, n = .N, r2 = NA_real_)
    } else {
      f <- stats::lm(dev ~ wdev)
      .(beta = unname(stats::coef(f)[2]), n = .N,
        r2 = summary(f)$r.squared)
    }
  }, by = event_id]

  out[is.finite(beta)][]
}


#' Adjust marks to still-air equivalent
#'
#' Removes the fitted wind effect, so a mark run into a headwind and one run
#' with a tailwind become comparable. Two things follow:
#'
#' Ability estimates sharpen, because variation that was previously charged to
#' the athlete is attributed to the conditions instead.
#'
#' Wind-aided marks become usable. Marks over +2.0 m/s are ineligible for
#' records and are normally discarded, but once adjusted they carry real
#' information about ability. On a championship harvest this recovers a
#' meaningful share of sprint and jump results that would otherwise be thrown
#' away.
#'
#' @param results Canonical results with `wind` and `perf`.
#' @param wind_effect A table from [fit_wind_effect()].
#' @return `results` with `perf` adjusted and `wind_adj` recording the shift.
#'   Rows with no wind reading, or in events with no fitted coefficient, are
#'   unchanged.
#' @export
adjust_wind <- function(results, wind_effect) {
  dt <- data.table::copy(data.table::as.data.table(results))
  if (is.null(wind_effect) || !nrow(wind_effect)) {
    dt[, wind_adj := 0]
    return(dt[])
  }
  b <- wind_effect$beta[match(dt$event_id, wind_effect$event_id)]
  b[!is.finite(b)] <- 0
  w <- dt$wind
  w[!is.finite(w)] <- 0
  dt[, wind_adj := b * w]
  dt[, perf := perf - wind_adj]
  dt[]
}
