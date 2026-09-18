#' Load the exported conditions-model parameters for an event
#'
#' Each event's `gamm4` conditions fit (wind, altitude, indoor, plus the
#' athlete / race-shock / residual variance components) is exported once per
#' refit to a small JSON file by `citiusdata/scripts/export_conditions_params.R`.
#' The curves are the fitted smooths evaluated on a grid, relative to the
#' reference point (wind 0, altitude 0, outdoor), in perf-log units. Nothing
#' here needs a model runtime: [adjust_conditions()] is linear interpolation
#' and [race_shock()] is one line of arithmetic, so the same numbers can be
#' applied live (R or JS) as in a batch rebuild.
#'
#' @param event_id Event id, e.g. `"AT-100Metres-M"`.
#' @param dir Directory of `<event_id>.json` parameter files.
#' @return A list, or `NULL` when the event has no fitted model.
#' @export
conditions_params <- function(event_id, dir) {
  f <- file.path(dir, paste0(event_id, ".json"))
  if (!file.exists(f)) return(NULL)
  jsonlite::fromJSON(f)
}

#' Wind, altitude and indoor corrections for a set of marks
#'
#' Applies an event's exported conditions curves. Every correction is the
#' fitted effect of that condition relative to still air, sea level, outdoors,
#' in perf-log units (positive = the condition helped), so
#' `perf - wind_adj - venue_adj - indoor_adj` is what the athlete would have
#' produced without it. Values beyond the fitted grid are clamped to its ends
#' rather than extrapolated. Missing wind / altitude give a zero correction.
#'
#' `venue_adj` is the altitude curve plus, when the event's parameters carry a
#' `venues` lookup and the venue is in it, that venue's offset -- a course or a
#' track, measured 2026-09-19 at 1.36% sd on the marathon and 0.50% on the
#' 100m, more than a race's shock. An unknown venue gets the altitude curve
#' only. `alt_adj` and `venue_off` are returned separately as well.
#'
#' @param params Output of [conditions_params()].
#' @param wind Wind reading in m/s (`NA` where not measured).
#' @param alt_m Venue altitude in metres.
#' @param indoor Logical, `TRUE` for an indoor mark.
#' @param venue Venue name as the corpus spells it (`venue_city`), or `NULL`.
#' @return A `data.table` with `wind_adj`, `venue_adj`, `indoor_adj`,
#'   `alt_adj`, `venue_off`.
#' @export
adjust_conditions <- function(params, wind = NA_real_, alt_m = NA_real_, indoor = FALSE, venue = NULL) {
  n <- max(length(wind), length(alt_m), length(indoor), length(venue))
  wind <- rep_len(wind, n); alt_m <- rep_len(alt_m, n); indoor <- rep_len(indoor, n)
  interp <- function(grid, curve, x) {
    out <- numeric(length(x)); ok <- is.finite(x)
    if (any(ok)) out[ok] <- stats::approx(grid, curve, xout = pmin(pmax(x[ok], min(grid)), max(grid)), rule = 2)$y
    out
  }
  wind_adj <- if (!is.null(params$wind)) interp(params$wind$grid, params$wind$curve, wind) else numeric(n)
  alt_adj <- interp(params$altitude$grid_m, params$altitude$curve, pmax(alt_m, 0))
  venue_off <- numeric(n)
  if (!is.null(venue) && length(params$venues)) {
    venue <- rep_len(as.character(venue), n)
    hit <- !is.na(venue) & venue %in% names(params$venues)
    if (any(hit)) venue_off[hit] <- as.numeric(unlist(params$venues[venue[hit]]))
  }
  indoor_adj <- if (isTRUE(params$has_indoor)) ifelse(indoor %in% TRUE, params$indoor_coef, 0) else numeric(n)
  data.table::data.table(wind_adj = wind_adj, venue_adj = alt_adj + venue_off, indoor_adj = indoor_adj,
                         alt_adj = alt_adj, venue_off = venue_off)
}

#' Race shock: the shrunk field-mean surprise
#'
#' What the whole field shared on the day after wind, altitude and indoor are
#' removed and each athlete's own expected level is subtracted. It is the BLUP
#' of a one-way random effect with athlete levels treated as known:
#' `mean(resid) * n * var_race / (n * var_race + var_resid)`, so a small or
#' noisy field is pulled towards zero. Verified against `lme4`'s race BLUP on
#' the men's 100m fitting sample: correlation 1.0000, RMSE 3e-6.
#'
#' Residuals are winsorised at `cap_sd` residual standard deviations first.
#' Without that, one athlete who pulls up (12.47 against an expected 10.55 in a
#' real 100m heat) reads as a "slow race" and credits the rest of the field
#' 0.18s they did not earn. The cap keeps the shrinkage arithmetic intact and
#' bounds any single athlete's leverage.
#'
#' @param resid Cleaned perf minus expected perf, one per athlete in the race.
#'   `NA` (no expectation, e.g. a debutant) is excluded from the mean.
#' @param var_race,var_resid Race-shock and residual variances from
#'   [conditions_params()]. `var_resid` should be the variance of the
#'   expectation actually used — forecast error for a live forecast.
#' @param cap_sd Winsorising cap in residual standard deviations.
#' @return A single number in perf-log units (positive = fast race); `0` when
#'   no athlete has an expectation.
#' @export
race_shock <- function(resid, var_race, var_resid, cap_sd = 3) {
  r <- resid[is.finite(resid)]
  n <- length(r)
  if (n == 0L || !is.finite(var_race) || !is.finite(var_resid)) return(0)
  cap <- cap_sd * sqrt(var_resid)
  r <- pmin(pmax(r, -cap), cap)
  mean(r) * n * var_race / (n * var_race + var_resid)
}

#' Leave-one-out race shock, one value per athlete
#'
#' [race_shock()] computed for each athlete from the OTHER athletes' residuals
#' only. An athlete's own surprise is then never evidence that the day was
#' fast: a lone runner's PB stays a PB, and the within-athlete scatter
#' reduction it reports is honest rather than partly self-referential. This is
#' the version the adjusted-marks table uses.
#'
#' @inheritParams race_shock
#' @return A numeric vector the length of `resid` (0 for an athlete with no
#'   other athlete to learn from).
#' @export
race_shock_loo <- function(resid, var_race, var_resid, cap_sd = 3) {
  n_all <- length(resid)
  if (!is.finite(var_race) || !is.finite(var_resid)) return(numeric(n_all))
  ok <- is.finite(resid)
  cap <- cap_sd * sqrt(var_resid)
  r <- ifelse(ok, pmin(pmax(resid, -cap), cap), 0)
  n <- sum(ok); s <- sum(r)
  n_i <- n - ok                     # others with an expectation
  s_i <- s - r                      # their residual sum
  out <- ifelse(n_i > 0L, (s_i / pmax(n_i, 1L)) * n_i * var_race / (n_i * var_race + var_resid), 0)
  out
}
