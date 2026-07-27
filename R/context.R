#' Estimate the effect of any categorical context on marks
#'
#' Generalises the pattern used for venue: centre each mark on its own
#' athlete-event mean so ability is removed, then read the average deviation per
#' level of the covariate. Suitable for lane, indoor/outdoor, track surface, or
#' anything else recorded as a category.
#'
#' Prefer this over a bespoke function per covariate. Wind is the exception —
#' it is continuous and gets a slope rather than per-level means, so it keeps
#' [fit_wind_effect()].
#'
#' **This measures association, not necessarily cause.** Several plausible
#' covariates fail on inspection because they restate something already
#' modelled: within-meet race number reproduces the round effect almost exactly,
#' and racing at home is confounded with racing at minor domestic meets. Check a
#' new covariate against what the model already contains before adopting it.
#'
#' @param results Canonical results.
#' @param covariate Column name holding the category.
#' @param by_event Fit separately per event. `TRUE` is right when the effect
#'   plausibly differs by event; `FALSE` pools, which suits covariates with few
#'   levels and thin per-event data.
#' @param min_n Minimum marks for a level to be estimated.
#' @return A `data.table` of levels, `effect` (log-units, positive = better),
#'   and `n`. Effects are centred so they add no intercept.
#' @export
fit_context_effect <- function(results, covariate, by_event = TRUE, min_n = 30L) {
  dt <- data.table::as.data.table(results)
  empty <- data.table::data.table(event_id = character(), level = character(),
                                  effect = numeric(), n = integer())
  if (!covariate %in% names(dt) ||
      !all(c("perf", "event_id", "athlete_id") %in% names(dt))) return(empty)

  dt <- dt[!is.na(perf) & !is.na(event_id) & !is.na(get(covariate))]
  if (!nrow(dt)) return(empty)

  dt[, athlete_id := as.character(athlete_id)]
  dt[, level := as.character(get(covariate))]

  # Alternating fit, NOT a single within-athlete centring. Centring once
  # attenuates the effect whenever exposure is unbalanced: an athlete racing
  # fraction p on level A has p*e_A already inside their own mean, so their
  # deviation carries only (1-p) of the true effect. Averaging over a field
  # where strong athletes favour one level shrinks the estimate badly - on a
  # planted 0.010 with an 85/15 split it recovered about 0.005.
  grp <- if (by_event) c("event_id", "level") else "level"
  dt[, lev_eff := 0]
  for (i in 1:50) {
    dt[, ath_eff := mean(perf - lev_eff), by = .(athlete_id, event_id)]
    new <- dt[, .(le = mean(perf - ath_eff)), by = grp]
    dt <- merge(dt, new, by = grp, all.x = TRUE, sort = FALSE)
    delta <- max(abs(dt$le - dt$lev_eff), na.rm = TRUE)
    dt[, lev_eff := le][, le := NULL]
    if (is.finite(delta) && delta < 1e-9) break
  }

  out <- dt[, .(effect = data.table::first(lev_eff), n = .N), by = grp][n >= min_n]
  if (!nrow(out)) return(empty)

  if (by_event) {
    out[, effect := effect - stats::weighted.mean(effect, n), by = event_id]
  } else {
    out[, effect := effect - stats::weighted.mean(effect, n)]
    out[, event_id := NA_character_]
  }
  data.table::setcolorder(out, c("event_id", "level", "effect", "n"))
  out[]
}


#' Estimate the effect of a continuous covariate on marks
#'
#' The continuous counterpart to [fit_context_effect()]: fits a slope rather
#' than per-level means. [fit_wind_effect()] is a specialisation of this for
#' wind; use this for anything else measured on a scale, such as swimming
#' reaction time.
#'
#' Fitted per event by default, because the same covariate can matter very
#' differently across events — wind moves a 100m far more than a 200m, where
#' half the race is on the bend.
#'
#' **Note on predictive use.** Some covariates are only known *after* the race.
#' Reaction time is the clear case: you cannot know a future one. Such covariates
#' still earn their place by cleaning historical marks — a swimmer who botched
#' one start should not carry that in their ability estimate — but they cannot
#' be supplied for a race yet to happen. The athlete's typical value is already
#' inside their ability.
#'
#' @param results Canonical results.
#' @param covariate Numeric column name.
#' @param by_event Fit a separate slope per event.
#' @param min_n Minimum marks for a slope to be fitted.
#' @param trim Ignore values beyond this many SDs from the covariate mean;
#'   extreme values are usually recording errors and would lever the slope.
#' @return A `data.table` of `event_id`, `beta`, `n` and `r2`.
#' @export
fit_numeric_effect <- function(results, covariate, by_event = TRUE,
                               min_n = 200L, trim = 5) {
  dt <- data.table::as.data.table(results)
  empty <- data.table::data.table(event_id = character(), beta = numeric(),
                                  n = integer(), r2 = numeric())
  if (!covariate %in% names(dt) ||
      !all(c("perf", "event_id", "athlete_id") %in% names(dt))) return(empty)

  dt[, xv := suppressWarnings(as.numeric(get(covariate)))]
  dt <- dt[!is.na(perf) & !is.na(event_id) & !is.na(xv)]
  if (!nrow(dt)) return(empty)

  mu <- mean(dt$xv); s <- stats::sd(dt$xv)
  if (is.finite(s) && s > 0) dt <- dt[abs(xv - mu) <= trim * s]
  if (!nrow(dt)) return(empty)

  dt[, athlete_id := as.character(athlete_id)]
  # Within athlete-event: the slope is the effect on the *same* athlete, so it
  # cannot absorb the fact that better athletes differ systematically on the
  # covariate (faster swimmers also tend to have faster starts).
  dt[, dev := perf - mean(perf), by = .(athlete_id, event_id)]
  dt[, xdev := xv - mean(xv), by = .(athlete_id, event_id)]

  grp <- if (by_event) "event_id" else character(0)
  out <- dt[, {
    if (.N < min_n || stats::sd(xdev) <= 0) {
      .(beta = NA_real_, n = .N, r2 = NA_real_)
    } else {
      f <- stats::lm(dev ~ xdev)
      .(beta = unname(stats::coef(f)[2]), n = .N, r2 = summary(f)$r.squared)
    }
  }, by = grp]
  if (!by_event) out[, event_id := NA_character_]
  out[is.finite(beta)][]
}


#' Remove a fitted continuous covariate effect from marks
#'
#' @param results Canonical results.
#' @param numeric_effect A table from [fit_numeric_effect()].
#' @param covariate Numeric column name; must match the fit.
#' @return `results` with `perf` adjusted, relative to each athlete-event's own
#'   mean covariate value, and `numeric_adj` recording the shift.
#' @export
adjust_numeric <- function(results, numeric_effect, covariate) {
  dt <- data.table::copy(data.table::as.data.table(results))
  if (is.null(numeric_effect) || !nrow(numeric_effect) || !covariate %in% names(dt)) {
    dt[, numeric_adj := 0]
    return(dt[])
  }
  dt[, xv := suppressWarnings(as.numeric(get(covariate)))]
  dt[, athlete_id := as.character(athlete_id)]
  # Adjust relative to the athlete's own typical value: their average start is
  # part of who they are and belongs in ability, only the deviation is context.
  dt[, xdev := xv - mean(xv, na.rm = TRUE), by = .(athlete_id, event_id)]

  pooled <- all(is.na(numeric_effect$event_id))
  b <- if (pooled) rep(numeric_effect$beta[1], nrow(dt)) else
    numeric_effect$beta[match(dt$event_id, numeric_effect$event_id)]
  b[!is.finite(b)] <- 0
  x <- dt$xdev
  x[!is.finite(x)] <- 0

  dt[, numeric_adj := b * x]
  dt[, perf := perf - numeric_adj]
  dt[, c("xv", "xdev") := NULL]
  dt[]
}


#' Remove a fitted context effect from marks
#'
#' @param results Canonical results.
#' @param context_effect A table from [fit_context_effect()].
#' @param covariate Column name; must match the fit.
#' @return `results` with `perf` adjusted and `context_adj` recording the shift.
#'   Levels with no estimate are unchanged.
#' @export
adjust_context <- function(results, context_effect, covariate) {
  dt <- data.table::copy(data.table::as.data.table(results))
  if (is.null(context_effect) || !nrow(context_effect) || !covariate %in% names(dt)) {
    dt[, context_adj := 0]
    return(dt[])
  }
  pooled <- all(is.na(context_effect$event_id))
  key <- if (pooled) as.character(dt[[covariate]]) else
    paste(dt$event_id, as.character(dt[[covariate]]))
  ck <- if (pooled) context_effect$level else
    paste(context_effect$event_id, context_effect$level)

  adj <- context_effect$effect[match(key, ck)]
  adj[!is.finite(adj)] <- 0
  dt[, context_adj := adj]
  dt[, perf := perf - context_adj]
  dt[]
}
