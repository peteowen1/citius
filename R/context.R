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
