#' Estimate persistent venue effects
#'
#' Some tracks and pools are reliably fast. That is distinct from the race-level
#' shock in [calibrate()]: a race effect captures conditions on one day, but
#' cannot learn that a particular stadium has been quick for fifteen years.
#'
#' **Validated as NOT usable with `comp_name` as the key — read before adopting.**
#' On the championship harvest the fitted "venue" effects are dominated by things
#' already modelled elsewhere:
#'
#' - The fastest men's 1500m "venues" are Golden Gala, Herculis and Monaco —
#'   elite meets with professional pacemakers. The slowest are World U20, USA
#'   U20, the Youth Olympics and Polish U18. Those are not slow tracks, they are
#'   junior fields, which the aging curve and tier offsets already capture.
#' - Fitted variance shares exceed **100%** of sigma for several events, which is
#'   impossible for a real variance component and proves the estimate is
#'   absorbing other effects.
#' - Adjusting for it shrinks sigma by **0.03%** — nothing, because the signal is
#'   already accounted for.
#'
#' The function is correct and the target effect is real: surfaces and pools do
#' differ. But `comp_name` identifies a *meet*, and a meet carries its tier, its
#' field quality and its age profile. Use `venue_stadium` (captured from
#' `0ce853e` onward) once a re-harvest supplies it, and fit it jointly with tier
#' rather than alone.
#'
#' @param results Canonical results with a venue column.
#' @param venue_col Column identifying the venue. Use `venue_stadium`.
#'   `comp_name` is **not** an adequate proxy — see above.
#' @param min_n Minimum marks for a venue-event to be estimated.
#' @return A `data.table` of `event_id`, `venue`, `effect` and `n`.
#' @seealso [adjust_venue()]
#' @export
fit_venue_effect <- function(results, venue_col = "comp_name", min_n = 15L) {
  dt <- data.table::as.data.table(results)
  empty <- data.table::data.table(event_id = character(), venue = character(),
                                  effect = numeric(), n = integer())
  if (!venue_col %in% names(dt) || !all(c("perf", "event_id", "athlete_id") %in% names(dt))) {
    return(empty)
  }
  dt <- dt[!is.na(perf) & !is.na(event_id) & !is.na(get(venue_col))]
  if (!nrow(dt)) return(empty)

  dt[, athlete_id := as.character(athlete_id)]
  dt[, venue := as.character(get(venue_col))]

  # Alternating fit rather than a single centring. Venue exposure is strongly
  # unbalanced - fast athletes race disproportionately at fast meets - and
  # centring once attenuates the effect by exactly that imbalance: an athlete
  # racing fraction p at one venue carries only (1-p) of its effect in their
  # deviation. See fit_context_effect() for the derivation.
  dt[, ven_eff := 0]
  for (i in 1:50) {
    dt[, ath_eff := mean(perf - ven_eff), by = .(athlete_id, event_id)]
    new <- dt[, .(ve = mean(perf - ath_eff)), by = .(event_id, venue)]
    dt <- merge(dt, new, by = c("event_id", "venue"), all.x = TRUE, sort = FALSE)
    delta <- max(abs(dt$ve - dt$ven_eff), na.rm = TRUE)
    dt[, ven_eff := ve][, ve := NULL]
    if (is.finite(delta) && delta < 1e-9) break
  }

  out <- dt[, .(effect = data.table::first(ven_eff), n = .N),
            by = .(event_id, venue)][n >= min_n]
  if (!nrow(out)) return(empty)

  # Centre per event so venue effects are relative, not an extra intercept.
  out[, effect := effect - stats::weighted.mean(effect, n), by = event_id]
  out[]
}


#' Remove persistent venue effects from marks
#'
#' @param results Canonical results.
#' @param venue_effect A table from [fit_venue_effect()].
#' @param venue_col Column identifying the venue; must match the fit.
#' @return `results` with `perf` adjusted and `venue_adj` recording the shift.
#'   Marks at venues with no estimate are unchanged.
#' @export
adjust_venue <- function(results, venue_effect, venue_col = "comp_name") {
  dt <- .one_copy_dt(results)
  if (is.null(venue_effect) || !nrow(venue_effect) || !venue_col %in% names(dt)) {
    dt[, venue_adj := 0]
    return(dt[])
  }
  key <- paste(dt$event_id, as.character(dt[[venue_col]]))
  vk <- paste(venue_effect$event_id, venue_effect$venue)
  adj <- venue_effect$effect[match(key, vk)]
  adj[!is.finite(adj)] <- 0
  dt[, venue_adj := adj]
  dt[, perf := perf - venue_adj]
  dt[]
}
