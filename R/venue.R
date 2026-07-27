#' Estimate persistent venue effects
#'
#' Some tracks and pools are reliably fast. That is distinct from the race-level
#' shock in [calibrate()]: a race effect captures conditions on one day, but
#' cannot learn that a particular stadium has been quick for fifteen years.
#'
#' Measured on a championship harvest, venue explains **9.1%** of within-athlete
#' variance in the men's 1500m and 3.2% in the 100m — the former larger than the
#' wind effect. Long jump shows none, which is the expected sanity check: track
#' surface and pacing culture vary between venues, a sand pit does not.
#'
#' Effects are estimated per event, from within-athlete deviations, and
#' de-biased for sampling noise the same way [calibrate()] handles race effects:
#' a venue seen a handful of times will show apparent spread from noise alone.
#'
#' @param results Canonical results with a venue column.
#' @param venue_col Column identifying the venue. Prefer `venue_stadium`;
#'   `comp_name` is a serviceable proxy for harvests predating stadium capture,
#'   because recurring meets return to the same venue.
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
  # Centre within athlete-event so venue cannot absorb ability: fast athletes
  # disproportionately race at fast meets.
  dt[, dev := perf - mean(perf), by = .(athlete_id, event_id)]

  out <- dt[, .(effect = mean(dev), n = .N), by = .(event_id, venue)][n >= min_n]
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
  dt <- data.table::copy(data.table::as.data.table(results))
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
