#' Measure how asymmetric performance is around an athlete's own level
#'
#' Performance is not symmetric and the simulator assumes it is. An athlete can
#' have an arbitrarily bad day -- a jog-through, an injury mid-race, three
#' failures at the opening height, a foul-out -- but cannot have a miraculously
#' good one, because ability is bounded above by physiology and unbounded below
#' by circumstance.
#'
#' Measured on 802,099 marks across seven events (2026-07-31), taking each
#' athlete's deviations from their OWN median so ability is removed and no
#' outlier can move the centre:
#'
#' | event | up_sd | dn_sd | ratio | 99th pct | 1st pct |
#' |---|---|---|---|---|---|
#' | Pole Vault M | 0.0426 | 0.0769 | 1.81 | +2.24 | -4.74 |
#' | Shot Put M | 0.0426 | 0.0731 | 1.71 | +2.35 | -4.73 |
#' | 1500m M | 0.0215 | 0.0325 | 1.51 | +2.27 | -3.90 |
#' | 100m M | 0.0169 | 0.0245 | 1.46 | +2.31 | -3.75 |
#' | High Jump W | 0.0298 | 0.0406 | 1.36 | +2.17 | -3.38 |
#'
#' Percentiles are in units of the athlete's own good-side spread. The good tail
#' stops at a strikingly consistent +2.2 to +2.35 across every event, which is
#' the physiological ceiling appearing in the data; the bad tail runs to -3.4 to
#' -4.7. The worst events are pole vault and shot put -- exactly the ones where
#' you can fail out of a competition.
#'
#' **Why it matters for placings.** A single symmetric `sigma` fitted on all
#' marks sits between the two, so the simulated GOOD tail is 12-39% wider than
#' anything the athlete has ever produced (22% on the 100m, 39% on pole vault).
#' A race is won by the best draw, so downside contamination is converted
#' directly into upside win probability. That is how an athlete predicted at
#' 11.66 can out-rank one predicted at 10.17.
#'
#' @param results Canonical results with `athlete_id`, `event_id` and `perf`.
#' @param min_marks Minimum marks per athlete-event to contribute. Below about a
#'   dozen the median is too noisy for the deviations to mean anything.
#' @param min_athletes Minimum contributing athlete-events for an event to get
#'   its own ratios; below this it falls back to its family.
#' @return A `data.table` with one row per event: `r_up` and `r_dn`, the good-
#'   and bad-side spreads as multiples of the pooled spread, plus the counts
#'   behind them. Ratios are the quantity the simulator needs, because it
#'   already knows each athlete's overall `sigma` and only needs to know how to
#'   split it.
#' @export
fit_asymmetry <- function(results, min_marks = 12L, min_athletes = 30L) {
  d <- data.table::as.data.table(results)
  need <- c("athlete_id", "event_id", "perf")
  if (!all(need %in% names(d))) {
    cli::cli_abort("{.arg results} needs {.field {setdiff(need, names(d))}}.")
  }
  d <- d[!is.na(perf) & !is.na(event_id) & !is.na(athlete_id)]
  if (!nrow(d)) {
    return(data.table::data.table(event_id = character(), r_up = numeric(),
                                  r_dn = numeric(), n_ath = integer(),
                                  n = integer(), source = character()))
  }

  # Centre on the athlete's own MEDIAN, not their mean. The contamination this
  # function exists to measure would drag a mean down and then be measured
  # relative to its own effect.
  d[, .k := .N, by = .(athlete_id, event_id)]
  d <- d[.k >= min_marks]
  if (!nrow(d)) {
    return(data.table::data.table(event_id = character(), r_up = numeric(),
                                  r_dn = numeric(), n_ath = integer(),
                                  n = integer(), source = character()))
  }
  d[, dev := perf - stats::median(perf), by = .(athlete_id, event_id)]

  semi <- function(v, side) {
    x <- if (side > 0) v[v > 0] else v[v < 0]
    if (length(x) < 2L) return(NA_real_)
    sqrt(mean(x^2))
  }
  ev <- d[, {
    a <- stats::sd(dev)
    u <- semi(dev, 1); l <- semi(dev, -1)
    .(r_up = u / a, r_dn = l / a, n_ath = data.table::uniqueN(athlete_id), n = .N)
  }, by = event_id]

  reg <- data.table::as.data.table(
    .citius_event_registry[, c("event_id", "family")])
  ev <- merge(ev, reg, by = "event_id", all.x = TRUE)
  ev[, source := "event"]

  # Thin events borrow their family's ratios rather than a global constant.
  # Asymmetry tracks the failure mode of the event -- technical events can foul
  # out, track events cannot -- and that is a family property, not a global one.
  fam <- ev[n_ath >= min_athletes & is.finite(r_up) & is.finite(r_dn),
            .(f_up = stats::median(r_up), f_dn = stats::median(r_dn)), by = family]
  ev <- merge(ev, fam, by = "family", all.x = TRUE)
  thin <- ev$n_ath < min_athletes | !is.finite(ev$r_up) | !is.finite(ev$r_dn)
  ev[thin & is.finite(f_up), `:=`(r_up = f_up, r_dn = f_dn, source = "family")]
  ev <- ev[is.finite(r_up) & is.finite(r_dn)]

  data.table::setorder(ev, -n)
  ev[, .(event_id, family, r_up, r_dn, n_ath, n, source)][]
}


#' Good- and bad-side scale multipliers for one event
#'
#' Returns `NULL` when no asymmetry is calibrated, which makes the simulator
#' fall back to a symmetric draw. That is the honest default: it fails visibly
#' rather than encoding a guess about a quantity that varies 1.36-1.81 across
#' events.
#' @keywords internal
#' @noRd
.asymmetry_ratios <- function(event_id, calibration) {
  if (is.null(calibration) || is.null(calibration$asymmetry)) return(NULL)
  a <- data.table::as.data.table(calibration$asymmetry)
  if (!nrow(a) || !all(c("r_up", "r_dn") %in% names(a))) return(NULL)
  i <- match(event_id, a$event_id)
  if (is.na(i)) {
    reg <- .citius_event_registry
    fam <- reg$family[match(event_id, reg$event_id)]
    if (is.na(fam) || !"family" %in% names(a)) return(NULL)
    sub <- a[family == fam]
    if (!nrow(sub)) return(NULL)
    return(c(up = stats::median(sub$r_up), dn = stats::median(sub$r_dn)))
  }
  out <- c(up = a$r_up[i], dn = a$r_dn[i])
  if (!all(is.finite(out))) return(NULL)
  out
}
