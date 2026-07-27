#' Estimate how performance varies with age
#'
#' Fits a smooth age curve per event family from *within-athlete* variation.
#' Each performance is first centred on its own athlete-event mean, which
#' removes ability entirely; what remains is how far above or below their own
#' career level an athlete performed at each age. The curve is therefore
#' identified the same way a fixed-effects panel model is — from athletes being
#' observed at several ages, never from comparing a 20-year-old to a 30-year-old.
#'
#' That distinction is the whole point. Comparing across athletes would measure
#' which ages elite athletes happen to be, not what ageing does to a given
#' athlete: sprinters who are still competing at 34 are a heavily selected
#' group, and a cross-sectional curve would read their survivorship as
#' longevity.
#'
#' Curves are fitted per family rather than per event because peak ages differ
#' far more between sprinting, distance running and throwing than between the
#' 100m and the 200m, and pooling buys the smoother a great deal of data.
#'
#' @param results Canonical results with an `age` column.
#' @param min_results Minimum results for a family to be fitted.
#' @param k Basis dimension for the age smooth.
#' @param min_density Minimum share of a family's results that must fall within
#'   a year of an age for that age to be eligible as the peak. Guards against
#'   the smoother's drift across the sparse tail being read as a late peak.
#' @return An object of class `citius_aging`: per-family fitted curves, the age
#'   at which each family peaks, and the plateau around it.
#' @seealso [age_adjustment()] to apply it.
#' @export
fit_aging_curve <- function(results, min_results = 200L, k = 6,
                            min_density = 0.01) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    cli::cli_abort("{.pkg mgcv} is required to fit aging curves.")
  }
  dt <- data.table::as.data.table(results)
  dt <- dt[!is.na(perf) & !is.na(age) & is.finite(age) & age > 10 & age < 50]
  if (!nrow(dt)) return(.empty_aging())

  # Drop any pre-existing `family` before merging. Otherwise the join creates
  # family.x/family.y, the NSE below falls through to stats::family, and the
  # error is "cannot coerce type 'closure'" - which points nowhere near the
  # cause. Callers reusing enriched data hit this easily.
  if ("family" %in% names(dt)) dt[, family := NULL]
  reg <- .citius_event_registry[, c("event_id", "family")]
  dt <- merge(dt, reg, by = "event_id", all.x = TRUE, sort = FALSE)
  dt <- dt[!is.na(family)]

  # Centre within athlete-event: removes ability, leaves the age pattern.
  dt[, athlete_id := as.character(athlete_id)]
  dt[, dev := perf - mean(perf), by = .(athlete_id, event_id)]
  # Only athletes seen at more than one age carry information about ageing.
  dt[, n_ages := data.table::uniqueN(round(age)), by = .(athlete_id, event_id)]
  dt <- dt[n_ages >= 2L]
  if (!nrow(dt)) return(.empty_aging())

  fams <- dt[, .N, by = family][N >= min_results]$family
  if (!length(fams)) return(.empty_aging())

  curves <- lapply(fams, function(f) {
    sub <- dt[family == f]
    kk <- min(k, max(3L, data.table::uniqueN(round(sub$age)) - 1L))
    fit <- try(mgcv::gam(dev ~ s(age, k = kk), data = sub), silent = TRUE)
    if (inherits(fit, "try-error")) return(NULL)

    grid <- data.frame(age = seq(min(sub$age), max(sub$age), length.out = 200))
    grid$effect <- as.numeric(stats::predict(fit, newdata = grid))

    # Peak search is restricted to ages the data actually supports. Age curves
    # are close to flat across the plateau, so an unrestricted `which.max`
    # latches onto smoother drift in the sparse tail: on a real harvest that put
    # the middle-distance peak at 37 (three observations) and road running at 49
    # (one). Support is defined by local density rather than a fixed age
    # window, so it adapts to each family's data.
    density <- vapply(grid$age, function(a) sum(abs(sub$age - a) < 1), numeric(1))
    supported <- density >= max(min_density * nrow(sub), 10)
    if (!any(supported)) supported <- rep(TRUE, nrow(grid))

    peak_idx <- which(supported)[which.max(grid$effect[supported])]
    grid$effect <- grid$effect - grid$effect[peak_idx]

    # The plateau: ages performing within a whisker of peak. Reported because a
    # single "peak age" implies a precision these curves do not have.
    near <- supported & grid$effect > -0.002
    sup_ages <- grid$age[supported]

    # A peak sitting on the edge of the supported range is not a peak: the curve
    # was still rising where the data ran out, so the maximum is an artefact of
    # where observation stopped rather than of physiology. Families in this
    # state are flagged rather than silently reported, because their curve
    # cannot distinguish "peaks here" from "we stopped looking here".
    peak_interior <- grid$age[peak_idx] > min(sup_ages) + 0.5 &&
      grid$age[peak_idx] < max(sup_ages) - 0.5

    data.table::data.table(
      family = f, age = grid$age, effect = grid$effect,
      supported = supported,
      peak_age = grid$age[peak_idx],
      peak_identified = peak_interior,
      plateau_lo = if (any(near)) min(grid$age[near]) else NA_real_,
      plateau_hi = if (any(near)) max(grid$age[near]) else NA_real_,
      n = nrow(sub))
  })

  curves <- data.table::rbindlist(Filter(Negate(is.null), curves))
  if (!nrow(curves)) return(.empty_aging())

  peaks <- unique(curves[, .(family, peak_age, peak_identified,
                             plateau_lo, plateau_hi, n)])
  unident <- peaks[peak_identified == FALSE]$family
  if (length(unident)) {
    cli::cli_warn(c(
      "Peak age not identified for {.val {unident}}: the curve was still rising where the data ran out.",
      i = "Treat {.field peak_age} for {length(unident)} famil{?y/ies} as a lower bound, not an estimate."
    ))
  }
  structure(list(curves = curves[], peaks = peaks[]), class = "citius_aging")
}

#' @keywords internal
#' @noRd
.empty_aging <- function() {
  structure(list(
    curves = data.table::data.table(family = character(), age = numeric(),
                                    effect = numeric(), peak_age = numeric(),
                                    n = integer()),
    peaks = data.table::data.table(family = character(), peak_age = numeric(),
                                   peak_identified = logical(),
                                   plateau_lo = numeric(), plateau_hi = numeric(),
                                   n = integer())
  ), class = "citius_aging")
}

#' @export
print.citius_aging <- function(x, ...) {
  cli::cli_h3("citius aging curves")
  if (!nrow(x$peaks)) {
    cli::cli_text("No families fitted.")
    return(invisible(x))
  }
  print(x$peaks[order(peak_age)])
  invisible(x)
}


#' Age effect on performance, on the log performance scale
#'
#' Returns how much better or worse an athlete of a given age performs relative
#' to that family's peak age. Values are zero at peak and negative away from it.
#'
#' @param age Numeric vector of ages in years.
#' @param event_id Canonical event ids, recycled against `age`.
#' @param aging A `citius_aging` from [fit_aging_curve()].
#' @return Numeric vector of adjustments; `0` where no curve is available, so an
#'   unfitted family degrades to no age correction rather than a guessed one.
#' @export
age_adjustment <- function(age, event_id, aging) {
  n <- max(length(age), length(event_id))
  age <- rep_len(as.numeric(age), n)
  event_id <- rep_len(as.character(event_id), n)
  out <- rep(0, n)
  if (is.null(aging) || !inherits(aging, "citius_aging") || !nrow(aging$curves)) {
    return(out)
  }

  reg <- .citius_event_registry
  family <- reg$family[match(event_id, reg$event_id)]

  for (f in unique(stats::na.omit(family))) {
    cv <- aging$curves[family == f]
    if (!nrow(cv)) next
    idx <- which(family == f & is.finite(age))
    if (!length(idx)) next
    out[idx] <- stats::approx(cv$age, cv$effect, xout = age[idx], rule = 2)$y
  }
  out
}


#' Project an ability estimate to a different age
#'
#' Ability estimated from a career history reflects the ages at which those
#' performances happened. Predicting a Games two years out means asking what the
#' athlete will be capable of at their age *then*, which for a 22-year-old and a
#' 34-year-old are movements in opposite directions.
#'
#' @param ability A `data.table` from [estimate_ability()] with an `age_now`
#'   column giving each athlete's age at the target date, and `age_ref` giving
#'   the mean age of the results the estimate was built from.
#' @param aging A `citius_aging` from [fit_aging_curve()].
#' @return `ability` with `ability` shifted by the age difference and an
#'   `age_shift` column recording the adjustment applied.
#' @export
project_ability <- function(ability, aging, max_shift = 0.05) {
  ab <- data.table::copy(data.table::as.data.table(ability))
  req <- c("age_now", "age_ref", "event_id")
  missing <- setdiff(req, names(ab))
  if (length(missing)) {
    cli::cli_abort("{.arg ability} is missing required column{?s}: {.field {missing}}.")
  }
  ab[, age_shift := age_adjustment(age_now, event_id, aging) -
       age_adjustment(age_ref, event_id, aging)]

  # Scale the projection by how much of the estimate is actually about this
  # athlete. When ability has been shrunk most of the way to the event mean,
  # the number being projected is largely the event mean, and ageing an event
  # mean is meaningless. Without this, athletes with almost no recent evidence
  # get a full ageing correction applied to a population average — which was
  # enough to put sprinters at the top of the triple jump.
  if ("shrinkage" %in% names(ab)) {
    ab[is.finite(shrinkage), age_shift := age_shift * (1 - shrinkage)]
  }

  # A large shift means `age_ref` is not the age the estimate actually
  # represents. The usual cause is passing an unweighted career-mean age: an
  # athlete with a long junior record averages out around 19, so the full
  # junior-to-peak improvement gets applied on top of an estimate that already
  # reflects current form. That double-counts ageing and inflates young
  # athletes enormously — in one real case pushing a sprinter's projection
  # faster than his personal best. `age_ref` should come from
  # [estimate_ability()], which computes it under the same weights.
  big <- ab[is.finite(age_shift) & abs(age_shift) > max_shift]
  if (nrow(big)) {
    cli::cli_warn(c(
      "{nrow(big)} projection{?s} shift ability by more than {round(100 * max_shift)}%.",
      i = "Check {.field age_ref} is the weighted mean age from {.fn estimate_ability}, not a career mean.",
      i = "Largest: {round(100 * (exp(max(abs(big$age_shift))) - 1), 1)}%."
    ), .frequency = "once", .frequency_id = "citius_big_age_shift")
  }

  ab[, ability := ability + age_shift]
  ab[]
}
