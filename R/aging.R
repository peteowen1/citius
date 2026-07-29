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
                            min_density = 0.01,
                            method = c("difference", "centred")) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    cli::cli_abort("{.pkg mgcv} is required to fit aging curves.")
  }
  method <- match.arg(method)
  if (identical(method, "difference")) {
    return(.fit_aging_difference(results, min_results = min_results, k = k,
                                 min_density = min_density))
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

#' Aging curve by first differences
#'
#' The `"centred"` method subtracts each athlete-event mean and smooths the
#' deviation against age. That is a fixed-effects estimator on an **unbalanced
#' panel**, and the deviation at a given age depends on how much of the athlete's
#' career was observed: a sprinter seen only at 17-19 has a career mean that *is*
#' their junior level, so their deviation at 18 is near zero, while one seen
#' 17-30 has a mean including their peak and sits far below it. Measured on the
#' athletics corpus, mean deviation at age 18 runs from +0.17% for athletes with
#' under two years of span to -1.75% for those with twelve or more.
#'
#' Pooling those flattens exactly the tails, where the span mix is most skewed.
#' The centred fit put a 16-year-old sprinter **0.55%** off their peak — a future
#' 10.00 runner clocking 10.06 at sixteen.
#'
#' This method instead fits the **year-on-year change** against age and
#' integrates it. Differencing removes the athlete's level entirely, so career
#' span cannot bias it. The season aggregate is the **median**, not the best: a
#' season best improves purely from taking more attempts, and race counts fall
#' with age (4.5 a year at 29-31 down to 2.9 at 35+), which would otherwise
#' masquerade as decline.
#'
#' The trade is that integrating a slope accumulates error, so levels can drift
#' over a long range where the centred fit would not. Measured against a backtest
#' it is clearly the better estimator — absolute mark error improves with
#' `t = 12.4` over 28,737 marks and the systematic bias falls from -0.235% to
#' -0.011% — but it slightly over-corrects both tails.
#'
#' @keywords internal
#' @noRd
.fit_aging_difference <- function(results, min_results = 200L, k = 8,
                                  min_density = 0.005) {
  dt <- data.table::as.data.table(results)
  need <- c("athlete_id", "event_id", "perf", "age")
  miss <- setdiff(need, names(dt))
  if (length(miss)) {
    cli::cli_abort("{.arg results} is missing {.field {miss}}.")
  }
  dt <- dt[!is.na(perf) & !is.na(event_id) & !is.na(age)]
  if ("family" %in% names(dt)) dt[, family := NULL]
  reg <- .citius_event_registry[, c("event_id", "family")]
  dt <- merge(dt, reg, by = "event_id", all.x = TRUE, sort = FALSE)
  dt <- dt[!is.na(family)]
  if (!nrow(dt)) return(.empty_aging())

  dt[, athlete_id := as.character(athlete_id)]
  # Seasons are indexed by AGE, not calendar year. Age is what the curve is
  # about, and it does not depend on the feed carrying usable dates -- a caller
  # whose data spans many ages within one calendar year (synthetic fixtures do
  # exactly this) would otherwise collapse to a single season and form no pairs.
  dt[, .yr := round(age)]
  # One row per athlete-event-season, on the MEDIAN.
  y <- dt[, .(perf = stats::median(perf), age = mean(age)),
          by = c("athlete_id", "event_id", "family", ".yr")]
  data.table::setorderv(y, c("athlete_id", "event_id", ".yr"))
  y[, `:=`(.d = perf - data.table::shift(perf),
           .da = age - data.table::shift(age),
           .a0 = data.table::shift(age)), by = c("athlete_id", "event_id")]
  d <- y[is.finite(.d) & .da > 0.5 & .da < 2]
  if (!nrow(d)) return(.empty_aging())
  d[, .slope := .d / .da]
  d[, .amid := .a0 + .da / 2]

  fams <- d[, .N, by = family][N >= min_results]$family
  if (!length(fams)) return(.empty_aging())

  curves <- data.table::rbindlist(lapply(fams, function(f) {
    sub <- d[family == f]
    kk <- min(k, max(3L, data.table::uniqueN(round(sub$.amid)) - 1L))
    fit <- try(mgcv::gam(.slope ~ s(.amid, k = kk), data = sub), silent = TRUE)
    if (inherits(fit, "try-error")) return(NULL)
    grid <- data.frame(.amid = seq(min(sub$.amid), max(sub$.amid), length.out = 200))
    grid$slope <- as.numeric(stats::predict(fit, newdata = grid))
    # Integrate the slope to recover the level.
    eff <- cumsum(c(0, utils::head(grid$slope, -1) * diff(grid$.amid)))
    dens <- vapply(grid$.amid, function(a) sum(abs(sub$.amid - a) < 1), numeric(1))
    sup <- dens >= max(min_density * nrow(sub), 10)
    if (!any(sup)) sup <- rep(TRUE, nrow(grid))
    pk <- which(sup)[which.max(eff[sup])]
    eff <- eff - eff[pk]
    # Identified means the peak sits INSIDE the supported range, not inside the
    # full grid. Comparing against the grid missed a curve still rising where the
    # data ran out, because the unsupported tail kept `pk` away from nrow(grid).
    sup_idx <- which(sup)
    ident <- length(sup_idx) > 2L && pk > min(sup_idx) && pk < max(sup_idx)
    if (!ident) {
      cli::cli_warn(
        c("Aging peak for {.val {f}} is not identified: it sits at the edge of the supported range.",
          i = "{.field peak_age} is a bound, not an estimate."))
    }
    data.table::data.table(
      family = f, age = grid$.amid, effect = eff, supported = sup,
      peak_age = grid$.amid[pk], peak_identified = ident,
      plateau_lo = min(grid$.amid[eff > -0.005]),
      plateau_hi = max(grid$.amid[eff > -0.005]), n = nrow(sub))
  }), fill = TRUE)
  if (!nrow(curves)) return(.empty_aging())
  peaks <- unique(curves[, .(family, peak_age, peak_identified,
                             plateau_lo, plateau_hi, n)])
  structure(list(curves = curves[], peaks = peaks[]), class = "citius_aging")
}

#' Blend two aging curves
#'
#' The centred and first-difference estimators fail in opposite directions.
#' Centring on an unbalanced panel **under**-corrects the tails; integrating a
#' fitted slope **over**-corrects them, because integration accumulates error.
#' Measured on the athletics backtest, the mark bias for athletes aged 20 or
#' under runs −0.543% under the centred curve and +0.238% under the
#' first-difference one — the same defect seen from both sides.
#'
#' A blend lands between. The weight is a fitted quantity, not a preference:
#' solving `(1 - w) * bias_centred + w * bias_diff = 0` band by band gives 0.70
#' at age 20 and under, 0.65 at 20–23 and 0.74 above 32.
#'
#' No single scalar can zero every band — the middle bands are negative under
#' both estimators, so they imply `w > 1`. The weight targets the tails, which is
#' where the two estimators actually disagree.
#'
#' @param centred,difference Aging objects from [fit_aging_curve()].
#' @param weight How far toward `difference` to move, in `[0, 1]`.
#' @return A `citius_aging` object on the blended curve.
#' @export
blend_aging <- function(centred, difference, weight = 0.7) {
  stopifnot(inherits(centred, "citius_aging"), inherits(difference, "citius_aging"))
  if (!is.finite(weight) || weight < 0 || weight > 1) {
    cli::cli_abort("{.arg weight} must lie in [0, 1].")
  }
  a <- data.table::as.data.table(centred$curves)
  b <- data.table::as.data.table(difference$curves)
  fams <- intersect(unique(a$family), unique(b$family))
  if (!length(fams)) cli::cli_abort("The two curves share no families.")

  curves <- data.table::rbindlist(lapply(fams, function(f) {
    ca <- a[family == f]; cb <- b[family == f]
    # Interpolate both onto a common grid before mixing -- the two estimators
    # produce grids over different age ranges, and mixing them positionally
    # would blend age 16 with age 19.
    lo <- max(min(ca$age), min(cb$age)); hi <- min(max(ca$age), max(cb$age))
    if (!is.finite(lo) || !is.finite(hi) || hi <= lo) return(NULL)
    grid <- seq(lo, hi, length.out = 200)
    ea <- stats::approx(ca$age, ca$effect, xout = grid, rule = 2)$y
    eb <- stats::approx(cb$age, cb$effect, xout = grid, rule = 2)$y
    eff <- (1 - weight) * ea + weight * eb
    pk <- which.max(eff)
    eff <- eff - eff[pk]
    data.table::data.table(
      family = f, age = grid, effect = eff, supported = TRUE,
      peak_age = grid[pk], peak_identified = pk > 1L && pk < length(grid),
      plateau_lo = min(grid[eff > -0.005]), plateau_hi = max(grid[eff > -0.005]),
      n = cb$n[1])
  }), fill = TRUE)
  if (!nrow(curves)) cli::cli_abort("Blending produced no curves.")
  peaks <- unique(curves[, .(family, peak_age, peak_identified,
                             plateau_lo, plateau_hi, n)])
  structure(list(curves = curves[], peaks = peaks[]), class = "citius_aging")
}
