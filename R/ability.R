#' Weight a historical result by recency, competition tier and round
#'
#' Controls how much each past performance counts toward an athlete's current
#' ability. Three multiplicative components:
#'
#' \describe{
#'   \item{Recency}{Exponential decay with a configurable half-life. Form is
#'     transient; a run from three years ago should not carry the weight of one
#'     from last month.}
#'   \item{Tier}{Championship and Diamond League fields are deep and paced;
#'     minor meets are neither. A time run in a stacked race is stronger
#'     evidence of ability than the same time run alone.}
#'   \item{Round}{Heats are frequently coasted — athletes do the minimum needed
#'     to qualify — so a heat time understates ability more often than a final
#'     time does.}
#' }
#'
#' @param date Date vector of performance dates.
#' @param tier Character vector of World Athletics category codes (`"OW"`,
#'   `"GL"`, `"A"`..`"F"`), or `NA`.
#' @param round Character vector of round codes (`"F"`, `"SF1"`, `"H4"`, or
#'   World Aquatics `"Final"`/`"Heats"`).
#' @param as_of Reference date from which recency is measured.
#' @param half_life Days after which a result carries half weight.
#' @param calibration Optional `citius_calibration` from [calibrate()]. Supplies
#'   measured precisions for each round and tier. Without one, context weights
#'   are flat and only recency applies.
#' @return Numeric vector of non-negative weights.
#' @seealso [calibrate()]
#' @export
result_weight <- function(date, tier = NA_character_, round = NA_character_,
                          as_of = Sys.Date(), half_life = 540,
                          calibration = NULL) {
  n <- length(date)
  age_days <- as.numeric(as_of - as.Date(date))
  age_days[is.na(age_days) | age_days < 0] <- 0
  recency <- 0.5^(age_days / half_life)

  tier <- rep_len(as.character(tier), n)
  round <- rep_len(as.character(round), n)

  recency * .context_precision(calibration, "round", .round_class(round)) *
    .context_precision(calibration, "tier", .tier_class(tier))
}

#' Measured precision of a context, or a flat weight when uncalibrated
#'
#' Weights are precisions: a context whose residuals are noisy carries less
#' information about ability and is downweighted in proportion. Nothing is
#' asserted about heats mattering less than finals — that falls out of how
#' predictable each turns out to be.
#'
#' Returning flat weights when no calibration is supplied is deliberate. A
#' guessed weighting is not more honest than no weighting, and a flat one at
#' least fails visibly rather than quietly encoding an assumption.
#'
#' @keywords internal
#' @noRd
.context_precision <- function(calibration, which, classes) {
  if (is.null(calibration) || is.null(calibration[[which]])) {
    return(rep(1, length(classes)))
  }
  tbl <- calibration[[which]]
  col <- if (which == "round") "round_class" else "tier_class"
  out <- tbl$precision[match(classes, tbl[[col]])]
  out[!is.finite(out)] <- stats::median(tbl$precision, na.rm = TRUE)
  out[!is.finite(out)] <- 1
  out
}


#' Estimate the recency half-life from predictive accuracy
#'
#' How fast form decays is a property of a sport, not a modelling preference,
#' and it is directly measurable: hold out each athlete's most recent
#' performance, predict it from their earlier ones under a range of half-lives,
#' and keep whichever predicts best.
#'
#' Fitting this matters more than it looks. Too long a half-life keeps
#' decade-old form alive and lets retired athletes contend; too short throws
#' away real evidence and leaves every estimate at the event mean. Sprint form
#' and marathon form do not decay at the same rate, so half-lives are fitted per
#' event family.
#'
#' Athletes with fewer than three results contribute nothing — there is no
#' history to predict *from* once one result is held out.
#'
#' @param results Canonical results.
#' @param candidates Half-lives in days to evaluate.
#' @param min_history Minimum prior results an athlete must have to contribute.
#' @return A `data.table` with one row per family: the chosen `half_life`, the
#'   error achieved, and how many held-out performances informed it.
#' @examples
#' \dontrun{
#' fit_half_life(history)
#' }
#' @export
fit_half_life <- function(results,
                          candidates = c(90, 180, 270, 365, 540, 730, 1095, 1825, 3650),
                          min_history = 3L) {
  dt <- data.table::as.data.table(results)
  dt <- dt[!is.na(perf) & !is.na(event_id) & !is.na(date)]
  if (!nrow(dt)) {
    return(data.table::data.table(family = character(), half_life = numeric(),
                                  mae = numeric(), n = integer()))
  }
  dt[, athlete_id := as.character(athlete_id)]

  reg <- .citius_event_registry[, c("event_id", "family")]
  dt <- merge(dt, reg, by = "event_id", all.x = TRUE, sort = FALSE)
  dt <- dt[!is.na(family)]

  data.table::setorder(dt, athlete_id, event_id, date)
  dt[, idx := seq_len(.N), by = .(athlete_id, event_id)]
  dt[, n_tot := .N, by = .(athlete_id, event_id)]
  usable <- dt[n_tot >= min_history + 1L]
  if (!nrow(usable)) {
    return(data.table::data.table(family = character(), half_life = numeric(),
                                  mae = numeric(), n = integer()))
  }

  scored <- data.table::rbindlist(lapply(candidates, function(hl) {
    usable[, {
      target <- perf[.N]
      t_date <- date[.N]
      past <- seq_len(.N - 1L)
      w <- 0.5^(as.numeric(t_date - date[past]) / hl)
      pred <- if (sum(w) > 0) stats::weighted.mean(perf[past], w) else NA_real_
      .(family = data.table::first(family), err = abs(target - pred))
    }, by = .(athlete_id, event_id)][, .(half_life = hl, mae = mean(err, na.rm = TRUE),
                                         n = sum(!is.na(err))), by = family]
  }))

  best <- scored[, .SD[which.min(mae)], by = family]

  # An optimum sitting on the edge of the search grid is not an optimum: error
  # was still falling where the grid ran out, so the value reflects where we
  # stopped looking. Those families fall back to the pooled optimum across
  # identified families rather than reporting a boundary artefact.
  lo <- min(candidates); hi <- max(candidates)
  best[, identified := half_life > lo & half_life < hi]

  if (any(best$identified)) {
    pooled <- stats::weighted.mean(best[identified == TRUE]$half_life,
                                   best[identified == TRUE]$n)
    unident <- best[identified == FALSE]$family
    if (length(unident)) {
      cli::cli_warn(c(
        "Half-life unidentified for {.val {unident}}; using the pooled value {round(pooled)} days.",
        i = "The optimum sat on the edge of {.arg candidates} - widen the grid or harvest more history."
      ), .frequency = "once", .frequency_id = "citius_hl_unidentified")
      best[identified == FALSE, half_life := pooled]
    }
  }
  best[]
}


#' Look up an event's fitted half-life, with a documented fallback
#' @keywords internal
#' @noRd
.event_half_life <- function(event_id, half_life) {
  default <- 540
  if (is.null(half_life)) return(rep(default, length(event_id)))
  if (is.numeric(half_life)) return(rep_len(half_life, length(event_id)))

  reg <- .citius_event_registry
  fam <- reg$family[match(event_id, reg$event_id)]
  hl <- data.table::as.data.table(half_life)
  out <- hl$half_life[match(fam, hl$family)]
  out[!is.finite(out)] <- if (nrow(hl)) stats::median(hl$half_life) else default
  out
}


#' Estimate systematic round and tier offsets from the data
#'
#' Athletes do not perform uniformly across contexts. Heats are coasted, minor
#' meets are unpaced and shallow, championship finals are peaked for. A plain
#' weighted mean over an athlete's whole history therefore estimates their
#' *average* performance, which is materially worse than the thing we actually
#' want to predict: their performance in a final.
#'
#' This recovers those offsets empirically rather than assuming them. Each
#' performance is centred on its own athlete's mean, which removes ability
#' entirely, and the remaining structure in the residuals is attributed to
#' round and then to competition tier. Ability can then be expressed on a
#' common *final-equivalent, top-tier* footing.
#'
#' Estimating offsets from within-athlete residuals rather than raw marks is
#' what makes this safe: otherwise the fact that better athletes reach more
#' finals would be absorbed into the "final effect".
#'
#' @param results Canonical results, as passed to [estimate_ability()].
#' @return A list with `round` and `tier` named numeric vectors of offsets on
#'   the log performance scale, plus the `n` behind each.
#' @export
estimate_context_effects <- function(results) {
  dt <- data.table::as.data.table(results)
  dt <- dt[!is.na(perf) & !is.na(event_id)]
  empty <- list(round = c(final = 0), tier = c(top = 0), n = 0L)
  if (!nrow(dt)) return(empty)

  dt[, athlete_id := as.character(athlete_id)]
  dt[, round_class := .round_class(if ("round" %in% names(dt)) round else NA_character_)]
  dt[, tier_class := .tier_class(if ("tier" %in% names(dt)) tier else NA_character_)]

  # Centre within athlete-event: removes ability, leaving context + noise.
  dt[, resid := perf - mean(perf), by = .(athlete_id, event_id)]

  r_eff <- dt[, .(eff = mean(resid), n = .N), by = round_class]
  r_eff[, eff := eff - eff[round_class == "final"][1]]
  if (!nrow(r_eff[round_class == "final"])) r_eff[, eff := eff - max(eff)]

  dt <- merge(dt, r_eff[, .(round_class, r_adj = eff)], by = "round_class", all.x = TRUE)
  dt[, resid2 := resid - r_adj]

  t_eff <- dt[, .(eff = mean(resid2), n = .N), by = tier_class]
  t_eff[, eff := eff - eff[tier_class == "top"][1]]
  if (!nrow(t_eff[tier_class == "top"])) t_eff[, eff := eff - max(eff)]

  list(
    round = stats::setNames(r_eff$eff, r_eff$round_class),
    tier  = stats::setNames(t_eff$eff, t_eff$tier_class),
    n     = nrow(dt)
  )
}

#' @keywords internal
#' @noRd
.round_class <- function(round) {
  r <- toupper(trimws(as.character(round)))
  out <- rep("other", length(r))
  out[grepl("^H", r) | grepl("HEAT", r)] <- "heat"
  out[grepl("^SF", r) | grepl("SEMI", r)] <- "semi"
  out[grepl("^QF", r) | grepl("QUARTER", r)] <- "quarter"
  out[grepl("^F", r) | grepl("FINAL", r)] <- "final"
  out[is.na(r)] <- "other"
  out
}

#' @keywords internal
#' @noRd
.tier_class <- function(tier) {
  t <- toupper(trimws(as.character(tier)))
  out <- rep("mid", length(t))
  out[t %in% c("OW", "GW", "GL")] <- "top"
  out[t %in% c("A", "B")] <- "high"
  out[t %in% c("C", "D", "DF")] <- "mid"
  out[t %in% c("E", "F")] <- "low"
  out[is.na(t)] <- "mid"
  out
}


#' Estimate latent athlete ability per event
#'
#' Produces, for every athlete-event pair present in `results`, a point estimate
#' of current ability on the oriented performance scale plus the within-athlete
#' spread around it.
#'
#' Two things distinguish this from a weighted mean of past marks:
#'
#' **Tactical trimming.** In events flagged `tactical` in [citius_events()], the
#' slowest performances are usually sit-and-kick championship races rather than
#' bad days. Including them drags the ability estimate down and inflates the
#' variance — badly. The worst `trim_tactical` fraction (by weight) is therefore
#' dropped for those events only. Sprints and swims are untrimmed.
#'
#' **Empirical-Bayes shrinkage.** An athlete with two results should not be
#' credited with a precisely-known ability. Estimates are shrunk toward the
#' event mean by a factor set by the ratio of within-athlete to between-athlete
#' variance, so sparse histories regress heavily and deep ones barely move.
#'
#' @param results A `data.table` of results in the canonical schema, as returned
#'   by [athlete_results()] or [aquatics_results()].
#' @param as_of Reference date for recency weighting. Defaults to today.
#' @param half_life Either a single number of days, or a fitted table from
#'   [fit_half_life()] giving a per-family half-life. Prefer the fitted table:
#'   how fast form decays is measurable, and the measured values (sprint ~135
#'   days, distance and field ~180) are far shorter than the 540-day scalar
#'   default, which keeps stale form alive.
#' @param trim_tactical Fraction of worst performances to drop in tactical
#'   events. Set to `0` to disable.
#' @param min_results Minimum results required to report an athlete.
#' @param adjust_context Whether to put every performance on a final-equivalent,
#'   top-tier footing before averaging, using [estimate_context_effects()].
#'   Without this the estimate answers "how does this athlete perform on an
#'   average day at an average meet", which is systematically slower than a
#'   championship final and will under-predict the event being simulated.
#' @return A `data.table` with `athlete_id`, `event_id`, `ability`,
#'   `ability_raw`, `sigma`, `n`, `n_eff`, `shrinkage`, `age_ref` and
#'   `last_date`. `age_ref` is the weighted mean age behind the estimate and is
#'   what [project_ability()] must project *from*.
#' @seealso [simulate_event()] which consumes this.
#' @export
estimate_ability <- function(results, as_of = Sys.Date(), half_life = 540,
                             trim_tactical = 0.25, min_results = 1L,
                             adjust_context = TRUE, calibration = NULL) {
  if (!nrow(results)) {
    return(data.table::data.table(
      athlete_id = character(), event_id = character(), ability = numeric(),
      ability_raw = numeric(), sigma = numeric(), n = integer(),
      n_eff = numeric(), shrinkage = numeric(), last_date = as.Date(character())
    ))
  }

  dt <- data.table::copy(data.table::as.data.table(results))
  dt <- dt[!is.na(perf) & !is.na(event_id)]
  if (!nrow(dt)) {
    return(estimate_ability(results[0], as_of, half_life, trim_tactical,
                            min_results, adjust_context, calibration))
  }

  dt[, athlete_id := as.character(athlete_id)]
  # Half-life may be a scalar or a fitted per-family table from fit_half_life().
  dt[, hl := .event_half_life(event_id, half_life)]
  dt[, w := result_weight(date, tier = if ("tier" %in% names(dt)) tier else NA_character_,
                          round = if ("round" %in% names(dt)) round else NA_character_,
                          as_of = as_of, half_life = hl,
                          calibration = calibration)]

  reg <- .citius_event_registry[, c("event_id", "tactical", "cv_prior")]
  dt <- merge(dt, reg, by = "event_id", all.x = TRUE, sort = FALSE)
  dt[is.na(tactical), tactical := FALSE]

  # Prefer a measured tactical signal over the registry's hand-set flag. Races
  # in tactical events skew slow, so a strongly negative skew in the fitted race
  # effects is direct evidence that times decouple from ability.
  if (!is.null(calibration) && !is.null(calibration$events)) {
    ti <- calibration$events[, c("event_id", "tactical_index", "calibrated")]
    dt <- merge(dt, ti, by = "event_id", all.x = TRUE, sort = FALSE)
    dt[isTRUE(calibrated) & is.finite(tactical_index), tactical := tactical_index < -0.5]
  }

  if (isTRUE(adjust_context)) {
    ctx <- if (!is.null(calibration) && !is.null(calibration$round)) {
      list(round = stats::setNames(calibration$round$offset, calibration$round$round_class),
           tier  = stats::setNames(calibration$tier$offset, calibration$tier$tier_class))
    } else estimate_context_effects(dt)
    rc <- .round_class(if ("round" %in% names(dt)) dt$round else NA_character_)
    tc <- .tier_class(if ("tier" %in% names(dt)) dt$tier else NA_character_)
    r_adj <- ctx$round[rc]; r_adj[is.na(r_adj)] <- 0
    t_adj <- ctx$tier[tc];  t_adj[is.na(t_adj)] <- 0
    dt[, perf := perf - unname(r_adj) - unname(t_adj)]
  }

  if (trim_tactical > 0) {
    dt <- dt[, .SD[.trim_worst(perf, w, trim = data.table::first(tactical) * trim_tactical)],
             by = .(athlete_id, event_id)]
  }

  ab <- dt[, {
    ok <- w > 0
    mu <- if (sum(ok)) stats::weighted.mean(perf[ok], w[ok]) else NA_real_
    s  <- .weighted_sd(perf[ok], w[ok])
    # The effective age of the estimate: the weighted mean age under the same
    # weights that produced it. This is what [project_ability()] must measure
    # from. Using an unweighted career mean instead double-counts ageing —
    # recency decay already makes the estimate reflect current form, so
    # shifting it by the gap from a junior-heavy career mean applies the
    # improvement a second time.
    a_ref <- if ("age" %in% names(.SD) && sum(ok & !is.na(age))) {
      stats::weighted.mean(age[ok & !is.na(age)], w[ok & !is.na(age)])
    } else NA_real_
    .(ability_raw = mu,
      sigma_raw   = s,
      n           = .N,
      # Total weight is *absolute* evidence and is what shrinkage must use.
      # n_eff below measures only how evenly weight is spread, so an athlete
      # whose results are all twenty years old keeps a high n_eff — their
      # weights are uniformly tiny — and escapes shrinkage entirely.
      w_total     = sum(w),
      n_eff       = sum(w)^2 / sum(w^2),
      age_ref     = a_ref,
      cv_prior    = data.table::first(cv_prior),
      last_date   = max(date, na.rm = TRUE))
  }, by = .(athlete_id, event_id)]

  ab <- ab[n >= min_results & !is.na(ability_raw)]
  if (!nrow(ab)) return(estimate_ability(results[0], as_of, half_life, trim_tactical, min_results))

  # Event-level priors drive the shrinkage strength
  ab[, `:=`(
    prior_mu = mean(ability_raw, na.rm = TRUE),
    sigma_between = stats::sd(ability_raw, na.rm = TRUE)
  ), by = event_id]

  ab[, sigma := data.table::fifelse(
    is.na(sigma_raw) | sigma_raw <= 0, cv_prior, sigma_raw
  )]
  # Blend the observed spread toward the event prior; a two-race athlete's
  # sample SD is close to meaningless on its own.
  ab[, sigma := (n_eff * sigma + 2 * cv_prior) / (n_eff + 2)]

  ab[, sigma_between := data.table::fifelse(
    is.na(sigma_between) | sigma_between <= 0, sigma, sigma_between
  )]
  ab[, kappa := (sigma^2) / (sigma_between^2)]
  # Shrink on total weight, not n_eff: a decade-old record carries almost no
  # weight and should regress to the event mean regardless of how many results
  # it contains. This is what makes stale athletes fall out of contention on
  # their own, rather than needing a hand-set staleness cutoff.
  ab[, shrinkage := kappa / (w_total + kappa)]
  ab[, ability := (1 - shrinkage) * ability_raw + shrinkage * prior_mu]

  ab[, c("athlete_id", "event_id", "ability", "ability_raw", "sigma",
         "n", "n_eff", "w_total", "shrinkage", "age_ref", "last_date"),
     with = FALSE][]
}


#' Drop the worst-performing fraction of a set of results
#' @keywords internal
#' @noRd
.trim_worst <- function(perf, w, trim) {
  if (is.na(trim) || trim <= 0 || length(perf) < 4L) return(seq_along(perf))
  k <- floor(length(perf) * trim)
  if (k < 1L) return(seq_along(perf))
  ord <- order(perf)             # ascending: worst first on the oriented scale
  seq_along(perf)[-ord[seq_len(k)]]
}

#' @keywords internal
#' @noRd
.weighted_sd <- function(x, w) {
  if (length(x) < 2L || !sum(w > 0)) return(NA_real_)
  mu <- stats::weighted.mean(x, w)
  v <- sum(w * (x - mu)^2) / (sum(w) - sum(w^2) / sum(w))
  if (!is.finite(v) || v < 0) return(NA_real_)
  sqrt(v)
}
