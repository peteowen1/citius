#' Flag marks that cannot be genuine performances
#'
#' Feeds contain entries that are not performances at all: pole vaults recorded
#' as `0.03` metres and shot puts as `0.16` are no-marks, and a 25.13 second
#' 100 metres is an athlete who pulled up. Left in place these are catastrophic
#' on a log scale — a single 0.03 sits seven natural logs from a genuine 5.50 —
#' and they corrupt exactly the variance estimates the package exists to
#' produce.
#'
#' Detection uses the Hampel identifier: a mark is implausible when it lies more
#' than `k` scaled median absolute deviations from its event's median, on the
#' performance scale. The rule is deliberately a standard robust-statistics
#' convention applied uniformly, not a per-sport threshold — no judgement about
#' what a "reasonable" vault or sprint is enters the model. MAD is used rather
#' than standard deviation precisely because the contaminating values are
#' extreme enough to wreck a non-robust scale estimate.
#'
#' Flagged marks become `NA`, which is the honest encoding: a failure is a
#' missing performance, not a bad one. For technical events this is also what
#' makes the foul rate measurable.
#'
#' @param results Canonical results.
#' @param k Number of scaled MADs beyond which a mark is flagged. The
#'   conventional Hampel value is 3; a looser default is used here so that
#'   genuinely poor but real performances survive.
#' @return The input with implausible `mark` and `perf` values set to `NA` and a
#'   logical `implausible` column added.
#' @export
flag_implausible <- function(results, k = 5) {
  dt <- data.table::copy(if (data.table::is.data.table(results)) results
                         else data.table::as.data.table(results))
  if (!nrow(dt)) {
    dt[, implausible := logical()]
    return(dt[])
  }

  dt[, implausible := FALSE]
  dt[!is.na(perf), implausible := {
    med <- stats::median(perf)
    scaled_mad <- 1.4826 * stats::median(abs(perf - med))
    if (!is.finite(scaled_mad) || scaled_mad <= 0) rep(FALSE, .N)
    else abs(perf - med) > k * scaled_mad
  }, by = event_id]

  n_flag <- sum(dt$implausible, na.rm = TRUE)
  if (n_flag) {
    cli::cli_alert_info(
      "Flagged {n_flag} implausible mark{?s} ({round(100 * n_flag / nrow(dt), 2)}%) as missing."
    )
  }
  dt[implausible == TRUE, `:=`(mark = NA_real_, perf = NA_real_)]
  dt[]
}


#' Reconstruct race groupings from athlete-level histories
#'
#' Calibration needs to know which performances shared a race. The
#' competition-level endpoint returns only a subset of events, so the practical
#' route is to harvest many athletes individually and recover the groupings:
#' two performances belong to the same race when they share a competition, an
#' event, a round and a date.
#'
#' Coverage is a function of overlap. Harvesting eight sprinters recovers only
#' the races those eight happened to share; harvesting two hundred recovers
#' most championship fields. Races left with a single athlete contribute nothing
#' to the shared-shock estimate and are simply carried through.
#'
#' @param results Canonical results from [athletics_athlete_results()] or
#'   [aquatics_results()], typically stacked across many athletes.
#' @return The input with a `race_key` column added.
#' @seealso [calibrate()]
#' @examples
#' \dontrun{
#' histories <- rbindlist(lapply(ids, athletics_athlete_results))
#' cal <- calibrate(add_race_key(histories))
#' }
#' @export
add_race_key <- function(results) {
  dt <- data.table::copy(if (data.table::is.data.table(results)) results
                         else data.table::as.data.table(results))
  comp <- if ("competition_id" %in% names(dt)) dt$competition_id else NA
  rnd <- if ("round" %in% names(dt)) dt$round else NA
  dt[, race_key := paste(comp, event_id, rnd, as.character(date), sep = "|")]
  dt[]
}


#' Split performances into athlete, race and residual components
#'
#' Fits the two-way decomposition `perf = a_athlete + c_race + e` by alternating
#' projections: hold race effects fixed and average within athlete, hold athlete
#' effects fixed and average within race, repeat to convergence.
#'
#' This is the identification step that everything empirical in the package
#' depends on. A history organised by athlete can only ever tell you how much
#' someone varies; it cannot distinguish an off day from a race that was slow
#' for the entire field. Once whole fields are observed, the part of a
#' performance common to everyone in the race is separable from the part
#' specific to the athlete — and those two quantities need completely different
#' treatment in simulation.
#'
#' Race effects are centred to mean zero, which resolves the additive
#' confounding between `a` and `c`.
#'
#' @param results Canonical results containing a `race_key` column identifying
#'   which performances shared a race.
#' @param max_iter Maximum alternating sweeps.
#' @param tol Convergence tolerance on the maximum change in race effects.
#' @return A list with an `ability` table (one row per athlete-event), a `race`
#'   effect table, the augmented `data` carrying `resid`, and `converged`.
#' @export
decompose_races <- function(results, max_iter = 400L, tol = 1e-8) {
  dt <- data.table::as.data.table(results)
  if (!"race_key" %in% names(dt)) {
    cli::cli_abort("{.arg results} must contain a {.field race_key} column; use {.fn athletics_competition_results}.")
  }
  # Rows without a canonical event must go. Ability is indexed by athlete *and*
  # event, so every NA-event row for one athlete collapses into a single group —
  # pooling, say, a relay leg with a marathon. The resulting residuals are
  # enormous and poison every context offset computed downstream. On a real
  # harvest this inverted the round and tier effects entirely, reporting heats
  # as 14% faster than finals.
  n_before <- nrow(dt)
  dt <- dt[!is.na(perf) & !is.na(athlete_id) & !is.na(race_key) & !is.na(event_id)]
  dropped <- n_before - nrow(dt)
  if (dropped > 0.2 * n_before) {
    cli::cli_warn(c(
      "Dropped {dropped} of {n_before} row{?s} ({round(100 * dropped / n_before)}%) lacking a canonical event or performance.",
      i = "Relays and unrecognised events resolve to {.code NA} {.field event_id} by design."
    ), .frequency = "once", .frequency_id = "citius_decompose_dropped")
  }
  if (!nrow(dt)) return(list(ability = NULL, race = NULL, data = dt, converged = TRUE))

  dt[, athlete_id := as.character(athlete_id)]

  # A race containing a single harvested athlete carries no information about
  # what was shared: any deviation is equally explicable as a slow race or a bad
  # day. Giving it a free race effect would fit that deviation exactly, driving
  # its residual to zero while still consuming a degree of freedom — which
  # silently inflates every variance estimate downstream. Such races keep a race
  # effect of zero, so their deviation stays honestly in the residual.
  dt[, n_in_race := .N, by = race_key]
  dt[, shared := n_in_race >= 2L]
  dt[, c_r := 0]

  # Group once, outside the loop. Both groupings key on character columns, which
  # data.table must re-hash on every sweep; integer group ids make each sweep a
  # radix pass instead.
  dt[, ae_id := .GRP, by = .(athlete_id, event_id)]
  dt[, rk_id := .GRP, by = race_key]

  converged <- FALSE
  for (i in seq_len(max_iter)) {
    # Ability is per athlete *per event*: a sprinter's 100m and 200m marks live
    # on entirely different scales, and pooling them puts the gap between the
    # two distances into the residual, where it masquerades as day-to-day noise.
    dt[, a_i := mean(perf - c_r), by = ae_id]
    new_c <- dt[shared == TRUE, .(c_new = mean(perf - a_i)), by = rk_id]
    if (!nrow(new_c)) { converged <- TRUE; break }
    new_c[, c_new := c_new - mean(c_new)]        # centre: resolves the confounding
    # An update join, NOT a merge. `merge()` here rebuilt the entire table --
    # every row, every column -- on each of up to 50 sweeps, purely to attach one
    # number per race. This writes in place.
    dt[, c_new := 0]                             # races with no fitted effect
    dt[new_c, on = "rk_id", c_new := i.c_new]
    delta <- max(abs(dt$c_new - dt$c_r), na.rm = TRUE)
    dt[, c_r := c_new]
    if (is.finite(delta) && delta < tol) { converged <- TRUE; break }
  }

  dt[, c("c_new", "ae_id", "rk_id") := NULL]
  dt[, resid := perf - a_i - c_r]

  race <- dt[shared == TRUE, .(c_r = data.table::first(c_r), n_in_race = .N,
                 event_id = data.table::first(event_id),
                 round = if ("round" %in% names(dt)) data.table::first(round) else NA_character_,
                 tier = if ("tier" %in% names(dt)) data.table::first(tier) else NA_character_),
             by = race_key]
  # Ability is indexed by athlete-event; sensitivity, below, is a property of
  # the athlete and is estimated separately across all their events.
  ability <- dt[, .(a_i = data.table::first(a_i), n = .N), by = .(athlete_id, event_id)]

  # A decomposition that has not converged is a weaker foundation than every
  # variance estimate built on it assumes -- sigma_within, condition_sd, the
  # round precisions and tail_df are all computed from these residuals. The flag
  # was returned from the start and read by nobody, so it silently reported
  # FALSE on the swimming corpus while the numbers were quoted as measured.
  # Report the shortfall against the scale that matters rather than in the
  # abstract: 1e-3 is meaningless next to an athletics sigma of 0.038 and is a
  # fifth of a swimming sigma of 0.0073.
  # Measured on the swimming corpus: at the old default of 50 sweeps
  # condition_sd came out 7.4% LOW and sigma_within 0.6% high; by 400 both are
  # within 0.5% of their converged values. The absolute tolerance of 1e-8 is not
  # reachable in any practical number of sweeps -- alternating projections
  # converge linearly and this data contracts at ~0.98 per sweep -- so 400 is
  # chosen from where the estimates stop moving, not from where delta stops.
  if (!converged) {
    sigma_hint <- stats::sd(dt$resid, na.rm = TRUE)
    cli::cli_warn(c(
      "Two-way decomposition did not converge in {max_iter} sweep{?s}.",
      "!" = "Race effects still moving by {signif(delta, 3)}              ({round(100 * delta / sigma_hint)}% of the residual sd).",
      i = "Every variance estimate downstream is computed from these residuals."
    ), .frequency = "once", .frequency_id = "citius_decompose_converge")
  }
  list(ability = ability[], race = race[], data = dt[], converged = converged,
       delta = delta, sweeps = i)
}


#' Estimate every model parameter from observed results
#'
#' Replaces assumed constants with quantities measured from data. Per event it
#' recovers the within-athlete residual spread, the standard deviation of the
#' shared race shock, and how strongly races skew slow; pooled across events it
#' recovers round and tier offsets and how *informative* each context is.
#'
#' The shared-shock variance is estimated by method of moments. Observed race
#' effects carry sampling noise — a race with three athletes gives a much
#' noisier estimate than one with sixty — so the raw variance of fitted race
#' effects overstates the true shock. Subtracting the expected sampling
#' contribution `var(e)/n_r` corrects for this; without that correction, small
#' fields alone would manufacture apparent condition variance.
#'
#' Context *weights* are precisions rather than preferences: a round whose
#' residuals are noisy tells you less about ability and is downweighted
#' accordingly. Nothing is asserted about heats being less important — it falls
#' out of how predictable they are.
#'
#' @param results Canonical results with a `race_key` column.
#' @param min_races Minimum races for an event to be calibrated from data
#'   rather than falling back to the registry prior.
#' @return An object of class `citius_calibration`.
#' @seealso [race_conditions()], [condition_sensitivity()], [estimate_ability()]
#' @export
calibrate <- function(results, min_races = 8L) {
  results <- .drop_best_only(results, "calibrate()")
  dec <- decompose_races(results)
  if (is.null(dec$race) || !nrow(dec$race)) {
    return(.empty_calibration())
  }
  d <- dec$data

  # Residual variance per event, with a degrees-of-freedom correction for the
  # athlete and race effects that were fitted out.
  ev_stats <- d[, {
    n <- .N
    # One fitted ability per athlete-event, not per athlete.
    n_a <- data.table::uniqueN(athlete_id)
    # Only races that actually received a fitted effect cost a degree of freedom.
    n_r <- data.table::uniqueN(race_key[shared])
    df <- max(n - n_a - n_r + 1L, 1L)
    .(sigma_within = sqrt(sum(resid^2) / df),
      n_results = n, n_athletes = n_a, n_races = n_r)
  }, by = event_id]

  # Shared-shock sd, de-biased for the sampling noise in each fitted race effect.
  race_stats <- merge(dec$race, ev_stats[, .(event_id, sigma_within)],
                      by = "event_id", all.x = TRUE)
  cond <- race_stats[n_in_race >= 2L, {
    v_hat <- stats::var(c_r)
    bias <- mean(data.table::first(sigma_within)^2 / n_in_race)
    .(condition_sd = sqrt(max(v_hat - bias, 0)),
      tactical_index = .skewness(c_r),
      n_multi_races = .N)
  }, by = event_id]

  ev <- merge(ev_stats, cond, by = "event_id", all.x = TRUE)
  ev[is.na(n_multi_races), n_multi_races := 0L]
  ev[, cond_share := data.table::fifelse(
    is.na(condition_sd) | sigma_within <= 0, NA_real_,
    condition_sd^2 / (condition_sd^2 + sigma_within^2))]
  ev[, calibrated := n_multi_races >= min_races]

  # Rate of recording no valid performance — a field foul-out, or a track DNF.
  # Two things must hold for this to be measurable, and both are easy to get
  # wrong:
  #
  # 1. The input must be *unfiltered*. A no-mark is an NA performance, so
  #    dropping NAs first makes every event look clean.
  # 2. The input must come from [athletics_competition_results()]. The athlete-level
  #    endpoint silently omits results with no valid mark — an elite pole
  #    vaulter's 209-result history contains zero, which cannot be true — so an
  #    athlete-only harvest always measures zero no matter how large it is.
  raw <- data.table::as.data.table(results)
  fouls <- raw[, .(foul_rate = mean(is.na(perf))), by = event_id]
  ev <- merge(ev, fouls, by = "event_id", all.x = TRUE)

  if (nrow(ev) && all(ev$foul_rate == 0, na.rm = TRUE)) {
    cli::cli_warn(
      c("No missing performances anywhere; no-mark rates will all be zero.",
        i = "Harvest with {.fn athletics_competition_results} and do not filter {.code NA} marks before calibrating."),
      .frequency = "once", .frequency_id = "citius_no_missing_perf"
    )
  }

  ctx <- .context_stats(d)
  athlete <- .athlete_sensitivity(d, ev)
  tail_fit <- fit_tail_df(list(data = d))
  tail_df <- if (nrow(tail_fit)) tail_fit$df[1] else NA_real_

  structure(list(
    events = ev[],
    round = ctx$round,
    tier = ctx$tier,
    ability = dec$ability,
    athlete = athlete,
    tail_df = tail_df,
    # Measured separately by fit_form_sd(), which needs held-out meets and is
    # therefore too expensive to run inside calibrate(). Left NULL here and
    # attached by the calibration pipeline; the simulator treats NULL as zero,
    # which is the honest fallback rather than a guess.
    form_sd = NULL,
    race = dec$race,
    min_races = min_races,
    converged = dec$converged
  ), class = "citius_calibration")
}

#' Estimate how heavy the performance tails are
#'
#' Fits the degrees of freedom of the scaled-t distribution that
#' [simulate_event()] draws performances from, by matching the *observed
#' probability of moderate deviations* rather than a moment.
#'
#' Kurtosis is deliberately not used. Real result feeds contain a handful of
#' grossly wrong values, and the fourth moment is so dominated by them that
#' measured excess kurtosis reached 32 on championship data — implying `df`
#' around 4 when the actual tail mass sits only slightly above normal. Following
#' that would have made the simulator *more* dispersed, worsening the
#' under-confidence it was meant to fix.
#'
#' What matters for who wins a race is the shoulder of the distribution, not its
#' extremes, so `df` is chosen to minimise discrepancy in `P(|z| > k)` across
#' moderate `k`. On championship athletics data the old hard-coded `df = 6` put
#' roughly three times too much mass beyond two standard deviations.
#'
#' @param results Canonical results with a `race_key`, or a decomposition from
#'   [decompose_races()].
#' @param candidates Degrees of freedom to evaluate; `Inf` represents a normal.
#' @param probes Deviation sizes, in standard deviations, to match on.
#' @param clip Standardised residuals beyond this are treated as data errors and
#'   excluded. They are far outside any plausible performance and exist to stop
#'   feed corruption driving the fit.
#' @return A `data.table` of `df` and fit error, best first.
#' @export
fit_tail_df <- function(results, candidates = c(4, 5, 6, 8, 10, 15, 20, 30, 50, Inf),
                        probes = c(1.5, 2, 2.5, 3), clip = 8) {
  d <- if (is.list(results) && !is.null(results$data)) results$data else {
    dec <- decompose_races(.drop_best_only(results, "fit_tail_df()")); dec$data
  }
  d <- data.table::as.data.table(d)
  if (!nrow(d) || !"resid" %in% names(d)) {
    return(data.table::data.table(df = numeric(), err = numeric()))
  }
  if ("shared" %in% names(d)) d <- d[shared == TRUE]

  d <- d[is.finite(resid)]
  d[, z := resid / stats::sd(resid), by = event_id]
  z <- d$z[is.finite(d$z) & abs(d$z) < clip]
  if (length(z) < 100L) return(data.table::data.table(df = numeric(), err = numeric()))

  observed <- vapply(probes, function(k) mean(abs(z) > k), numeric(1))

  scored <- data.table::rbindlist(lapply(candidates, function(v) {
    expected <- if (is.infinite(v)) {
      2 * stats::pnorm(-probes)
    } else {
      2 * stats::pt(-probes / sqrt(v / (v - 2)), df = v)
    }
    # Relative error, so the rarer probes are not swamped by the common ones.
    data.table::data.table(df = v, err = mean(abs(expected - observed) / observed))
  }))
  data.table::setorder(scored, err)
  scored[]
}


#' Estimate how strongly each athlete responds to shared race conditions
#'
#' Regresses each athlete's ability-centred performance on the fitted race
#' effect. A slope of 1 means they move exactly with the field; above 1 means
#' conditions hit them harder than average; below 1 means they are relatively
#' immune.
#'
#' Two corrections matter here and both are estimated, not assumed:
#'
#' **Attenuation.** The regressor is a *fitted* race effect carrying sampling
#' noise, which biases every slope toward zero. The correction rescales by the
#' ratio of observed to de-biased race-effect variance, recovering the slope
#' that would have been obtained against the true shock.
#'
#' **Shrinkage.** An athlete seen in six races has a slope estimate that is
#' mostly noise. Each slope is combined with the population mean by precision
#' weighting, where the noise variance comes from the residual scale and the
#' between-athlete variance is itself recovered by subtracting mean noise from
#' the observed spread of slopes. Athletes with thin records end up near 1;
#' only those with substantial evidence move away from it.
#'
#' @param d Decomposed data from [decompose_races()].
#' @param ev Per-event statistics.
#' @param athlete Athlete effect table.
#' @return A `data.table` with `athlete_id`, `sensitivity`, `sensitivity_raw`
#'   and `n_races`.
#' @keywords internal
#' @noRd
.athlete_sensitivity <- function(d, ev) {
  d <- data.table::copy(d)
  d <- merge(d, ev[, .(event_id, sigma_within, condition_sd)],
             by = "event_id", all.x = TRUE)
  d <- d[is.finite(c_r) & is.finite(resid) & shared == TRUE]
  if (!nrow(d)) {
    return(data.table::data.table(athlete_id = character(), sensitivity = numeric(),
                                  sensitivity_raw = numeric(), n_obs = integer()))
  }

  # perf - a_i is the quantity that should scale with the race effect.
  d[, y := resid + c_r]

  s <- d[, {
    sxx <- sum(c_r^2)
    .(slope = if (sxx > 0) sum(c_r * y) / sxx else NA_real_,
      sxx = sxx,
      sigma_e = data.table::first(sigma_within),
      n_races = .N)
  }, by = athlete_id]

  # Attenuation: observed race-effect variance exceeds the true shock variance.
  var_obs <- stats::var(d$c_r, na.rm = TRUE)
  var_true <- stats::median(d$condition_sd, na.rm = TRUE)^2
  infl <- if (is.finite(var_obs) && is.finite(var_true) && var_true > 0) {
    min(var_obs / var_true, 5)      # cap: a wild ratio means the shock is barely identified
  } else 1
  s[, slope_adj := slope * infl]

  # Precision-weighted shrinkage toward the population mean.
  s[, noise_var := data.table::fifelse(sxx > 0, sigma_e^2 / sxx, NA_real_)]
  prior_mu <- stats::weighted.mean(s$slope_adj, 1 / s$noise_var, na.rm = TRUE)
  if (!is.finite(prior_mu)) prior_mu <- 1
  between <- max(stats::var(s$slope_adj, na.rm = TRUE) - mean(s$noise_var, na.rm = TRUE), 1e-6)

  s[, sensitivity := (slope_adj / noise_var + prior_mu / between) /
      (1 / noise_var + 1 / between)]
  s[!is.finite(sensitivity), sensitivity := prior_mu]

  # The overall scale of sensitivity is not separately identified from the
  # shared-shock magnitude: doubling every sensitivity and halving
  # condition_sd gives identical performances. Only the relative pattern
  # carries information, so it is normalised to mean 1 and the magnitude is
  # left entirely to race_conditions().
  m <- mean(s$sensitivity, na.rm = TRUE)
  if (is.finite(m) && m > 0) s[, sensitivity := sensitivity / m]

  data.table::setnames(s, "slope_adj", "sensitivity_raw")
  s[, .(athlete_id, sensitivity, sensitivity_raw, n_obs = n_races)][]
}


#' Pooled round and tier offsets plus their precisions
#' @keywords internal
#' @noRd
.context_stats <- function(d) {
  d <- data.table::copy(d)
  d[, round_class := .round_class(if ("round" %in% names(d)) round else NA_character_)]
  d[, tier_class := .tier_class(if ("tier" %in% names(d)) tier else NA_character_)]

  mk <- function(by_col, ref) {
    s <- d[, .(offset = mean(resid + c_r), sd = stats::sd(resid), n = .N),
           by = c(by_col)]
    ref_off <- s[get(by_col) == ref]$offset
    if (!length(ref_off)) ref_off <- max(s$offset, na.rm = TRUE)
    s[, offset := offset - ref_off]
    s[, sd := data.table::fifelse(is.na(sd) | sd <= 0, stats::median(s$sd, na.rm = TRUE), sd)]
    # Precision weight: contexts with noisier residuals carry less information.
    s[, precision := (min(sd, na.rm = TRUE) / sd)^2]
    # Normalise to mean 1 across observations. Precision must express *relative*
    # informativeness only — if the typical context weighed less than 1, total
    # weight would no longer be on a "number of results" scale and the
    # empirical-Bayes shrinkage in estimate_ability() would over-shrink every
    # athlete toward the event mean.
    m <- stats::weighted.mean(s$precision, s$n)
    if (is.finite(m) && m > 0) s[, precision := precision / m]
    s[]
  }

  list(round = mk("round_class", "final"), tier = mk("tier_class", "top"))
}

#' @keywords internal
#' @noRd
.skewness <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3L) return(NA_real_)
  s <- stats::sd(x)
  if (!is.finite(s) || s <= 0) return(NA_real_)
  sum(((x - mean(x)) / s)^3) / n
}

#' @keywords internal
#' @noRd
.empty_calibration <- function() {
  structure(list(
    events = data.table::data.table(
      event_id = character(), sigma_within = numeric(), condition_sd = numeric(),
      tactical_index = numeric(), cond_share = numeric(), calibrated = logical(),
      foul_rate = numeric(), n_results = integer(), n_races = integer()),
    round = NULL, tier = NULL, athlete = NULL, race = NULL,
    min_races = 8L, converged = TRUE
  ), class = "citius_calibration")
}

#' @export
print.citius_calibration <- function(x, ...) {
  ev <- x$events
  cli::cli_h3("citius calibration")
  cli::cli_text("{nrow(ev)} event{?s} | {sum(ev$calibrated, na.rm = TRUE)} calibrated from data")
  if (nrow(ev)) {
    show <- ev[calibrated == TRUE][order(-n_races)]
    cols <- intersect(c("event_id", "sigma_within", "condition_sd", "cond_share",
                        "tactical_index", "n_races"), names(show))
    print(utils::head(show[, cols, with = FALSE], 10L))
  }
  invisible(x)
}


#' Look up a calibrated value with a documented fallback
#' @keywords internal
#' @noRd
.calibrated_value <- function(calibration, event_id, column, fallback) {
  if (is.null(calibration) || !inherits(calibration, "citius_calibration")) return(fallback)
  ev <- calibration$events
  idx <- match(event_id, ev$event_id)
  if (is.na(idx) || !isTRUE(ev$calibrated[idx])) return(fallback)
  val <- ev[[column]][idx]
  if (is.null(val) || !is.finite(val)) return(fallback)
  val
}
