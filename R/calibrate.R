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
  # Without a competition to separate them, every same-event/round/date result
  # worldwide collapses into one "race" and the decomposition reads a whole
  # day's meets as a single shared shock. That must never happen silently.
  if (!"competition_id" %in% names(dt)) {
    cli::cli_warn(c(
      "{.arg results} has no {.field competition_id}; race keys will pool every
       result sharing an event, round and date into one race.",
      i = "Harvest with a source that carries competition ids before calibrating."
    ))
  }
  # THIS KEY CANNOT SEPARATE SECTIONS, so it must never overwrite one that can.
  # Every heat of a championship round shares competition, event, round and date,
  # so deriving over the top of a real key merges them into one pseudo-race. That
  # is what happened to the athletics corpus until 2026-08-14: 27.6% of its races
  # had more than one winner, one "final" had 45, and every variance estimate
  # downstream was fitted on those merged fields. See
  # ../../docs/incidents/ for the write-up, and build_athletics_corpus.R for the
  # caller-side rule that preserves authoritative keys.
  if ("race_key" %in% names(dt) && any(!is.na(dt$race_key))) {
    cli::cli_warn(c(
      "{sum(!is.na(dt$race_key))} row{?s} already carry a {.field race_key}; overwriting it with a derived one.",
      i = "The derived key is competition|event|round|date and cannot tell one
           heat of a round from another. Preserve the existing key and derive
           only for rows that lack one."
    ), .frequency = "once", .frequency_id = "citius_race_key_overwrite")
  }
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
#' @param min_race_size Smallest field that may receive a fitted race effect.
#'   Below it a race keeps `c_r = 0` and its deviation stays in the residual —
#'   the same treatment singletons already get, for the same reason. Raising it
#'   trades coverage of the shared shock for a less noisy estimate of it; see
#'   the note on small fields in the details.
#' @return A list with an `ability` table (one row per athlete-event), a `race`
#'   effect table, the augmented `data` carrying `resid`, and `converged`.
#' @export
decompose_races <- function(results, max_iter = 400L, tol = 1e-8,
                            min_race_size = 2L,
                            centre = c("always", "auto")) {
  # `centre = "auto"` is the FIX for the non-convergence this function has always
  # had on athletics, MEASURED 2026-08-13. It is not the default only because
  # flipping it moves sigma_within, condition_sd, tail_df and every context
  # offset in every calibration, and no shipped number changes without an A/B.
  #
  # WHY "always" IS WRONG WHENEVER ANY RACE IS PINNED. Races below
  # `min_race_size` keep `c_r == 0` by construction, and a pinned race FIXES THE
  # ZERO POINT -- the additive confounding between `a` and `c` is already
  # resolved. Centring the free effects to mean zero then imposes a SECOND,
  # incompatible constraint. The system is over-determined, there is no fixed
  # point, and the iteration drifts forever: the pinned races pull the level one
  # way each sweep and the centring pushes it back.
  #
  # Measured on the men's 100m, 183,738 results, min_race_size = 4:
  #
  #   variant                     delta @400   delta @2000   sd(c_r) 400->2000
  #   centred (today)             3.21e-04     2.72e-04      +117.3%  never converges
  #   NOT centred                 1.36e-05     9.98e-09      +0.0%    converged, 1038 sweeps
  #   centred over all rows       3.23e-04     2.84e-04      +120.9%  never converges
  #
  # cor between the centred and uncentred answers is 0.40, so this is a WRONG
  # ANSWER, not a slow one. The converged race-effect sd is 0.01390 (1.40% of a
  # mark); the centred run was still climbing through 0.03646 at 2,000 sweeps.
  # Without the fix, `condition_sd` -- which is computed from these effects
  # rather than the residuals -- is unreliable. `sigma_within` and `tail_df` come
  # from the residuals and move only 0.38%, so they are largely unaffected.
  #
  # The uncentred solution has a non-zero mean (the pinned races define the
  # origin, not the average race), so any consumer comparing `c_r` across
  # decompositions must not assume it is centred.
  centre <- match.arg(centre)
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
  # The same argument extends past singletons. A two-athlete race fits its effect
  # from two observations, so most of what it "measures" is the pair's own noise.
  # The de-biasing step subtracts var(e)/n_r, but that correction is itself
  # noisiest exactly where it is largest, and test-calibrate.R records residual
  # upward bias that survives it at small field sizes. `min_race_size` is the
  # threshold below which a race is treated like a singleton rather than trusted.
  dt[, n_in_race := .N, by = race_key]
  dt[, shared := n_in_race >= as.integer(min_race_size)]
  dt[, c_r := 0]

  # Group once, outside the loop. Both groupings key on character columns, which
  # data.table must re-hash on every sweep; integer group ids make each sweep a
  # radix pass instead.
  dt[, ae_id := .GRP, by = .(athlete_id, event_id)]
  dt[, rk_id := .GRP, by = race_key]

  converged <- FALSE
  # Initialised because the loop can break before `delta` is ever assigned --
  # when no race reaches `min_race_size`, `new_c` is empty on the first sweep.
  # The return list reads `delta` unconditionally, so an all-singleton input
  # errored with "object 'delta' not found" rather than reporting no race
  # structure.
  delta <- NA_real_
  i <- 0L
  for (i in seq_len(max_iter)) {
    # Ability is per athlete *per event*: a sprinter's 100m and 200m marks live
    # on entirely different scales, and pooling them puts the gap between the
    # two distances into the residual, where it masquerades as day-to-day noise.
    dt[, a_i := mean(perf - c_r), by = ae_id]
    new_c <- dt[shared == TRUE, .(c_new = mean(perf - a_i)), by = rk_id]
    if (!nrow(new_c)) { converged <- TRUE; break }
    # Centre only when it is actually needed. With any race pinned at zero the
    # level is already identified and this constraint is redundant AND harmful --
    # see the note at the top of this function.
    if (identical(centre, "always") || all(dt$shared)) {
      new_c[, c_new := c_new - mean(c_new)]      # centre: resolves the confounding
    }
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
#' @param min_race_size Smallest field that may receive a fitted race effect,
#'   passed to [decompose_races()].
#' @param context_per_family,context_per_event Fit round and tier offsets at
#'   family or event grain as well as pooled, passed to
#'   [estimate_context_effects()]. Both off by default: per-family was refuted by
#'   the `cstack` and `cround` arms, and per-event is untested. See that
#'   function for why the two are not the same bet.
#' @param context_season,context_indoor Fit the seasonal-phase and
#'   indoor/outdoor offsets, via [fit_season_effect()] and
#'   [fit_indoor_effect()]. **Both off by default because the pair was measured
#'   and rejected**, not because they are untested.
#'
#'   A 250-meet A/B on 2026-08-04 — two arms off one calibration differing in
#'   exactly these two elements — made placings significantly *worse* in every
#'   population while improving marks: gold Brier +2.02% on majors
#'   (p = 0.00059), +0.74% across all 948 scored finals (p = 0.00019), with the
#'   favourite-wins rate falling from 50.3% to 49.6%. Marks moved the other way,
#'   significantly on T2 (MAE centred -0.61%, p = 2.4e-05).
#'
#'   The earlier out-of-sample result that motivated the season offsets
#'   (-0.66% relative RMSE, 2026-07-30) measured **marks only** and never tested
#'   placings, which is where the cost lands.
#'
#'   Likely mechanism, already documented for `momentum` in [estimate_ability()]:
#'   stripping a component from history without adding it back for the target
#'   race forecasts every athlete in an average state of readiness, which is
#'   wrong for exactly the athletes who peak for a championship. Seasonal phase
#'   and current form are confounded, so removing the season strips form signal
#'   with it.
#'
#'   **The A/B moved both elements together**, so the damage is not attributed
#'   between them; separating them needs its own run. Turn either on only to
#'   re-measure, never to deploy.
#' @return An object of class `citius_calibration`.
#' @seealso [race_conditions()], [condition_sensitivity()], [estimate_ability()]
#' @export
calibrate <- function(results, min_races = 8L, min_race_size = 2L,
                      context_per_family = FALSE, context_per_event = FALSE,
                      context_season = FALSE, context_indoor = FALSE,
                      centre = c("always", "auto"), max_iter = 400L) {
  centre <- match.arg(centre)
  # `centre` and `max_iter` are exposed so the convergence fix is REACHABLE from
  # a calibration build. Both defaults reproduce the previous behaviour exactly.
  # They travel together on purpose: `centre = "auto"` converges on the men's
  # 100m only after ~1,038 sweeps, so passing it with the 400 default would
  # produce an unconverged fit under a name that claims otherwise -- a worse
  # failure than the one being fixed.
  if (identical(centre, "auto") && max_iter <= 400L) {
    cli::cli_warn(c(
      "{.code centre = \"auto\"} with {.code max_iter = {max_iter}} may not converge.",
      i = "The uncentred fit needed 1,038 sweeps on the men's 100m. Check {.field converged} on the result."
    ))
  }
  results <- .drop_best_only(results, "calibrate()")
  dec <- decompose_races(results, min_race_size = min_race_size,
                         centre = centre, max_iter = max_iter)
  if (is.null(dec$race) || !nrow(dec$race)) {
    return(.empty_calibration())
  }
  d <- dec$data

  # Residual variance per event, with a degrees-of-freedom correction for the
  # athlete and race effects that were fitted out.
  # `sigma_within` is the spread that remains once the race effect is removed.
  # Rows in races that never received a fitted effect — singletons, and anything
  # under `min_race_size` — have had NOTHING removed, so their residual still
  # carries the full shared shock. Pooling them inflates the estimate by exactly
  # the quantity `condition_sd` is separately trying to measure, and the two then
  # disagree. It also made raising `min_race_size` look useless: on the athletics
  # corpus condition_sd fell 69% -> 10% while sigma_within rose 15% -> 25%, which
  # is the same variance moving house rather than any bias being removed.
  ev_stats <- d[shared == TRUE, {
    n <- .N
    # One fitted ability per athlete-event, not per athlete.
    n_a <- data.table::uniqueN(athlete_id)
    n_r <- data.table::uniqueN(race_key)
    df <- max(n - n_a - n_r + 1L, 1L)
    .(sigma_within = sqrt(sum(resid^2) / df),
      n_shared = n, n_athletes = n_a, n_races = n_r)
  }, by = event_id]
  # Total rows are still reported, so the share the estimate rests on is visible.
  ev_stats <- merge(ev_stats, d[, .(n_results = .N), by = event_id],
                    by = "event_id", all = TRUE)
  ev_stats[is.na(n_shared), `:=`(n_shared = 0L, n_races = 0L)]

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
  # 3. When competition-level and athlete-level rows are pooled into one corpus,
  #    only the competition rows can carry a no-mark at all, so the athlete rows
  #    act as a mass of clean denominators and drive every rate toward zero.
  #    Nothing errors and the output is still a plausible-looking rate. A
  #    `nomark_observable` column marks the rows where an absent mark is
  #    information rather than an absence of data; where it is present the
  #    denominator is restricted to those rows.
  raw <- data.table::as.data.table(results)
  if ("nomark_observable" %in% names(raw)) {
    obs <- raw[nomark_observable %in% TRUE]
    if (nrow(obs)) {
      cli::cli_inform(
        "No-mark rates measured on {format(nrow(obs), big.mark = ',')} of {format(nrow(raw), big.mark = ',')} rows where a no-mark is observable."
      )
      raw <- obs
    }
  }
  fouls <- raw[, .(foul_rate = mean(is.na(perf))), by = event_id]
  ev <- merge(ev, fouls, by = "event_id", all.x = TRUE)

  foul_round <- if ("round" %in% names(raw)) {
    raw[, .rc := .round_class(round)]
    fr <- raw[!is.na(.rc), .(foul_rate = mean(is.na(perf)), n_obs = .N), by = .(event_id, round_class = .rc)]
    raw[, .rc := NULL]
    fr <- merge(fr, fouls[, .(event_id, global_foul = foul_rate)], by = "event_id", all.x = TRUE)
    # Pseudo-count blend toward the event's global rate. 30 is a PLACEHOLDER,
    # never fitted -- and nothing currently reads `foul_round` (it is on the
    # wiring guard's KNOWN_UNREAD register), so fitting it before a consumer
    # exists would be measuring a dead layer. Named here so the day a consumer
    # is wired, the constant is one grep away rather than a bare literal.
    foul_pool_n <- 30
    fr[, foul_rate := (n_obs * foul_rate + foul_pool_n * global_foul) / (n_obs + foul_pool_n)]
    fr[, global_foul := NULL]
    fr[]
  } else NULL

  if (nrow(ev) && all(ev$foul_rate == 0, na.rm = TRUE)) {
    cli::cli_warn(
      c("No missing performances anywhere; no-mark rates will all be zero.",
        i = "Harvest with {.fn athletics_competition_results} and do not filter {.code NA} marks before calibrating."),
      .frequency = "once", .frequency_id = "citius_no_missing_perf"
    )
  }

  # Wind, fitted here so the calibration object carries EVERY measured effect and
  # `estimate_ability()` has one place to read them from. Splitting the
  # measurement from the application is what let a fitted wind coefficient sit
  # unused while `adjust_wind()` appeared only inside a comment.
  wind <- if ("wind" %in% names(results)) {
    .fit_layer_or_warn(data.table::as.data.table(fit_wind_effect(results)), "wind")
  } else NULL

  cfam <- .fit_layer_or_warn(
    estimate_context_effects(results,
                             per_family = isTRUE(context_per_family),
                             per_event  = isTRUE(context_per_event)),
    "round_family/tier_family")
  if (is.null(cfam)) cfam <- list(round_family = NULL, tier_family = NULL,
                                  round_event = NULL, tier_event = NULL)
  ctx <- .context_stats(d)
  athlete <- .athlete_sensitivity(d, ev)
  tail_fit <- fit_tail_df(list(data = d))
  tail_df <- if (nrow(tail_fit)) tail_fit$df[1] else NA_real_

  structure(list(
    events = ev[],
    foul_round = foul_round,
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
    wind = wind,
    # Per-family round and tier offsets. The pooled versions average over events
    # that behave oppositely -- the low-tier penalty is -0.45% for road and
    # -3.59% for throws -- so a single value mis-adjusts both ends.
    round_family = cfam$round_family, tier_family = cfam$tier_family,
    round_event = cfam$round_event, tier_event = cfam$tier_event,
    # How far championship spread departs from pooled spread, per family. sigma
    # is fitted across the whole history but the forecast targets a top-tier
    # final, and those are different distributions -- narrower for field events,
    # wider for road. estimate_ability() applies this to the sigma it returns.
    sigma_context = .fit_layer_or_warn(fit_sigma_context(results), "sigma_context"),
    # How a global championship final differs from another top-tier final. Round
    # and tier offsets reference "final" and "top", so this distinction is
    # otherwise inexpressible -- and it is not zero.
    championship = .fit_layer_or_warn(fit_championship_effect(results), "championship"),
    # Indoor/outdoor and seasonal phase. Both were built, tested and — for season
    # — validated out of sample (offsets fitted pre-2020 improved 2020+ top-tier
    # final prediction by 0.66% relative RMSE), and then neither was ever
    # attached to a calibration. `estimate_ability()` has read `calibration$indoor`
    # and `calibration$season` the whole time; with nothing setting them, both
    # blocks were dead in every deployed run. That is the identical failure the
    # note above `wind` describes — a fitted coefficient sitting unused because
    # measurement and application live in different files — so they are attached
    # HERE, next to the other context effects, rather than by a pipeline script.
    indoor = if (isTRUE(context_indoor) && "indoor" %in% names(results)) {
      .fit_layer_or_warn(fit_indoor_effect(results), "indoor")
    } else NULL,
    # Needs `venue_country` to split the hemispheres. Without it every mark
    # classifies northern and southern athletes get a calendar six months out of
    # phase, so absence of the column is a reason to fit nothing, not a reason to
    # fit a pooled calendar.
    season = if (isTRUE(context_season) &&
                 all(c("date", "venue_country") %in% names(results))) {
      .fit_layer_or_warn(fit_season_effect(results), "season")
    } else NULL,
    min_races = min_races,
    min_race_size = min_race_size,
    converged = dec$converged,
    # Carried, not just warned about. The non-convergence warning is rate-limited
    # to once per session, so comparing three settings in one run showed the
    # delta from the FIRST only -- which is precisely when the number is needed.
    delta = dec$delta, sweeps = dec$sweeps
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
      # MULTIPLY by the t scale, do not divide (fixed 2026-08-14). The z-scores
      # are standardised to unit variance, and a unit-variance t_v is t/sqrt(
      # v/(v-2)), so P(|z| > k) = P(|t| > k * sqrt(v/(v-2))). The old division
      # overstated the expected tail mass of every low-df candidate, so
      # genuinely heavy-tailed residuals fitted as NEARLY NORMAL -- planted
      # df = 5 came back ranked worst of ten candidates while df = 30 won.
      # That is the opposite failure to the hard-coded df = 6 this fitter
      # replaced, and it was invisible on near-normal input, which is why no
      # earlier test caught it. NOTE: rebuilt calibrations will carry a lower
      # (fatter-tailed) tail_df than deployed ones; A/B before shipping one.
      2 * stats::pt(-probes * sqrt(v / (v - 2)), df = v)
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

  # Between-athlete variance by DerSimonian-Laird, NOT by subtracting the mean
  # noise from the observed spread.
  #
  # The naive moment estimator `var(slope_adj) - mean(noise_var)` is unusable
  # here because `noise_var` is sigma^2/sxx and `sxx` -- the sum of squared race
  # effects an athlete was exposed to -- approaches zero for anyone seen in a
  # couple of near-neutral races. Measured on the corpus 2026-07-31:
  #
  #   median(noise_var)   0.107
  #   mean(noise_var)   147000        <- 1.4 million times the median
  #   max(noise_var)      5.6e9
  #
  # so the subtrahend was set entirely by the least informative athletes in the
  # sample, `between` came out at -138000, pinned to its floor, and every
  # athlete shrank to the prior mean. The estimator was behaving correctly given
  # a corrupted noise estimate; the fault was using an unweighted mean over a
  # quantity with an unbounded tail.
  #
  # DL weights each athlete by their own precision, so an athlete with no
  # exposure to varying conditions contributes almost nothing instead of
  # dominating. It is the standard heterogeneity estimator for exactly this
  # shape of problem and introduces no tuned constant. Cross-check: it recovers
  # sd(between) = 0.286 on the corpus against the 0.235 measured on the 58k
  # harvest by the naive estimator back when that estimator still worked.
  ok <- is.finite(s$noise_var) & s$noise_var > 0 & is.finite(s$slope_adj)
  between <- 1e-6
  if (sum(ok) > 1L) {
    w <- 1 / s$noise_var[ok]
    th <- s$slope_adj[ok]
    mu_w <- sum(w * th) / sum(w)
    Q <- sum(w * (th - mu_w)^2)
    denom <- sum(w) - sum(w^2) / sum(w)
    tau2 <- if (denom > 0) (Q - (sum(ok) - 1L)) / denom else 0
    between <- max(tau2, 1e-6)
  }

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

  # A constant sensitivity is not a calibration, it is a failure to identify one
  # -- and a silent one, because everything downstream keeps working. With every
  # s_i equal, `s_i * c` degenerates to `c`, which cancels from every pairwise
  # comparison, so the shared shock becomes INERT for placings and only moves
  # marks. That is the single mechanism by which race conditions can reorder a
  # field, and losing it looks exactly like the shock being unimportant.
  #
  # Measured 2026-07-31: sd(sensitivity) == 0 on every calibration built before
  # that date, while sd(sensitivity_raw) was 1.60 -- `between` pinned to its
  # floor because the old moment estimator subtracted an outlier-dominated
  # `mean(noise_var)`. Fixed by the DerSimonian-Laird estimator above. If this
  # warning fires again the cause is NOT that, so check first whether the race
  # effects identify at all: `sxx` near zero for the whole population means the
  # decomposition found no shared shock to regress against.
  if (nrow(s) > 1L) {
    spread <- stats::sd(s$sensitivity, na.rm = TRUE)
    if (!is.finite(spread) || spread < 1e-8) {
      cli::cli_warn(c(
        "Condition sensitivity collapsed to a constant: the shared race shock cannot reorder any field.",
        i = "Between-athlete variance hit its floor, so every athlete shrank to the prior mean.",
        i = "`sd(sensitivity_raw)` is {signif(stats::sd(s$sensitivity_raw, na.rm = TRUE), 3)} and median `sxx` is {signif(stats::median(s$sxx, na.rm = TRUE), 3)}.",
        i = "If the raw slopes vary this is a shrinkage failure; if `sxx` is ~0 throughout, the decomposition found no shared shock to regress against."
      ), .frequency = "once", .frequency_id = "citius_sensitivity_collapsed")
    }
  }

  data.table::setnames(s, "slope_adj", "sensitivity_raw")
  s[, .(athlete_id, sensitivity, sensitivity_raw, n_obs = n_races)][]
}


#' Pooled round and tier offsets plus their precisions
#' @keywords internal
#' @noRd
.context_stats <- function(d) {
  d <- data.table::copy(d)
  d[, round_class := .round_class(if ("round" %in% names(d)) round else NA_character_)]
  d[, tier_class := .tier_class_of(d)]

  mk <- function(by_col, ref) {
    # `offset` telescopes to perf - a_i whether or not c_r was fitted, so it can
    # use every row. `sd` cannot: rows from singleton or sub-min_race_size races
    # never got a fitted c_r, so their residual still contains the whole race
    # shock. Pooling them inflates the sd by exactly the quantity condition_sd
    # measures separately -- the same trap already guarded for sigma_within and
    # in fit_tail_df(). It matters because these sd values ARE the round
    # precisions, and heats are likelier than finals to be only partly
    # harvested, so the bias runs along the very contrast being measured.
    ds <- if ("shared" %in% names(d)) d[shared == TRUE] else d
    sds <- ds[, .(sd = stats::sd(resid)), by = c(by_col)]
    s <- d[, .(offset = mean(resid + c_r), n = .N), by = c(by_col)]
    s <- merge(s, sds, by = by_col, all.x = TRUE)
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
    round = NULL, tier = NULL, athlete = NULL, race = NULL, wind = NULL,
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


#' Fit a calibration layer, and NEVER let it fail silently
#'
#' `calibrate()` wrapped five of its layer fitters in
#' `tryCatch(..., error = function(e) NULL)`. That converts a fitter error into
#' a silently absent layer -- which is this package's single recurring defect
#' class, and the reason `test-calibration-wiring.R` exists. The wiring guard
#' checks the deployed ARTEFACT, so a layer lost this way is caught only at
#' deployment and only if it is on the guard's registers; a build that loses
#' `sigma_context` to a refactor-induced error would otherwise print nothing.
#'
#' The catch is kept -- a broken optional layer should not abort a calibration
#' that is 95% usable -- but it now says what it dropped and why.
#'
#' @keywords internal
#' @noRd
.fit_layer_or_warn <- function(expr, layer) {
  tryCatch(expr, error = function(e) {
    cli::cli_warn(c(
      "Fitting calibration layer {.field {layer}} failed; this calibration will not carry it.",
      x = conditionMessage(e),
      i = "A silently absent layer is this package's recurring defect class. Treat this as a build failure unless the absence is intended."
    ))
    NULL
  })
}


#' Warn once when predicting from a calibration that never converged
#'
#' `decompose_races()` stamps `converged`, `delta` and `sweeps` into every
#' calibration, `rebaseline_chain.R` prints them as it writes the file, and until
#' 2026-08-13 **nothing read them again**. So every forecast, every published
#' rating and every backtest ran off whatever the solver happened to reach, with
#' no way to tell from the output.
#'
#' That is not hypothetical. The deployed calibration is `converged = FALSE` at
#' `delta = 1.66e-04` -- race effects still moving by 1% of the residual sd -- and
#' every variance estimate downstream is computed from those residuals.
#'
#' It warns rather than aborts because the unconverged fit is, measured, the one
#' that forecasts BETTER: `centre = "auto"` reached delta 2.5e-07 and lost on gold
#' Brier by +0.56% (p = 0.00078). So non-convergence here is a fact to surface,
#' not a fault to refuse on. See DECISIONS.md 2026-08-13.
#'
#' @keywords internal
#' @noRd
.warn_unconverged <- function(calibration) {
  if (is.null(calibration) || !inherits(calibration, "citius_calibration")) {
    return(invisible(FALSE))
  }
  # `isFALSE` not `!isTRUE`: an older calibration carrying no `converged` slot is
  # unknown, not failed, and must not be reported as if it had been measured.
  if (!isFALSE(calibration$converged)) return(invisible(FALSE))
  d <- calibration$delta
  s <- calibration$sweeps
  cli::cli_warn(c(
    "Predicting from a calibration whose decomposition did not converge.",
    "*" = "{.field delta} {.val {if (is.null(d)) NA else signif(d, 3)}} after
           {.val {if (is.null(s)) NA else s}} sweep{?s}.",
    i = "Every variance estimate downstream is computed from these residuals."
  ), .frequency = "once", .frequency_id = "citius_calibration_unconverged")
  invisible(TRUE)
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
