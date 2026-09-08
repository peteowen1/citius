# PLACEHOLDER CONSTANTS, named so each exists exactly once and its status is
# declared. The package rule is "no hand-tuned constants in the models"; these
# two are the surviving exceptions and each is a FALLBACK, not an estimate.
#
# Within-athlete CV used only when an event's `cv_prior` is absent or
# non-positive -- the same placeholder role `cv_prior` itself plays against a
# calibration. 0.02 is the order of magnitude of athletics CVs (measured
# sigma_within runs ~0.008-0.04 by event); it exists so the Huber cutoff is
# defined before any calibration does, and nothing more.
.CITIUS_FALLBACK_CV <- 0.02

# Pseudo-count for blending a thin athlete's sample sigma toward the event
# value: sigma <- (w * sigma + k * target) / (w + k) with k = 2. Never fitted --
# the `crob` arm that validated this path (-0.56% gold Brier, 2026-07-31) ran
# WITH k = 2 inside it, so the bundle is validated but the constant is not
# separately attributed. If evidence-depth weighting is ever revisited (its
# measured ceiling is -0.15% gold Brier; see the block comment in
# estimate_ability()), fitting k is the natural first arm.
.CITIUS_SIGMA_PSEUDO_N <- 2

# Env override for the pseudo-count, so an arm can test it without a code edit
# (2026-09-06: the shoot-out put pseudo-n 20-40 well ahead of 2 on hold-out
# allocation skill; the arm that decides is run through this). Unset = 2, the
# validated bundle above. Anything unparseable or negative falls back to 2 and
# says so, rather than silently running the default under a different label.
.sigma_pseudo_n <- function() {
  raw <- Sys.getenv("CITIUS_SIGMA_PSEUDO_N", "")
  if (!nzchar(raw)) return(.CITIUS_SIGMA_PSEUDO_N)
  v <- suppressWarnings(as.numeric(raw))
  if (!is.finite(v) || v < 0) {
    cli::cli_warn("CITIUS_SIGMA_PSEUDO_N={.val {raw}} is not a non-negative number; using {(.CITIUS_SIGMA_PSEUDO_N)}.")
    return(.CITIUS_SIGMA_PSEUDO_N)
  }
  v
}

# Env override for a global multiplier on the per-athlete sigma AFTER shrinkage
# and the per-family context ratio. Unset = 1. This exists so the PIT coverage
# check can find the scale at which the simulated spread is calibrated; the
# fitted value belongs in the calibration object, not in an env var, before it
# ships (the "no hand-tuned constants" rule).
# Families where a slow race can plausibly reflect RACING rather than conditions,
# and so where the calibration's tactical override is allowed to fire. Middle
# and distance are the textbook case; road and walk races are decided tactically
# over the closing kilometres; a combined event's individual marks are paced
# against the points table rather than contested flat out.
#
# Sprints, hurdles, jumps and throws are excluded: a slow 100m or a short shot
# put is weather or a bad day, never tactics, and the context adjustment already
# handles the former.
#
# SWIMMING IS EXCLUDED TOO, and that is a decision rather than an oversight.
# The registry carries swim_sprint, swim_distance, swim_middle and swim_im, and
# none is listed here, so the override can never fire for them. Distance
# swimming plausibly IS tactical -- a 1500m freestyle final is paced much like a
# 1500m on the track -- but no swimming event has been through the marks lab, so
# there is no measurement to justify including it. Add the swim families when
# there is one, not before.
.CITIUS_TACTICAL_FAMILIES <- c("middle", "distance", "road", "walk", "combined")

.sigma_marks_pseudo_n <- function() {
  raw <- Sys.getenv("CITIUS_SIGMA_MARKS_PSEUDO_N", "")
  if (!nzchar(raw)) return(40)
  v <- suppressWarnings(as.numeric(raw))
  if (!is.finite(v) || v < 0) {
    cli::cli_warn("CITIUS_SIGMA_MARKS_PSEUDO_N={.val {raw}} is not a non-negative number; using 40.")
    return(40)
  }
  v
}

# How much of a predicted MARK is the athlete's own recent form.
#
# 0 = the ranking ability alone, which is what the model did until 2026-09-07;
# 1 = the plain mean of their last five marks. This is a MARKS-ONLY term.
# `estimate_ability()` emits the ingredient, `recent_mean`; `simulate_event()`
# does the blending, and applies it only to the centre of the MARK
# distribution, never to `ability`, which the ranking is built from. A blend
# therefore cannot move a finishing order or a medal probability, and there is
# a test that asserts exactly that.
#
# DEFAULT 0: OFF. This is a DIAGNOSTIC LEVER, not a model component.
#
# It was briefly deployed at 0.5 on 2026-09-07 and turned off the same day, on
# Pete's objection, which is correct and worth keeping written down:
#
#   "You can't blend with a baseline to beat a baseline cause then you're
#    stealing the baseline's info."
#
# Three reasons it had to go, in increasing order of how much they matter:
#
#   1. A model containing the baseline cannot be honestly SCORED against that
#      baseline. Part of any measured win is shrinkage toward it, so the metric
#      stops measuring the thing it exists to measure.
#   2. It does nothing for an athlete with no recent history -- a debutant, a
#      comeback -- which is exactly where prediction is hardest and where the
#      underlying defect is fully exposed.
#   3. It could only ever be applied to MARKS, never to the ranking. That was
#      presented as a safety property. It is really the tell: a term that has to
#      be kept away from the quantity that decides medals is a patch on a
#      metric, not a model of anything. If recent form carries signal, it should
#      change who wins.
#
# What it DID establish, and what makes it worth keeping as a lever: mixing in a
# plain unweighted mean of five raw marks improves held-out mark error by ~4%,
# which means `ability` is systematically wrong as a point forecast in a way a
# dumb average is not. That gap is a measurement of a real defect. Set this
# above 0 only to re-measure that gap, never to ship.
# See docs/reviews/marks-blend-2026-09-07.md.
.marks_blend <- function() {
  raw <- Sys.getenv("CITIUS_MARKS_BLEND", "")
  if (!nzchar(raw)) return(0)
  v <- suppressWarnings(as.numeric(raw))
  if (!is.finite(v) || v < 0 || v > 1) {
    cli::cli_warn("CITIUS_MARKS_BLEND={.val {raw}} is not a number in [0, 1]; using 0.")
    return(0)
  }
  v
}

.sigma_scale_env <- function() {
  raw <- Sys.getenv("CITIUS_SIGMA_SCALE", "")
  if (!nzchar(raw)) return(1)
  v <- suppressWarnings(as.numeric(raw))
  if (!is.finite(v) || v <= 0) {
    cli::cli_warn("CITIUS_SIGMA_SCALE={.val {raw}} is not a positive number; using 1.")
    return(1)
  }
  v
}

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
#' @param tier World Athletics category of the RACE: `"OW"`, `"GL"`, `"GW"`,
#'   `"DF"`, `"A"`..`"F"`, or `NA`.
#'
#'   **Not `meet_tier`**, which is the catalogue's rating of a MEETING
#'   (`"T1_elite"`, `"T2_strong"`, `"T3_development"`). The two classifications
#'   cross rather than nest: one T1_elite meeting contains races of several WAC
#'   categories, because a Diamond League meeting's headline disciplines and its
#'   supporting programme are categorised separately. Weltklasse Zürich runs
#'   `"GW"` disciplines beside `"F"` support races inside a single T1_elite
#'   meeting, and 90 of the 849 races in the lab's "elite" test set are `"F"`
#'   for exactly that reason.
#'
#'   Diagnostics name it `race_tier` where both appear; the stored column keeps
#'   `tier`, since renaming it would invalidate a 7.5M-row parquet store.
#' @param round Character vector of round codes (`"F"`, `"SF1"`, `"H4"`, or
#'   World Aquatics `"Final"`/`"Heats"`).
#' @param as_of Reference date from which recency is measured.
#' @param half_life Days after which a result carries half weight.
#' @param calibration Optional `citius_calibration` from [calibrate()]. Supplies
#'   measured precisions for each round and tier. Without one, context weights
#'   are flat and only recency applies.
#' @param tier_class Optional pre-resolved tier class (`"top"`, `"mid"`,
#'   `"low"`, ...). Supply this when the caller can resolve the class better
#'   than the feed code allows — `estimate_ability()` passes the catalogue-aware
#'   class so that the weights and the offsets share one vocabulary. Pass the
#'   *class*, never a class routed back through `tier`: unrecognised codes map
#'   to `"mid"`, so double-mapping is silent.
#' @param precision_scale Exponent applied to the context precision (tier and
#'   round), leaving recency untouched. `1`, the default, is the calibration as
#'   fitted; `0` makes every mark count the same whatever meet it was set at.
#'   Measured best at **0** for mark prediction: the fitted weights measure which
#'   race most precisely pins down current ability, which is not the same as
#'   which race best predicts a championship final.
#' @return Numeric vector of non-negative weights.
#' @seealso [calibrate()]
#' @export
result_weight <- function(date, tier = NA_character_, round = NA_character_,
                          as_of = Sys.Date(), half_life = 540,
                          calibration = NULL, tier_class = NULL,
                          precision_scale = 1) {
  n <- length(date)
  age_days <- as.numeric(as_of - as.Date(date))
  # KNOWN CHOICE, not an oversight: an NA or future date gets age 0, i.e. FULL
  # recency weight -- a result with an unparseable date is treated as set
  # today. The conservative alternative (weight 0, excluding it) changes every
  # ability estimate built on a feed with missing dates, so it must ship
  # through a measured arm, not a review fix. Flagged 2026-08-14.
  age_days[is.na(age_days) | age_days < 0] <- 0
  recency <- 0.5^(age_days / half_life)

  tier <- rep_len(as.character(tier), n)
  round <- rep_len(as.character(round), n)

  # `tier_class` lets the caller pass a class that is already resolved -- in
  # practice .tier_class_of(), which prefers the catalogue's meet_tier. Without
  # it this function can only see the feed code, and the WAC promotion
  # (31aff53) made that a real divergence rather than a cosmetic one: the
  # OFFSETS moved to a three-class WAC vocabulary (top/mid/low) while the
  # WEIGHTS kept looking up the four-class feed one, which also emits "high".
  # "high" then matched nothing and silently took the median precision --
  # weighting ~469k of the least reliable rows in the corpus 29% too heavily
  # (fitted 0.7132 -> median 0.9211). Passing the class in is what keeps both
  # sides of a calibration speaking the same language.
  #
  # Do NOT route a resolved class back through `tier`: .tier_class() maps any
  # unrecognised code to "mid", so .tier_class("top") is "mid", and the
  # double-mapping would be silent.
  tc <- if (is.null(tier_class)) .tier_class(tier) else rep_len(as.character(tier_class), n)

  prec <- .context_precision(calibration, "round", .round_class(round)) *
    .context_precision(calibration, "tier", tc)
  # PRECISION_SCALE: an exponent on the context precision, recency untouched.
  #
  # An exponent rather than a multiplier because these are precisions, which
  # compose multiplicatively: halving it halves the log-ratio between trusting a
  # final and trusting a heat, which is the natural way to shrink a weight that
  # is itself a ratio. 1 is the calibration as fitted; 0 makes every mark count
  # the same whatever meet it was set at.
  #
  # Measured on the marks lab, held out on 44 events: the fitted weights (1)
  # beat 24 events separated, and switching them off (0) beats 28, with pooled
  # error 2.0624 -> 2.0368. All nine families fit 0 independently.
  #
  # The weights are not wrong, they answer a different question. They measure
  # which race most precisely pins down CURRENT ABILITY, and routine races at
  # weak meets genuinely scatter least -- a category F semi-final carries 3.7x a
  # Diamond League final. A forecast needs which race best predicts a
  # CHAMPIONSHIP FINAL, and up-weighting routine runs to predict a peak effort
  # is backwards.
  #
  # Default 1, so every existing caller is unchanged. See
  # docs/reviews/marks-optimisation-2026-09-07.md.
  if (!isTRUE(all.equal(precision_scale, 1))) prec <- prec^precision_scale
  recency * prec
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
  hit <- match(classes, tbl[[col]])

  # A label the table does not have is a VOCABULARY MISMATCH, not a novelty,
  # and it must be loud. The median substitution below is good defensive code
  # and it is exactly what hid the WAC promotion's worst side effect for two
  # days: moving the calibration to a three-class tier vocabulary deleted the
  # "high" bucket that result_weight() still asked for, so ~469k of the least
  # precise rows in the corpus silently weighted 29% too heavily (0.7132 ->
  # median 0.9211) with nothing failing and no NA anywhere.
  #
  # A fallback keyed on "did the lookup match" cannot tell an unknown label
  # from one the calibration USED TO HAVE. Naming the two vocabularies is what
  # makes the difference visible, so that is what this does.
  # WARN, not abort. Aborting is the instinct and it is wrong here: the known
  # remaining mismatch ("high" from the feed fallback, see .tier_class_of) is
  # real, is documented, and affects rows that must still be weighted somehow.
  # Killing the run would force a modelling change to be made under time
  # pressure, which is how the original defect shipped. Naming both
  # vocabularies is enough to stop it hiding for two days again.
  miss <- unique(classes[is.na(hit) & !is.na(classes)])
  if (length(miss)) {
    cli::cli_warn(c(
      "!" = "{.field {which}} class{?es} {.val {miss}} {?is/are} not in the calibration;
             taking the median precision.",
      "i" = "The calibration offers: {.val {unique(tbl[[col]])}}.",
      "i" = "A vocabulary mismatch, not a novelty: the substitution is silent in
             the numbers, so it is said out loud here. This is how the WAC
             promotion mis-weighted ~10% of the corpus for two days.",
      .frequency = "once", .frequency_id = paste0("citius_ctx_prec_", which)))
  }

  out <- tbl$precision[hit]
  # NA classes still take the median: those are genuinely unknown context, not
  # a mismatch, and refusing to weight them at all would drop the result.
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
                          # 90 was the old floor, and four families pinned to it
                          # and were reported unidentified. Adding points below
                          # showed the minimum is genuinely AT 90 for sprint,
                          # hurdles and throw — MAE rises again at 60 — so the
                          # boundary test was flagging a real optimum as an
                          # artefact purely because it equalled min(candidates).
                          # With the grid widened all nine families identify, and
                          # the four stop falling back to a pooled 207 days that
                          # was too long for every one of them.
                          candidates = c(14, 30, 45, 60, 90, 135, 180, 270, 365,
                                         540, 730, 1095, 1825, 3650),
                          min_history = 3L) {
  results <- .drop_best_only(results, "fit_half_life()")
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

  out <- if ("event_id" %in% names(hl)) {
    hl$half_life[match(event_id, hl$event_id)]
  } else {
    hl$half_life[match(fam, hl$family)]
  }
  if ("event_id" %in% names(hl) && "family" %in% names(hl)) {
    na_idx <- which(!is.finite(out))
    if (length(na_idx)) {
      fam_val <- hl$half_life[match(fam[na_idx], hl$family)]
      out[na_idx] <- fam_val
    }
  }
  out[!is.finite(out)] <- if (nrow(hl)) stats::median(hl$half_life) else default
  out
}


#' Resolve a per-event parameter to one value per row
#'
#' Three parameters now accept either a scalar or a table: `races_half_life`,
#' `trim_tactical` and `context_scale`. They resolve identically -- match on
#' `event_id`, fall back to `family`, and fall back again to an explicit
#' default -- so they share one resolver rather than three copies that drift.
#'
#' THE FALLBACK IS THE CALLER'S DEFAULT, NOT A MEDIAN of the table. A median
#' would quietly apply a value fitted on other events to one the table knows
#' nothing about; the documented default is the honest answer. ([.event_half_life()]
#' does use a median, and is left alone rather than changed underneath its
#' callers, but new parameters do not copy it.)
#'
#' @param event_id Character vector of event ids, one per row.
#' @param spec `NULL`, a scalar, or a table with `column` plus `event_id`
#'   and/or `family`.
#' @param column Name of the value column expected in `spec`.
#' @param default Value for rows the table does not cover.
#' @keywords internal
#' @noRd
.event_param <- function(event_id, spec, column, default) {
  if (is.null(spec)) return(rep(default, length(event_id)))
  if (is.numeric(spec)) return(rep_len(spec, length(event_id)))

  tb <- data.table::as.data.table(spec)
  if (!column %in% names(tb)) {
    cli::cli_abort("Table for {.arg {column}} needs a {.field {column}} column.")
  }
  reg <- .citius_event_registry
  fam <- reg$family[match(event_id, reg$event_id)]
  out <- if ("event_id" %in% names(tb)) {
    tb[[column]][match(event_id, tb$event_id)]
  } else {
    tb[[column]][match(fam, tb$family)]
  }
  if ("event_id" %in% names(tb) && "family" %in% names(tb)) {
    na_idx <- which(is.na(out))
    if (length(na_idx)) out[na_idx] <- tb[[column]][match(fam[na_idx], tb$family)]
  }
  out[is.na(out)] <- default
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
#' @param min_cell Minimum marks for a family-context cell to be estimated at
#'   all. Cells below this fall back to the pooled offset.
#' @param per_family Compute per-family round and tier offsets alongside the
#'   pooled ones. **Off by default: two backtest arms refuted it.** `cstack`
#'   (round + tier) lost to wind-only on MAE 2.2795% to 2.3205%, and `cround`
#'   (round alone, tier pooled) lost by more, 2.2795% to 2.3024% with gold and
#'   medal Brier both degraded and bias worse in every family. Kept behind a flag
#'   rather than deleted because the underlying observation is real — road's tier
#'   ordering genuinely inverts — but the parameterisation does not transfer.
#'   The distinction that DOES survive is [fit_championship_effect()].
#' @param per_event Compute per-EVENT round and tier offsets, shrunk toward the
#'   family offset where one exists and the pooled offset otherwise. Off by
#'   default and untested at the time of writing. Motivated by a measured split
#'   that per-family cannot represent: on T1 finals with data richness held
#'   fixed, the 100m beats a last-5 baseline by 10.8% while the 400m loses to it
#'   by 6.4%, and both are in the `sprint` family. `per_family` being refuted
#'   does not settle this — a family offset is wrong for the 400m however well
#'   it is estimated.
#' @param min_event_cell Minimum marks for an event-context cell. Lower than
#'   `min_cell` because event cells are inherently smaller, with the shrinkage
#'   rather than the threshold doing most of the work.
#' @param shrink Shrink per-family offsets toward the pooled offset by an
#'   empirical-Bayes weight whose strength is fitted out of sample. Only
#'   consulted when `per_family` is on. Note that this fitter validated round
#'   offsets at k = 0 and the backtest still refuted them, so it is a filter on
#'   the parameterisation rather than a licence for it.
#' @return A list with `round` and `tier` named numeric vectors of offsets on
#'   the log performance scale, plus the `n` behind each. When `shrink` is on,
#'   the per-family tables also carry `raw` (the unshrunk offset) and
#'   `shrink_k`, so the correction applied is auditable.
#' @export
estimate_context_effects <- function(results, min_cell = 2000L, shrink = TRUE,
                                     per_family = FALSE, per_event = FALSE,
                                     min_event_cell = 500L) {
  dt <- data.table::as.data.table(results)
  dt <- dt[!is.na(perf) & !is.na(event_id)]
  empty <- list(round = c(final = 0), tier = c(top = 0),
                round_family = NULL, tier_family = NULL,
                round_event = NULL, tier_event = NULL, n = 0L)
  if (!nrow(dt)) return(empty)

  dt[, athlete_id := as.character(athlete_id)]
  dt[, round_class := .round_class(if ("round" %in% names(dt)) round else NA_character_)]
  dt[, tier_class := .tier_class_of(dt)]

  # Centre within athlete-event: removes ability, leaving context + noise.
  dt[, resid := perf - mean(perf), by = .(athlete_id, event_id)]

  r_eff <- dt[, .(eff = mean(resid), n = .N), by = round_class]
  # Resolve the reference BEFORE subtracting. Subtracting first and then
  # testing for the reference made the fallback dead code: with no "final"
  # row, `eff[round_class == "final"][1]` is NA, so the subtraction turned the
  # whole column to NA, and the fallback's `max(eff)` on an all-NA column is
  # also NA. Every pooled offset came back NA and the caller silently treated
  # that as "no context adjustment" -- the opposite of the intended
  # reference-to-the-slowest-context behaviour. The per-family block below
  # already did it in this order.
  r_ref <- r_eff[round_class == "final", eff][1]
  if (!is.finite(r_ref)) r_ref <- max(r_eff$eff, na.rm = TRUE)
  if (is.finite(r_ref)) r_eff[, eff := eff - r_ref]

  dt <- merge(dt, r_eff[, .(round_class, r_adj = eff)], by = "round_class", all.x = TRUE)
  dt[, resid2 := resid - r_adj]

  t_eff <- dt[, .(eff = mean(resid2), n = .N), by = tier_class]
  # Same resolve-BEFORE-subtract order as the round block above, and for the
  # same reason. This block kept the old subtract-then-test order after the
  # round block was fixed: with no "top" row the subtraction turned the column
  # all-NA and the fallback's max() over all-NA was also NA, so a corpus with no
  # top-tier rows (a source mapping no tiers classifies everything "mid") got
  # every tier offset NA -- read downstream as a silent zero adjustment.
  t_ref <- t_eff[tier_class == "top", eff][1]
  if (!is.finite(t_ref)) t_ref <- max(t_eff$eff, na.rm = TRUE)
  if (is.finite(t_ref)) t_eff[, eff := eff - t_ref]

  # Per-family offsets alongside the pooled ones. A pooled offset is a weighted
  # average across events that behave completely differently: the low-tier
  # penalty is -0.45% for road and -3.59% for throws, and road's tier order is
  # INVERTED (a paced big-city marathon is faster than a tactical championship
  # one). Applying the pooled -1.69% to road inflates its low-tier marks by
  # ~1.2%, which is almost exactly the +0.95% forecast bias measured for road and
  # +1.10% for walk.
  #
  # Thin cells fall back to the pooled value rather than fitting noise.
  reg <- .citius_event_registry[, c("event_id", "family")]
  dt <- merge(dt, reg, by = "event_id", all.x = TRUE, sort = FALSE)
  rf <- tf <- NULL
  if (per_family && "family" %in% names(dt) && any(!is.na(dt$family))) {
    fd <- dt[!is.na(family)]
    rf <- fd[, .(eff = mean(resid), n = .N), by = .(family, round_class)]
    rf[, ref := eff[round_class == "final"][1], by = family]
    rf <- rf[!is.na(ref)]
    if (nrow(rf)) rf[, eff := eff - ref]
    rf <- rf[n >= min_cell, .(family, round_class, offset = eff, n)]

    fd <- merge(fd, r_eff[, .(round_class, r_adj2 = eff)], by = "round_class", all.x = TRUE)
    fd[is.na(r_adj2), r_adj2 := 0]
    fd[, resid3 := resid - r_adj2]
    tf <- fd[, .(eff = mean(resid3), n = .N), by = .(family, tier_class)]
    tf[, ref := eff[tier_class == "top"][1], by = family]
    tf <- tf[!is.na(ref)]
    if (nrow(tf)) tf[, eff := eff - ref]
    tf <- tf[n >= min_cell, .(family, tier_class, offset = eff, n)]

    # Shrink each family offset toward the pooled one. Applying these raw was
    # measurably worse than not having them at all (arm `cstack`, 2026-07-30):
    # road and walk improved by ~80% -- they are the families whose tier order is
    # genuinely inverted -- while throw and combined OVERCORRECTED, turning a
    # +0.60% bias into +2.05%. Those two carry the most extreme fitted low-tier
    # offsets on the thinnest cells, which is the signature of fitting noise.
    #
    # A family offset is a small-sample estimate of a quantity with a sensible
    # pooled prior, so it belongs under the same empirical-Bayes treatment
    # `estimate_ability()` already applies to athletes. `k` is FITTED by
    # out-of-sample validation, not chosen -- see .fit_context_shrink().
    if (shrink) {
      if (!is.null(rf) && nrow(rf)) {
        rf[, pooled := r_eff$eff[match(round_class, r_eff$round_class)]]
        rf[!is.finite(pooled), pooled := 0]
        k <- .fit_context_shrink(fd, "round_class", r_eff)
        rf[, raw := offset]
        rf[, `:=`(offset = pooled + (raw - pooled) * n / (n + k), shrink_k = k)]
        rf[, pooled := NULL]
      }
      if (!is.null(tf) && nrow(tf)) {
        tf[, pooled := t_eff$eff[match(tier_class, t_eff$tier_class)]]
        tf[!is.finite(pooled), pooled := 0]
        k <- .fit_context_shrink(fd, "tier_class", t_eff, resid_col = "resid3")
        tf[, raw := offset]
        tf[, `:=`(offset = pooled + (raw - pooled) * n / (n + k), shrink_k = k)]
        tf[, pooled := NULL]
      }
    }
  }

  # PER-EVENT offsets. A family is still a pool of events that behave
  # differently, and for round and tier the sprint family is the clearest case:
  # it holds the 100m and the 400m, which share almost nothing about how a heat
  # relates to a final. Measured on T1 finals with data richness held fixed, the
  # 100m beats the last-5 baseline by 10.8% while the 400m LOSES to it by 6.4%
  # and the 400m hurdles by 7.4% -- and per-family cannot see that split at all,
  # because both events sit in the same cell. Throws are the same story: discus
  # and javelin fly, shot and hammer do not.
  #
  # This is why per-family being refuted does not settle per-event. They fail
  # differently: the family offset is wrong for the 400m no matter how well it
  # is estimated, and no amount of shrinkage fixes a cell that is pooling two
  # unlike things.
  #
  # The prior is the family offset where one was fitted and the pooled offset
  # otherwise, so the chain is event -> family -> pooled and an event with thin
  # data keeps whatever coarser estimate it would have had. `k` is fitted by the
  # same out-of-sample validation used for families, at event grain.
  re <- te <- NULL
  if (per_event && "family" %in% names(dt) && any(!is.na(dt$family))) {
    ed <- dt[!is.na(family)]
    # THE REFERENCE CELL HAS TO CLEAR THE THRESHOLD TOO.
    #
    # An offset is a DIFFERENCE -- mean(resid | heat) minus mean(resid | final) --
    # so its precision depends on both cells, but the shrinkage weight n/(n + k)
    # sees only the first. At family grain that is harmless because the reference
    # cell is always enormous. At EVENT grain it is not: an event can carry
    # thousands of heat marks against a couple of hundred finals, and then a
    # noisy offset arrives with a large n attached and is shrunk almost not at
    # all -- the least reliable estimates getting the most weight, which is the
    # opposite of what shrinkage is for.
    #
    # Requiring both sides to clear `min_event_cell` removes the pathological
    # case without touching the shrinkage arithmetic, which matters because `k`
    # is fitted against the cell-count scale inside .fit_context_shrink(); moving
    # the caller to an effective n would apply a `k` fitted on a different scale.
    # Weighting by the harmonic effective n on BOTH sides is the fuller fix and
    # is left for when there is a measurement to justify it.
    re <- ed[, .(eff = mean(resid), n = .N), by = .(event_id, family, round_class)]
    re[, `:=`(ref = eff[round_class == "final"][1],
              n_ref = n[round_class == "final"][1]), by = event_id]
    re <- re[!is.na(ref)]
    if (nrow(re)) re[, eff := eff - ref]
    re <- re[n >= min_event_cell & n_ref >= min_event_cell,
             .(event_id, family, round_class, offset = eff, n, n_ref)]

    ed <- merge(ed, r_eff[, .(round_class, r_adj3 = eff)], by = "round_class", all.x = TRUE)
    ed[is.na(r_adj3), r_adj3 := 0]
    ed[, resid4 := resid - r_adj3]
    te <- ed[, .(eff = mean(resid4), n = .N), by = .(event_id, family, tier_class)]
    te[, `:=`(ref = eff[tier_class == "top"][1],
              n_ref = n[tier_class == "top"][1]), by = event_id]
    te <- te[!is.na(ref)]
    if (nrow(te)) te[, eff := eff - ref]
    te <- te[n >= min_event_cell & n_ref >= min_event_cell,
             .(event_id, family, tier_class, offset = eff, n, n_ref)]

    prior_for <- function(x, class_col, fam_tbl, pooled_eff) {
      p <- pooled_eff$eff[match(x[[class_col]], pooled_eff[[class_col]])]
      if (!is.null(fam_tbl) && nrow(fam_tbl)) {
        k <- match(paste(x$family, x[[class_col]]),
                   paste(fam_tbl$family, fam_tbl[[class_col]]))
        p[!is.na(k)] <- fam_tbl$offset[k[!is.na(k)]]
      }
      p[!is.finite(p)] <- 0
      p
    }
    if (shrink) {
      if (!is.null(re) && nrow(re)) {
        re[, pooled := prior_for(re, "round_class", rf, r_eff)]
        k <- .fit_context_shrink(ed, "round_class", r_eff, group_col = "event_id")
        re[, raw := offset]
        re[, `:=`(offset = pooled + (raw - pooled) * n / (n + k), shrink_k = k)]
        re[, pooled := NULL]
      }
      if (!is.null(te) && nrow(te)) {
        te[, pooled := prior_for(te, "tier_class", tf, t_eff)]
        k <- .fit_context_shrink(ed, "tier_class", t_eff, resid_col = "resid4",
                                 group_col = "event_id")
        te[, raw := offset]
        te[, `:=`(offset = pooled + (raw - pooled) * n / (n + k), shrink_k = k)]
        te[, pooled := NULL]
      }
    }
  }

  list(
    round = stats::setNames(r_eff$eff, r_eff$round_class),
    tier  = stats::setNames(t_eff$eff, t_eff$tier_class),
    round_family = rf,
    tier_family  = tf,
    round_event  = re,
    tier_event   = te,
    n     = nrow(dt)
  )
}

#' Fit the shrinkage weight for per-family context offsets
#'
#' Chooses `k` in the empirical-Bayes weight `n / (n + k)` by reproducing the job
#' the offsets actually do: predict an athlete's TOP-TIER, FINAL performance from
#' their performances in every other context.
#'
#' **Validating on corpus residuals instead gives the wrong answer, and this was
#' established the expensive way.** A first version split the corpus by date and
#' asked which `k` best reproduced each family-context cell in the held-out half.
#' It returned `k = 0` — no shrinkage, per-family offsets are fine — while the
#' `cstack` backtest showed those same offsets making forecasts *worse* (throw
#' bias +0.60% to +2.05%). Both results were correct about different questions.
#' The corpus is overwhelmingly low-tier, so a corpus-fit test asks "do these
#' offsets describe low-tier meets?" (yes, they are fitted on them) rather than
#' "do they carry a low-tier mark to a championship?" — which is the only use
#' they have.
#'
#' So the split here is by CONTEXT, not by date: hold out each athlete's top-tier
#' finals, predict them from the rest of that athlete's record, and score the
#' offsets on that. Athletes contribute only if they appear on both sides.
#'
#' Returns the pooled-only limit (`Inf`) when there is too little data to
#' validate, so the fallback is the behaviour that was already safe.
#'
#' @keywords internal
#' @noRd
.fit_context_shrink <- function(fd, class_col, pooled_eff, resid_col = "resid",
                                min_cell = 200L, group_col = "family") {
  need <- c("athlete_id", "event_id", "family", "perf", class_col, group_col)
  if (!all(need %in% names(fd))) return(Inf)
  ref <- if (class_col == "round_class") "final" else "top"
  d <- fd[!is.na(get(group_col)) & !is.na(perf) & !is.na(get(class_col))]
  if (nrow(d) < 10000L) return(Inf)

  d[, is_ref := get(class_col) == ref]
  d[, `:=`(n_ref = sum(is_ref), n_oth = sum(!is_ref)), by = .(athlete_id, event_id)]
  d <- d[n_ref > 0L & n_oth > 0L]
  if (nrow(d) < 5000L) return(Inf)

  # Per-group offset for each non-reference context, fitted on the SAME data the
  # caller fitted on, plus the value it will be shrunk toward. `group_col` is
  # "family" or "event_id"; the validation is identical either way, only the
  # grain of the cell changes.
  fam <- d[is_ref == FALSE, .(eff = mean(get(resid_col)), n = .N),
           by = c(group_col, class_col)][n >= min_cell]
  if (!nrow(fam)) return(Inf)
  fam[, pooled := pooled_eff$eff[match(get(class_col), pooled_eff[[class_col]])]]
  fam[!is.finite(pooled), pooled := 0]

  target <- d[is_ref == TRUE, .(tgt = mean(perf)), by = .(athlete_id, event_id)]
  oth <- merge(d[is_ref == FALSE], fam, by = c(group_col, class_col), all.x = FALSE)
  if (!nrow(oth)) return(Inf)

  # Collapse to one row per athlete-event-cell BEFORE sweeping k. The predictor
  # is mean(perf - adj) and adj is constant within a cell, so the whole sweep
  # reduces to sums of per-cell counts: sum(perf)/m - sum(cnt * adj)/m. Without
  # this the grid re-groups five million rows once per candidate.
  # `by` must be a literal c(), a key, or an eval()'d variable -- data.table
  # rejects a computed expression outright, and the error it raises recommends
  # exactly this form. unique() is needed because group_col is "event_id" in the
  # per-event path, which would otherwise repeat a column.
  #
  # The eval() here is data.table's column-selection idiom, not evaluation of
  # arbitrary code: by_cols is a character vector of column NAMES built from
  # in-package literals plus group_col/class_col, both of which are internal
  # function arguments. No caller-supplied data reaches it.
  by_cols <- unique(c("athlete_id", "event_id", group_col, class_col))
  cells <- oth[, .(cnt = .N, sp = sum(perf)), by = eval(by_cols)]
  cells <- merge(cells, fam, by = c(group_col, class_col), all.x = TRUE)
  tot <- cells[, .(m = sum(cnt), sp = sum(sp)), by = .(athlete_id, event_id)]
  tot <- merge(tot, target, by = c("athlete_id", "event_id"))
  if (!nrow(tot)) return(Inf)
  data.table::setkey(tot, athlete_id, event_id)

  grid <- c(0, 10^seq(2, 7, by = 0.25), Inf)
  sse <- vapply(grid, function(k) {
    w <- if (is.infinite(k)) 0 else cells$n / (cells$n + k)
    cells[, adjsum := cnt * (pooled + (eff - pooled) * w)]
    a <- cells[, .(sa = sum(adjsum)), by = .(athlete_id, event_id)]
    cmp <- a[tot, on = .(athlete_id, event_id)]
    mean(((cmp$sp - cmp$sa) / cmp$m - cmp$tgt)^2, na.rm = TRUE)
  }, numeric(1))
  if (all(!is.finite(sse))) return(Inf)
  grid[which.min(sse)]
}

#' @keywords internal
#' @noRd
.round_class <- function(round) {
  r <- toupper(trimws(as.character(round)))
  out <- rep("other", length(r))
  # These are sequential overwrites, so the LAST match wins and the patterns
  # must run least-specific to most-specific. Round labels nest: the feed's
  # actual semi-final label is "Semifinal - Heat", which contains HEAT, SEMI
  # and FINAL. With "final" applied last it classified as a FINAL -- all 14,764
  # semi-final results (4.79% of the harvest) were pooled into the reference
  # context that every other round's offset is measured against.
  out[grepl("^F", r) | grepl("FINAL", r)] <- "final"
  out[grepl("^H", r) | grepl("HEAT", r) | grepl("QUAL|Q[0-9]|CE", r)] <- "heat"
  out[grepl("^QF", r) | grepl("QUARTER", r)] <- "quarter"
  out[grepl("^SF", r) | grepl("SEMI", r)] <- "semi"
  out[is.na(r)] <- "other"
  out
}

#' Tier class for a results table, preferring the catalogue over the feed
#'
#' THE ONE PLACE TIER CLASS IS DERIVED. It previously happened in three
#' independent spots -- `.context_stats()` and `estimate_context_effects()` both
#' fitting offsets from the feed's `tier`, and `estimate_ability()` applying them
#' by `meet_tier` when available. Turning the `meet_tier` switch on therefore
#' looked up an offset fitted for "the feed says low" and applied it to
#' "the catalogue says T3", which are different populations.
#'
#' The direction of that error is the damaging one. The feed's "low" bucket is
#' contaminated with Diamond League, so the penalty fitted for it is far too
#' small; applied to correctly identified development meets it under-corrects
#' them. Half-fixed is worse than either end, and it is why the `meettier` arm
#' measured only -0.10% on marks -- it was measuring the mismatch, not the fix.
#'
#' Fit and application now call this same function, so they cannot diverge.
#'
#' @keywords internal
#' @noRd
.tier_class_of <- function(dt) {
  # KNOWN VOCABULARY SPLIT, deliberately left in place (2026-09-06). This
  # fallback is the legacy FOUR-class feed mapping (top/high/mid/low) while the
  # catalogue branch below is the THREE-class WAC one (top/mid/low), so a row
  # without `meet_tier` and a feed code of A or B still yields "high" -- a class
  # the WAC-fitted calibration does not contain, which then takes the median
  # precision in .context_precision().
  #
  # Collapsing the fallback onto the WAC table (A/B/C/D -> "mid", DF -> "top")
  # would remove the split and is probably right -- WAC calls the Diamond League
  # Final elite and it plainly is. But it MOVES ~7.5% of the corpus between
  # buckets and changes which offsets are fitted, which makes it a modelling
  # change, not a bugfix. It ships through a measured arm or not at all.
  #
  # What IS fixed: estimate_ability() now passes this resolved class to
  # result_weight(), so the 84.6% of rows the catalogue covers weight and offset
  # on the same label. Only the uncovered remainder can still reach "high".
  fb <- .tier_class(if ("tier" %in% names(dt)) dt$tier else NA_character_)
  if (!"meet_tier" %in% names(dt)) return(fb)
  mapped <- unname(c(T1_elite = "top", T2_strong = "mid",
                     T3_development = "low")[as.character(dt$meet_tier)])
  # An unclassified meet keeps the feed code rather than a guess.
  unname(data.table::fifelse(is.na(mapped), fb, mapped))
}

#' @keywords internal
#' @noRd
.tier_class <- function(tier) {
  t <- toupper(trimws(as.character(tier)))
  known <- c("OW", "GW", "GL", "A", "B", "C", "D", "DF", "E", "F")
  out <- rep("mid", length(t))
  out[t %in% c("OW", "GW", "GL")] <- "top"
  out[t %in% c("A", "B")] <- "high"
  out[t %in% c("C", "D", "DF")] <- "mid"
  out[t %in% c("E", "F")] <- "low"
  out[is.na(t)] <- "mid"
  # An unknown NON-missing code lands "mid" silently, which is how a new feed
  # code would misclassify without a trace -- "DF" itself appeared after this
  # mapping was first written. NA stays silent: absent is expected, unknown is
  # news.
  unknown <- !is.na(t) & nzchar(t) & !(t %in% known)
  if (any(unknown)) {
    cli::cli_warn(
      "Unknown tier code{?s} {.val {unique(t[unknown])}} classified as {.val mid}; extend .tier_class() if {?it is/they are} real.",
      .frequency = "once", .frequency_id = "citius_tier_unknown")
  }
  out
}


#' Put every performance on the footing the forecast targets
#'
#' The adjustment cascade extracted from [estimate_ability()], which had grown
#' it to ~350 lines inline: round/tier offsets (event -> family -> pooled
#' fallback chain), the fitted race effect, the coasting excess, wind,
#' momentum, indoor, seasonal phase and the championship offset -- in that
#' order, because the compositions are load-bearing: wind is suppressed on
#' rows where a race effect was applied, and coasting subtracts the EXCESS
#' over the round offset already removed. Each layer documents its own trap
#' inline below.
#'
#' Mutates `dt` BY REFERENCE -- every write is a data.table `:=`, and the
#' block was verified to contain no `dt <-` reassignment when extracted
#' (2026-08-13). Callers rely on that; the same table returns invisibly.
#' @keywords internal
#' @noRd
.adjust_history_to_target <- function(dt, calibration, adjust_race) {
  ctx <- if (!is.null(calibration) && !is.null(calibration$round)) {
    list(round = stats::setNames(calibration$round$offset, calibration$round$round_class),
         tier  = stats::setNames(calibration$tier$offset, calibration$tier$tier_class))
  } else estimate_context_effects(dt)
  rc <- .round_class(if ("round" %in% names(dt)) dt$round else NA_character_)
  # Tier class, from the MEET where one is supplied, otherwise from the feed's
  # per-result `tier` code.
  #
  # The feed code is not trustworthy: it varies WITHIN a single meet -- the
  # 2025 Weltklasse Zurich carries A, DF, F and GW across its own results,
  # classifying as high, mid, low and top at once -- and 189 of 1,341
  # competitions hold more than one. The direction of the damage is the worst
  # available: Diamond League marks, the strongest fields in the sport, are
  # routinely labelled "low" and then adjusted UPWARD by 1.69% as though set
  # at a slow meet. Those are precisely the athletes and races that make up
  # the T1 population the model is judged on.
  #
  # Pass `meet_tier` on the results (join it from
  # citiusdata/data/competition_catalogue.parquet) and it is used instead.
  # Same helper the calibration fits with, so the class an offset was
  # ESTIMATED for is always the class it is APPLIED to. This mapping used to be
  # written out here and nowhere else, which is exactly how the two halves came
  # apart.
  tc <- .tier_class_of(dt)
  r_adj <- ctx$round[rc]; r_adj[is.na(r_adj)] <- 0
  t_adj <- ctx$tier[tc];  t_adj[is.na(t_adj)] <- 0
  # Prefer the family's own offset where one was fitted; fall back to pooled.
  # The pooled value averages over events that behave oppositely -- road's
  # low-tier penalty is -0.45% against throws' -3.59% -- so applying it
  # uniformly mis-adjusts both ends.
  reg_c <- .citius_event_registry[, c("event_id", "family")]
  fam_c <- reg_c$family[match(dt$event_id, reg_c$event_id)]
  rfam <- if (!is.null(calibration$round_family)) calibration$round_family
          else ctx$round_family
  tfam <- if (!is.null(calibration$tier_family)) calibration$tier_family
          else ctx$tier_family
  if (!is.null(rfam) && nrow(rfam)) {
    k <- match(paste(fam_c, rc), paste(rfam$family, rfam$round_class))
    r_adj[!is.na(k)] <- rfam$offset[k[!is.na(k)]]
  }
  if (!is.null(tfam) && nrow(tfam)) {
    k <- match(paste(fam_c, tc), paste(tfam$family, tfam$tier_class))
    t_adj[!is.na(k)] <- tfam$offset[k[!is.na(k)]]
  }

  # The event's own offset wins over its family's, for the same reason the
  # family's wins over the pooled one: it is the least pooled estimate that
  # still has data behind it. Applied last so the fallback chain reads
  # event -> family -> pooled in the order the assignments happen.
  reve <- if (!is.null(calibration$round_event)) calibration$round_event
          else ctx$round_event
  teve <- if (!is.null(calibration$tier_event)) calibration$tier_event
          else ctx$tier_event
  if (!is.null(reve) && nrow(reve)) {
    k <- match(paste(dt$event_id, rc), paste(reve$event_id, reve$round_class))
    r_adj[!is.na(k)] <- reve$offset[k[!is.na(k)]]
  }
  if (!is.null(teve) && nrow(teve)) {
    k <- match(paste(dt$event_id, tc), paste(teve$event_id, teve$tier_class))
    t_adj[!is.na(k)] <- teve$offset[k[!is.na(k)]]
  }
  dt[, perf := perf - unname(r_adj) - unname(t_adj)]

  # THE RACE EFFECT. `decompose_races()` fits perf = athlete + race + resid,
  # and until 2026-08-13 this function read none of it: `calibration$race`
  # shipped in every deployed file, loaded on every run, and was never
  # consumed. A fast track, a fast night, pacing or altitude therefore flowed
  # straight into every athlete's estimate.
  #
  # This is the correction Pete described from the outside: if everyone in a
  # race ran 4s slow and an athlete ran 3s slow, they beat the race by 1s and
  # should be credited with it. `c_r` is the "everyone was 4s slow" term.
  #
  # ON THE DEPLOYED (CENTRED) FIT c_r HAS ZERO MEAN -- measured 3.33e-19, sd
  # 0.0271, 5-95% -0.043..0.039. The queue's warning that subtracting it shifts
  # ability levels globally was written for the UNCENTRED variant
  # (`centre = "auto"`), which was A/B'd and rejected on 2026-08-13. It does not
  # apply here, and marks are safe. Re-check that mean if the default ever
  # changes.
  #
  # WIND IS SUBSUMED, NOT ADDITIONAL. `calibrate()` removes shared wind INTO
  # the race effect (see the wind block below), so a race with a fitted `c_r`
  # has already had its wind taken out. Subtracting both double-counts it, and
  # the wind adjustment is therefore suppressed exactly on the rows where a
  # race effect applies. This is the same composition trap as the coasting
  # excess above: two corrections that each look right alone and overlap.
  # REFERENCED TO TOP-TIER FINALS, not subtracted raw. This is the same shape
  # as every other context correction here -- round offsets are referenced to
  # `final`, tier offsets to `top` -- and it is not optional.
  #
  # Subtracting raw `c_r` was tried on 2026-08-13 and failed its anchor
  # immediately: it predicted 2:01 for the world's best 800m women. `c_r` has
  # zero mean over the WHOLE corpus (3.33e-19), which is what made the raw form
  # look safe, but the corpus is mostly club racing. Over the races that matter
  # it is strongly positive -- Werro's finals average +2.88% -- because elite
  # finals ARE fast races. Subtracting that re-expresses every elite athlete in
  # average-club conditions and marks them down ~3%.
  #
  # A GLOBAL MEAN OF ZERO IS NOT ZERO WITHIN THE POPULATION YOU PREDICT FOR.
  # Referencing removes the between-race signal we want (this race was fast FOR
  # a championship final) while leaving the level where a forecast needs it.
  # DEFAULT OFF until it is backtested. The deployed calibration already
  # carries `race`, so wiring the reader alone would switch this on in every
  # forecast the moment it merged -- an unmeasured change shipped by accident,
  # which is most of what went wrong in this package's history. The anchor is
  # good (see below) and that is not the same as measured.
  has_cr <- rep(FALSE, nrow(dt))
  if (isTRUE(adjust_race) &&
      !is.null(calibration$race) && nrow(calibration$race) &&
      "race_key" %in% names(dt)) {
    rr <- data.table::as.data.table(calibration$race)
    if (!"ref_c_r" %in% names(rr)) {
      rr[, .rcl := .round_class(if ("round" %in% names(rr)) round else NA_character_)]
      rr[, .tcl := .tier_class(if ("tier" %in% names(rr)) tier else NA_character_)]
      # Per event, the mean race effect of a top-tier final. Fall back to the
      # event's own mean where an event has none (indoor-only events, thin
      # ones), and to zero only if even that is unavailable -- never silently
      # to the corpus mean, which is the failure being fixed.
      ref <- rr[.rcl == "final" & .tcl == "top",
                .(ref_c_r = mean(c_r, na.rm = TRUE)), by = event_id]
      fb <- rr[, .(fb_c_r = mean(c_r, na.rm = TRUE)), by = event_id]
      rr <- merge(rr, ref, by = "event_id", all.x = TRUE)
      rr <- merge(rr, fb, by = "event_id", all.x = TRUE)
      rr[!is.finite(ref_c_r), ref_c_r := fb_c_r]
      rr[!is.finite(ref_c_r), ref_c_r := 0]
    }
    i <- match(as.character(dt$race_key), as.character(rr$race_key))
    rs <- calibration$race_shock
    if (!is.null(rs) && !is.null(rs$expected) && is.finite(rs$beta)) {
      # EXCESS STRIP WITH FITTED PERSISTENCE (2026-09-06, Pete's design).
      #
      # The whole-effect strip above (c_r minus a top-final reference) put
      # history on "top final conditions" and needed an add-back for the
      # forecast race; the two halves never balanced and every final came
      # out ~2% pessimistic (docs/reviews/race-shock-arm-rejected-2026-09-06.md).
      #
      # This strips only the EXCESS: c_r minus what a race of that event x
      # tier class x round class normally shows (`rs$expected`, fitted by
      # fit_race_shock_persistence.R), and only the share of it that does NOT
      # predict the athlete's next result: (1 - beta) * excess, beta fitted by
      # regressing the next performance on the excess. A race that ran as its
      # kind usually does is untouched. Nothing is added back: the tier and
      # round context above already moves an athlete from the conditions
      # they raced in to the ones they are entering.
      if (!".rcl" %in% names(rr)) {
        rr[, .rcl := .round_class(if ("round" %in% names(rr)) round else NA_character_)]
        rr[, .tcl := .tier_class(if ("tier" %in% names(rr)) tier else NA_character_)]
      }
      ex <- data.table::as.data.table(rs$expected)
      e_cell <- ex$e_cell[match(paste(rr$event_id, rr$.tcl, rr$.rcl, sep = "|"),
                                paste(ex$event_id, ex$tier_class, ex$round_class, sep = "|"))]
      if (any(!is.finite(e_cell))) {
        evm <- rr[, .(m = mean(c_r, na.rm = TRUE)), by = event_id]
        fb <- evm$m[match(rr$event_id, evm$event_id)]
        e_cell[!is.finite(e_cell)] <- fb[!is.finite(e_cell)]
        e_cell[!is.finite(e_cell)] <- 0
      }
      # beta by the TIER CLASS of the shocked race -- the dominant structure
      # (first fit: top 0.53, high 0.72, mid 0.86, low 1.02): a big day at a
      # top meet is half the day, at a local meet it is the season. Falls back
      # to the overall beta. Clamped to [0, 1]: nothing is amplified.
      beta_r <- rep(rs$beta, nrow(rr))
      if (!is.null(rs$by_tier) && NROW(rs$by_tier)) {
        bt <- data.table::as.data.table(rs$by_tier)
        b_t <- bt$beta[match(rr$.tcl, bt$tier_class)]
        beta_r[is.finite(b_t)] <- b_t[is.finite(b_t)]
      }
      # Per-race beta, when the fitter stored one: persistence as a function of
      # what the race looked like (tier, share of the field that PB'd, wind,
      # size of the excess). A race where the whole field PB'd carries less
      # forward than its tier average says.
      if (!is.null(rs$by_race) && NROW(rs$by_race)) {
        br <- data.table::as.data.table(rs$by_race)
        b_r <- br$beta[match(as.character(rr$race_key), as.character(br$race_key))]
        beta_r[is.finite(b_r)] <- b_r[is.finite(b_r)]
      }
      beta_r <- pmin(pmax(beta_r, 0), 1)
      # FAMILY GATE. Two marks arms (2026-09-07, tier beta and per-race beta)
      # both improved sprint, hurdles, jump and throw and worsened middle,
      # distance and road: in the endurance events a shared effect is pacing
      # and course, which persist, and stripping them removes real form.
      # `rs$families` names where the strip applies; absent means everywhere.
      if (!is.null(rs$families) && length(rs$families)) {
        fam_rr <- .citius_event_registry$family[match(rr$event_id, .citius_event_registry$event_id)]
        beta_r[is.na(fam_rr) | !fam_rr %in% rs$families] <- 1
      }
      cr <- ((1 - beta_r) * (rr$c_r - e_cell))[i]
    } else {
      cr <- rr$c_r[i] - rr$ref_c_r[i]
    }

    # SHRINK BY FIELD SIZE. A race effect fitted on a two-athlete race is not
    # a race effect: with two runners, "the race was slow" and "both athletes
    # are slow" are the same observation, and the decomposition can only
    # separate them through athletes who also appear elsewhere.
    #
    # Found by anchor on 2026-08-13. An athlete with ONE corpus result --
    # 2:07.87 in a two-person race, c_r -0.068 against a +0.030 top-final
    # reference -- was handed a 9.8% uplift and ranked FIRST in the 800m W at
    # 1:55.29. `decompose_races()` already drops singletons; two is barely
    # better and was unguarded.
    #
    # The weight is n/(n+k) with k = sigma_within^2 / condition_sd^2, which is
    # the standard precision ratio for a group mean and is MEASURED per event
    # by calibrate() -- not a constant typed in here. A race effect is worth
    # believing in proportion to how many athletes it was fitted on relative
    # to how noisy the event is.
    n_r <- if ("n_in_race" %in% names(rr)) rr$n_in_race[i] else rep(NA_real_, nrow(dt))
    n_r[!is.finite(n_r)] <- 0
    # No events table, or an event with no measured spread, means k is unknown.
    # Unknown resolves to Inf, i.e. weight 0, i.e. NO race correction -- fail
    # closed rather than apply an unshrunk one. That is the same choice
    # `.calibrated_value()` makes with its documented fallback.
    ev <- calibration$events
    sw <- cs <- rep(NA_real_, nrow(dt))
    if (!is.null(ev) && NROW(ev)) {
      ev <- data.table::as.data.table(ev)
      j <- match(dt$event_id, ev$event_id)
      if ("sigma_within" %in% names(ev)) sw <- ev$sigma_within[j]
      if ("condition_sd" %in% names(ev)) cs <- ev$condition_sd[j]
    }
    k <- ifelse(is.finite(sw) & is.finite(cs) & cs > 0, (sw / cs)^2, Inf)
    wt <- n_r / (n_r + k)
    wt[!is.finite(wt)] <- 0
    cr <- cr * wt

    has_cr <- is.finite(cr) & wt > 0
    cr[!is.finite(cr)] <- 0
    dt[, perf := perf - cr]
  }

  # Coasting: the athlete-specific part of running a qualifying round easy.
  #
  # The line above has already removed the POPULATION round offset, which on
  # the current corpus is -0.59% for a heat. That is an average over everyone,
  # and it is nowhere near enough for an athlete who only needs to finish top
  # three to advance. Audrey Werro's heats run 5.3% slower than her finals --
  # nine times the population correction -- so 23 jogged heats outweighed 55
  # finals (heats carry 2.9x the precision of finals) and the fastest 800m
  # runner in the field was published ninth. See
  # ../../docs/incidents/werro-underrated-2026-08-13.md.
  #
  # SUBTRACT THE EXCESS, NOT THE TRAIT. `fit_coasting_trait()` measures each
  # athlete's mean heat deviation from their own athlete-event mean, so the
  # population heat effect is INSIDE the trait. Subtracting the raw trait here
  # would remove that component twice -- once via `r_adj` and once via the
  # trait -- and over-correct every coaster.
  #
  # KNOWN APPROXIMATION: the trait is referenced to the athlete's mean across
  # all rounds, while `r_adj` is referenced to finals. For an athlete who races
  # mostly finals the two references nearly coincide; for one with an unusual
  # round mix they do not. Left as an approximation rather than silently
  # refitting the trait to a final-referenced definition, because that would
  # change the fitted quantity under the same name. Measure before trusting.
  #
  # Gated on the calibration carrying the table, so this is inert until a
  # calibration is rebuilt with it -- and `test-calibration-wiring.R` now fails
  # if the deployed one lacks it, rather than letting it skip in silence.
  if (!is.null(calibration$coasting_trait) &&
      nrow(calibration$coasting_trait)) {
    ct <- data.table::as.data.table(calibration$coasting_trait)
    trait <- ct$coasting_trait[match(dt$athlete_id, ct$athlete_id)]
    trait[!is.finite(trait)] <- 0
    # The pooled heat offset, from the same calibration. Not a literal: the
    # value moves with every rebaseline, and a hardcoded -0.0059 would rot
    # silently the first time the corpus changed.
    pooled_heat <- 0
    rt <- calibration$round
    if (!is.null(rt) && "round_class" %in% names(rt) && "offset" %in% names(rt)) {
      ph <- rt$offset[match("heat", rt$round_class)]
      if (length(ph) && is.finite(ph)) pooled_heat <- ph
    }
    excess <- trait - pooled_heat
    # Only heats. A coaster's finals are raced, and the trait says nothing
    # about their semis.
    excess[.round_class(if ("round" %in% names(dt)) dt$round else NA_character_) != "heat"] <- 0
    dt[, perf := perf - excess]
  }

  # Wind, where the calibration carries a coefficient for the event. This is
  # the same adjustment layer as round and tier, and it belongs here rather
  # than in a pre-adjusted input file: `calibrate()` removes shared wind into
  # the race effect, but ability estimation never sees a race effect, so
  # between-race wind flows straight into the estimate.
  #
  # That channel is the large one. In the men's 100m the between-race wind
  # spread is 1.28 m/s against 0.33 within a race, so wind contaminates an
  # ability estimate by 48% of `sigma_within` while barely reordering any
  # single race. Measured on the backtest: gold skill +0.237 -> +0.240, with
  # the entire gain inside wind-legal events (t = +4.63 on 2,104 races) and
  # exactly none outside them (t = -0.41 on 4,515).
  # NOTE the variable names. `dt` already carries a column `w` — the recency
  # and precision weight built above — so a local `w` is SHADOWED inside
  # `dt[, ...]` and data.table silently uses the column instead. That subtracted
  # `beta * weight` from every mark rather than `beta * wind`: a constant shift
  # of 0.4% with the spread untouched, which no ranking test could ever catch.
  # See the NSE shadowing note in C:/dev/.claude/rules.
  if (!is.null(calibration$wind) && nrow(calibration$wind) &&
      "wind" %in% names(dt)) {
    wind_beta <- calibration$wind$beta[match(dt$event_id, calibration$wind$event_id)]
    wind_beta[!is.finite(wind_beta)] <- 0
    wind_val <- dt$wind
    wind_val[!is.finite(wind_val)] <- 0
    # Suppressed where a race effect was applied above: `calibrate()` folds
    # shared wind INTO `c_r`, so those rows have already had it removed and
    # subtracting again would double-count. Rows with no fitted race effect --
    # 27% of the corpus, and every row when no calibration carries `race` --
    # still get the wind correction, which is why this stays rather than being
    # deleted.
    wind_beta[has_cr] <- 0
    dt[, perf := perf - wind_beta * wind_val]
  }

  # Race momentum: an exponentially decayed count of recent race days. Same
  # adjustment layer as round, tier and wind, but note what it is NOT.
  #
  # Wind is a property of the RACE. Momentum is a property of the ATHLETE at a
  # moment, so stripping it here makes ability "momentum-neutral" -- what the
  # athlete is worth in an average state of readiness -- and the athlete's
  # momentum ON THE DAY has to be added back at prediction time. Strip only,
  # and every forecast is of an athlete in average form, which is wrong for
  # exactly the athletes who peak for a championship.
  #
  # See `apply_momentum()` for the other half.
  if (!is.null(calibration$momentum) && nrow(calibration$momentum) &&
      "momentum" %in% names(dt)) {
    reg_f <- .citius_event_registry[, c("event_id", "family")]
    fam <- reg_f$family[match(dt$event_id, reg_f$event_id)]
    mb <- calibration$momentum$beta[match(fam, calibration$momentum$family)]
    mb[!is.finite(mb)] <- 0
    mv <- dt$momentum
    mv[!is.finite(mv)] <- 0
    dt[, perf := perf - mb * mv]
  }

  # Indoor/outdoor. A race-level setting like round and tier, but the sign
  # differs by family -- sprint and middle are slower indoors, distance is
  # FASTER (no wind, better pacing) -- so a single global offset would cancel
  # them against each other.
  if (!is.null(calibration$indoor) && nrow(calibration$indoor) &&
      "indoor" %in% names(dt)) {
    reg_i <- .citius_event_registry[, c("event_id", "family")]
    fam_i <- reg_i$family[match(dt$event_id, reg_i$event_id)]
    io <- calibration$indoor$offset[match(fam_i, calibration$indoor$family)]
    io[!is.finite(io)] <- 0
    io[!(dt$indoor %in% TRUE)] <- 0
    dt[, perf := perf - io]
  }

  # Seasonal phase. Athletes are not equally sharp all year, and a championship
  # sits in a FIXED seasonal slot while an athlete's record spans the calendar.
  # Averaging unadjusted marks therefore drags every ability estimate below its
  # championship-day level, and drags it furthest for whoever happens to have an
  # early-season-heavy history. Same argument as the round and tier offsets.
  #
  # Offsets are centred within family-hemisphere by `fit_season_effect()`, so
  # this is a phase correction, not an intercept shift: it removes WHEN an
  # athlete raced, not how good they are. Stripped from history only, with no
  # add-back on the forecast — that is exactly the form validated out of sample
  # at -0.66% relative RMSE, and adding a target-month term back would be a
  # separate change needing its own validation.
  # `venue_country` is required, not optional. `calibrate()` only fits a season
  # effect when the fitting data carried it, so a non-NULL `calibration$season`
  # always holds a real split N/S calendar. Defaulting the SCORING data to "N"
  # when the column is absent would then look up every southern-hemisphere
  # athlete against the northern calendar -- six months out of phase, so the
  # offset lands with the WRONG SIGN rather than merely missing. Skipping the
  # correction entirely is the safe failure; applying it backwards is not.
  # The indoor block above re-checks its own column for the same reason.
  if (!is.null(calibration$season) && nrow(calibration$season) &&
      all(c("date", "venue_country") %in% names(dt))) {
    reg_s <- .citius_event_registry[, c("event_id", "family")]
    fam_s <- reg_s$family[match(dt$event_id, reg_s$event_id)]
    mon_s <- as.integer(format(as.Date(dt$date), "%m"))
    hemi_s <- data.table::fifelse(!is.na(dt$venue_country) &
                                    dt$venue_country %in% .citius_south, "S", "N")
    # Keyed join, not `match(paste(...), paste(...))`. Pasting three columns
    # builds one R string per row -- on a full corpus that is 6.6M strings and
    # hundreds of megabytes that R's own gc() does not account for, only the
    # OS does. That allocation pattern is what OOM-killed pipeline runs here
    # before; see the data.table RSS notes in C:/dev/.claude/rules.
    sk <- data.table::as.data.table(calibration$season)[
      , .(family, hemi, month, season_off = offset)]
    so <- sk[data.table::data.table(family = fam_s, hemi = hemi_s, month = mon_s),
             on = .(family, hemi, month), season_off]
    so[!is.finite(so)] <- 0
    dt[, perf := perf - so]
  }

  # Global championship vs another top-tier final. Round and tier offsets are
  # referenced to "final" and "top", so a top-tier final gets a zero adjustment
  # BY CONSTRUCTION and this distinction is currently inexpressible. It is not
  # zero, and the sign flips by family: endurance goes tactical and runs slower
  # (road -1.71%), power and technical events arrive tapered and run faster
  # (throw +1.40%). A pooled version is worse than none.
  #
  # Removing it here puts all history on a common NON-championship top-tier
  # final footing; `project_championship()` adds it back for the target. An
  # athlete whose record is all championships is unchanged by the round trip,
  # while one with only Diamond League form is correctly moved.
  if (!is.null(calibration$championship) && nrow(calibration$championship)) {
    reg_c <- .citius_event_registry[, c("event_id", "family")]
    fam_ch <- reg_c$family[match(dt$event_id, reg_c$event_id)]
    co <- calibration$championship$offset[
      match(fam_ch, calibration$championship$family)]
    co[!is.finite(co)] <- 0
    is_ch <- .is_championship(if ("tier" %in% names(dt)) dt$tier else NA_character_) &
      .round_class(if ("round" %in% names(dt)) dt$round else NA_character_) == "final"
    co[!is_ch] <- 0
    dt[, perf := perf - co]
  }
  invisible(dt)
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
#'   by [athletics_athlete_results()] or [aquatics_results()].
#' @param as_of Reference date for recency weighting. Defaults to today.
#' @param half_life Either a single number of days, or a fitted table from
#'   [fit_half_life()] giving a per-family half-life. Prefer the fitted table:
#'   how fast form decays is measurable, and the measured values (sprint ~135
#'   days, distance and field ~180) are far shorter than the 540-day scalar
#'   default, which keeps stale form alive.
#' @param races_half_life Either a single number, or a table with a
#'   `races_half_life` column plus `event_id` and/or `family`, in the same shape
#'   `half_life` accepts. An event the table does not name gets `Inf`, the term
#'   OFF, rather than a median of other events' values.
#'
#'   The number of the athlete's OWN subsequent races after
#'   which a result carries half weight, applied on top of `half_life`. A
#'   calendar half-life cannot tell apart an athlete who has raced 30 times
#'   since a performance from one who has raced twice; this can. `Inf`, the
#'   default, disables it and reproduces the previous behaviour exactly.
#'   Measured best around **5**, and it is not independent of `half_life`: with
#'   race-count decay on, the calendar half-life wants to be roughly twice as
#'   long, because 365 days had been standing in for a cap on how many results
#'   accumulate. Set both together or neither. See
#'   `docs/reviews/marks-blend-2026-09-07.md`.
#' @param precision_scale Exponent on the tier and round precision weights,
#'   scalar or a table with a `precision_scale` column plus `event_id` and/or
#'   `family`. `1` is the calibration as fitted; `0` weights every mark equally
#'   whatever meet it was set at, which measured better for marks on all nine
#'   families. Recency is untouched either way.
#' @param context_scale How much of the context adjustment to keep: `1` (the
#'   default) the whole correction, `0` none of it. Scalar, or a table with a
#'   `context_scale` column plus `event_id` and/or `family`. Only has an effect
#'   when `adjust_context = TRUE`. Fitted per family, distance wants 1.25 and
#'   road 1.00 while throw wants 0.25 and walk 0.00.
#' @param trim_tactical Fraction of worst performances to drop in tactical
#'   events. Set to `0` to disable. Scalar, or a table with a `trim_tactical`
#'   column plus `event_id` and/or `family` -- fitted that way it recovers what
#'   "tactical" means without registry input: middle, distance and combined
#'   0.40, jump and throw 0.00.
#' @param min_results Minimum results required to report an athlete.
#' @param only Optional vector of `athlete_id`s to return. This is the FAST
#'   PATH, not just a filter: the population quantities every athlete needs
#'   (`prior_mu`, `sigma_between`, and the robust-sigma scale `k`) are computed
#'   cheaply over the whole input, and the expensive per-athlete body then runs
#'   only for the ids named. The result for those athletes is identical to a
#'   full run, which a package test asserts rather than assumes. Use it whenever
#'   you know which athletes you are about to score -- a backtest or a
#'   diagnostic over a fixed set of races -- because without it the refit spends
#'   almost all of its time on athletes the caller will discard.
#' @param peak_gamma Exponent upweighting an athlete's own better marks over
#'   worse ones, ranked within (athlete, event). `0`, the default, weights
#'   every result equally on this axis. Scalar, or a table with a
#'   `peak_gamma` column plus `event_id` and/or `family`, same shape as
#'   `precision_scale`. Swept both sides of zero on the marks lab and `0` is a
#'   genuine interior optimum globally -- not an edge artefact -- but never
#'   tested per event. See `docs/plans/marks-parameter-optimisation-backlog-2026-09-08.md`.
#' @param adjust_context Whether to put every performance on a final-equivalent,
#'   top-tier footing before averaging, using [estimate_context_effects()].
#'   Without this the estimate answers "how does this athlete perform on an
#'   average day at an average meet", which is systematically slower than a
#'   championship final and will under-predict the event being simulated.
#' @return A `data.table` with `athlete_id`, `event_id`, `ability`,
#'   `ability_raw`, `sigma`, `sigma_raw`, `sigma_rob`, `sigma_marks`,
#'   `recent_mean`, `ability_se`, `n`, `n_eff`, `w_total`, `shrinkage`,
#'   `prior_mu`, `age_ref` and `last_date`. `age_ref` is the weighted mean age
#'   behind the estimate and is what [project_ability()] must project *from*.
#'
#'   Three of those are for MARKS rather than for the ranking, and
#'   [simulate_event()] reads them only for the mark distribution.
#'   `sigma_marks` is its spread; `recent_mean`, the plain mean of the athlete's
#'   last five raw marks, is blended into its centre by `CITIUS_MARKS_BLEND`,
#'   and is `NA` for an athlete with fewer than three. `ability` and `sigma`,
#'   which decide placings, are untouched by both.
#' @seealso [simulate_event()] which consumes this.
#' @export
estimate_ability <- function(results, as_of = Sys.Date(), half_life = 540,
                             races_half_life = Inf, context_scale = 1,
                             precision_scale = 1,
                             trim_tactical = 0.25, min_results = 1L,
                             adjust_context = TRUE, calibration = NULL,
                             robust_sigma = TRUE,
                             # DO NOT add "target"/"target_shrink" here. This is
                             # the DEFAULT, not the choice list -- match.arg()
                             # with several.ok = TRUE returns every element of
                             # it, so extending this line silently switches the
                             # extra parts on for every caller. The choices live
                             # at the match.arg() call below.
                             sigma_parts = c("estimator", "weight"),
                             sigma_mode = c("athlete", "event"),
                             only = NULL, peak_gamma = 0,
                             robust_location = FALSE,
                             decouple_peak = FALSE,
                             # Subtract the fitted race effect, referenced to
                             # top-tier finals and shrunk by field size. OFF
                             # until backtested: the deployed calibration
                             # carries `race`, so a default of TRUE would change
                             # every shipped forecast the moment this merged.
                             adjust_race = FALSE) {
  if (!nrow(results)) return(.empty_ability())

  # Surface a non-converged decomposition at the point it affects an answer,
  # not only in the build log nobody reads afterwards. Once per session.
  .warn_unconverged(calibration)

  dt <- .one_copy_dt(results)
  dt <- dt[!is.na(perf) & !is.na(event_id)]
  # Empty after filtering: return the empty schema DIRECTLY, never by recursing
  # with `results[0]`. The recursive form was written three times in this
  # function -- once named (which then silently dropped `adjust_race` when that
  # parameter was added) and twice positional, the exact shape a 2026-08-12
  # review found passing 13 arguments into 14 slots. A helper cannot drift when
  # the signature grows.
  if (!nrow(dt)) return(.empty_ability())

  dt[, athlete_id := as.character(athlete_id)]
  # Half-life may be a scalar or a fitted per-family table from fit_half_life().
  # "Did the caller pass anything?" is `missing()`, not "does the value happen
  # to equal the default?". The old test was `identical(half_life, 540)`, which
  # silently overrides a caller who passes 540 ON PURPOSE with the calibration's
  # value. No calibration in the repo carries `$half_life` yet, so this has
  # never fired -- but attaching a new field to an existing calibration object
  # is exactly how the season arms were built, and this file already documents
  # two promoted-config-not-reaching-every-consumer bugs.
  hl_spec <- if (!is.null(calibration) && !is.null(calibration$half_life) &&
                 missing(half_life)) calibration$half_life else half_life
  dt[, hl := .event_half_life(event_id, hl_spec)]
  # tier_class comes from .tier_class_of(), the SAME resolver the offsets use
  # (line 286), so the weighting and the offsets cannot drift onto different
  # tier vocabularies. Passing only `tier` here is what let the WAC promotion
  # reach the offsets and miss the weights.
  dt[, w := result_weight(date, tier = if ("tier" %in% names(dt)) tier else NA_character_,
                          round = if ("round" %in% names(dt)) round else NA_character_,
                          as_of = as_of, half_life = hl,
                          calibration = calibration,
                          tier_class = .tier_class_of(dt),
                          precision_scale = .event_param(event_id, precision_scale,
                                                         "precision_scale", 1))]

  # RACES-SINCE DECAY, on top of the calendar decay above.
  #
  # Named for what it is. `races_half_life = 5` means: a result carries half
  # weight once the athlete has run five more races in that event, a quarter
  # after ten, and so on -- exactly the shape `half_life` has, counted in the
  # athlete's own races instead of in days.
  #
  # `half_life` discounts a result by how long ago it happened. That is not the
  # only thing that makes a result stale: an athlete who has raced 30 times
  # since is further from that performance than one who has raced twice, and a
  # purely calendar decay treats them identically. This discounts a result by
  # how many of the athlete's own races have happened since -- k = 0 for their
  # most recent, 1 for the one before, and so on.
  #
  # WHY IT MATTERS, measured on 2024+ held out against a like-for-like last-5
  # baseline (diagnostics/marks_why_last5.R, 44 events):
  #
  #   calendar 365, no race decay   28 of 44   MAE 2.1487   the deployed config
  #   calendar 730, no race decay   17 of 44   MAE 2.2420   much worse alone
  #   calendar 365, race hl 5       36 of 44   MAE 2.0939
  #   calendar 730, race hl 5       37 of 44   MAE 2.0791
  #
  # The two are NOT separable, and that is the finding rather than a caveat:
  # a calendar half-life of 365 was doing two jobs, genuinely discounting stale
  # form AND crudely capping how many results pile up. Once race count handles
  # the second, the calendar decay relaxes to its real value and both improve.
  # Promote them together or not at all.
  #
  # This also replaces a cruder version of the same idea -- a hard cap on the N
  # most recent results, which peaked at 35 of 44. A cap is a cliff: result 20
  # counts fully and result 21 counts zero. The smooth form is better on every
  # measure, and adding a cap on top of it changes MAE by 0.01%, so the cap is
  # redundant rather than merely uglier.
  #
  # Inf is OFF and is the default, so existing callers are bit-identical.
  # Resolved to a column BEFORE the reorder, so the values travel with their
  # rows. Computing it into a bare vector and sorting afterwards is the silent
  # misalignment this file has been bitten by before.
  dt[, .rhl := .event_param(event_id, races_half_life, "races_half_life", Inf)]
  if (any(is.finite(dt$.rhl) & dt$.rhl > 0)) {
    data.table::setorder(dt, athlete_id, event_id, -date)
    dt[, .k := seq_len(.N) - 1L, by = .(athlete_id, event_id)]
    dt[is.finite(.rhl) & .rhl > 0, w := w * 0.5^(.k / .rhl)]
    dt[, .k := NULL]
  }
  dt[, .rhl := NULL]

  dt[, .pg := .event_param(event_id, peak_gamma, "peak_gamma", 0)]
  # != 0, NOT > 0. A negative peak_gamma is a real, fitted, intentional value
  # (event_params.rds carries distance at -0.5, e.g.) that upweights an
  # athlete's WORSE marks over their better ones -- `> 0` silently zeroed
  # every negative-gamma event's effect here while fit_event_params.R and
  # marks_hier_params.R's own replicas of this same block both correctly used
  # `!= 0`, so the fitted number and the applied number silently diverged.
  # Found by silent-failure-hunter review, 2026-09-08.
  if (any(dt$.pg != 0, na.rm = TRUE)) {
    dt[, .q := data.table::frank(perf, ties.method = "first") / .N, by = .(athlete_id, event_id)]
    dt[.pg != 0, w := w * (.q^.pg)]
    dt[, .q := NULL]
  }
  dt[, .pg := NULL]

  # `.fam` comes from the REGISTRY under a reserved name, not from whatever the
  # caller's results happen to carry. A bare `family` here would silently pick up
  # a caller column of the same name -- and if none existed, the gate below
  # would match nothing and quietly disable the override entirely.
  reg <- .citius_event_registry[, c("event_id", "tactical", "cv_prior", "family")]
  data.table::setnames(reg, "family", ".fam")
  dt <- merge(dt, reg, by = "event_id", all.x = TRUE, sort = FALSE)
  dt[is.na(tactical), tactical := FALSE]

  # Prefer a measured tactical signal over the registry's hand-set flag. Races
  # in tactical events skew slow, so a strongly negative skew in the fitted race
  # effects is direct evidence that times decouple from ability.
  if (!is.null(calibration) && !is.null(calibration$events)) {
    ti <- calibration$events[, c("event_id", "tactical_index", "calibrated")]
    dt <- merge(dt, ti, by = "event_id", all.x = TRUE, sort = FALSE)
    # GATED BY FAMILY, because `tactical_index` measures something broader than
    # tactics. It is `.skewness(c_r)`, the skew of an event's fitted race
    # effects, so it fires whenever some races come out much slower than typical.
    #
    # For a 1500m that IS tactics: championship finals are sit-and-kick and far
    # slower than paced races, a slow time there says nothing about ability, and
    # dropping the worst marks is right. For a shot put or a 100m the same skew
    # is WEATHER -- headwind, cold, a wet ring -- and the two want opposite
    # treatment. A tactically slow race should be dropped because it does not
    # measure the athlete; a weather-slowed race should be ADJUSTED, which
    # `.adjust_history_to_target()` already does. Trimming it as well deletes an
    # athlete's genuine bad days and biases the estimate upward.
    #
    # Ungated, the override flagged 52 of 74 events -- every throw and every
    # sprint. Measured on the marks lab with per-event parameters, held out on
    # 44 events: ungated 39 beaten and 23 separated wins, family-gated 41 and
    # 26. Registry-only reaches 42 beaten but only 25 separated, so the gate is
    # the better of the two narrowings and keeps the override's real work in the
    # families where it means something.
    stopifnot("registry family did not join" = ".fam" %in% names(dt))
    dt[calibrated %in% TRUE & is.finite(tactical_index) &
         .fam %in% .CITIUS_TACTICAL_FAMILIES,
       tactical := tactical_index < -0.5]
  }

  # `recent_mean` is built from RAW marks, so snapshot them before the
  # adjustment stack rewrites `perf` in place.
  #
  # UNCONDITIONAL, even though the blend defaults to off. The column is what the
  # marks diagnostics compare against, and gating it on the blend meant turning
  # the blend off silently stopped emitting the ingredient -- caught by test,
  # after `_deployed.R` had already been written claiming it was still emitted.
  # The cost is one numeric column on the history, which is nothing beside the
  # adjustment stack that runs on the next line.
  dt[, perf_raw := perf]

  if (isTRUE(adjust_context)) .adjust_history_to_target(dt, calibration, adjust_race)

  # CONTEXT SCALE: how much of the adjustment above to actually keep.
  #
  # `.adjust_history_to_target()` rewrites every mark to a final-equivalent,
  # neutral-conditions footing. That correction is itself estimated and carries
  # its own error, and how much of it is worth keeping differs sharply by event:
  # fitted per family on held-out marks, distance wants 1.25 and road 1.00 --
  # their times swing hugely with course and weather -- while throw wants 0.25
  # and walk 0.00.
  #
  # 1 keeps the full correction and is the default, so existing callers are
  # unchanged. 0 discards it, leaving the raw mark.
  if (isTRUE(adjust_context)) {
    dt[, .cs := .event_param(event_id, context_scale, "context_scale", 1)]
    if (!all(dt$.cs == 1)) dt[, perf := perf_raw + .cs * (perf - perf_raw)]
    dt[, .cs := NULL]
  }

  # Last five RAW marks per athlete-event, taken BEFORE the tactical trim.
  #
  # Before the trim on purpose: the blend's job is to pull the predicted mark
  # toward what the athlete has actually been producing, and the trim exists to
  # remove tactically slow races from the RANKING. Trimming here would reapply
  # a ranking correction to a quantity that is meant to be raw, and would make
  # the deployed term differ from the one measured in the marks lab, where the
  # baseline is a plain mean of the last five stored marks.
  #
  # Minimum three, matching the lab: a mean of one or two marks is noisier than
  # the ability it would be replacing, and those athletes keep `ability` alone.
  rr <- dt[, .(athlete_id, event_id, date, perf_raw)]
  if (!is.null(only)) rr <- rr[as.character(athlete_id) %in% as.character(only)]
  data.table::setorder(rr, athlete_id, event_id, -date)
  rr[, .rk := seq_len(.N), by = .(athlete_id, event_id)]
  .rec <- rr[.rk <= 5L, .(recent_mean = mean(perf_raw), n_recent = .N),
             by = .(athlete_id, event_id)][n_recent >= 3L]
  .rec[, athlete_id := as.character(athlete_id)]
  rm(rr)

  # `trim_tactical` accepts a per-event table too. Fitted per family, the values
  # recover what "tactical" is supposed to mean with no registry input: middle,
  # distance and combined want 0.40, while jump and throw want 0.00. A shot put
  # has no tactics to trim away.
  dt[, .trim := .event_param(event_id, trim_tactical, "trim_tactical", 0.25)]
  if (any(dt$.trim > 0)) {
    # Vectorised rank-and-filter, not `.SD[...]` per group. The `.SD` form made
    # data.table materialise a sub-table for every athlete-event group and cost
    # 74% of this function's runtime; the work itself is just "drop the worst
    # k marks", which needs no sub-table at all.
    dt[, .keep := TRUE]
    dt[tactical == TRUE & .trim > 0, .grp_n := .N, by = .(athlete_id, event_id)]
    dt[tactical == TRUE & .trim > 0 & .grp_n >= 4L,
       .rk := data.table::frank(perf, ties.method = "first"),
       by = .(athlete_id, event_id)]
    # frank is ascending and perf is oriented so higher is better: rank 1 is the
    # worst mark, which is what the tactical trim removes.
    dt[tactical == TRUE & .trim > 0 & .grp_n >= 4L,
       .keep := .rk > floor(.grp_n * .trim)]
    dt <- dt[.keep == TRUE]
    dt[, c(".keep", ".grp_n", ".rk") := NULL]
  }
  dt[, .trim := NULL]

  # ONLY: estimate abilities for a named set of athletes, without changing them.
  #
  # A backtest refits ability per meet and reads the ~300 entrants, but the
  # history covers every athlete who contested those events: 311,275
  # athlete-events for 312 entrants, so 998 estimates are computed per estimate
  # used, and that is 85% of a backtest's runtime.
  #
  # The non-entrants cannot simply be dropped, because the shrinkage prior
  # `prior_mu` is the event mean of `ability_raw` ACROSS ALL ATHLETES. Drop them
  # and the target every athlete is shrunk toward changes, which moves every
  # prediction. That is why `CITIUS_BT_ATHLETES` is documented as altering
  # prior_mu.
  #
  # But the prior needs only `ability_raw`, which is a plain weighted mean --
  # fully vectorisable in one grouped data.table op. The costly work is the rest
  # of the body: .weighted_sd() and .weighted_upper_sd(), which evaluate an R
  # closure per group and dominate the profile. So the priors are computed for
  # EVERYONE cheaply, and the expensive body runs only for `only`.
  #
  # The result is identical for the retained athletes. That is asserted by test,
  # not assumed.
  prior_all <- NULL
  if (!is.null(only)) {
    keep_ids <- as.character(only)
    # The prior must use the SAME location estimator as the full path, or the
    # population mean the retained athletes shrink toward is computed a
    # different way from their own point estimates. With robust_location = TRUE
    # the full path below uses an asymmetric Huber mean and this used a plain
    # weighted mean, so `only=` silently changed prior_mu -- breaking the
    # "identical for the retained athletes" guarantee this block claims. Live,
    # not latent: backtest_athletics.R passes `only=` and `robust_location=`
    # together, and run_robust_loc_screening.R sets the latter TRUE.
    if (isTRUE(robust_location) && !isTRUE(decouple_peak)) {
      pri <- dt[w > 0, {
        sig_ref <- data.table::first(cv_prior)
        if (!is.finite(sig_ref) || sig_ref <= 0) sig_ref <- .CITIUS_FALLBACK_CV
        .(n = .N,
          ability_raw = .asymmetric_huber_mean(perf, w, sig_target = sig_ref, k = 2.5))
      }, by = .(athlete_id, event_id)]
      cnts <- dt[, .(n_all = .N), by = .(athlete_id, event_id)]
      pri <- merge(pri, cnts, by = c("athlete_id", "event_id"), all.x = TRUE)
      pri[, n := n_all][, n_all := NULL]
    } else {
      sums <- dt[w > 0, .(.sw = sum(w), .swp = sum(w * perf)),
                 by = .(athlete_id, event_id)]
      cnts <- dt[, .(n = .N), by = .(athlete_id, event_id)]
      pri <- merge(cnts, sums, by = c("athlete_id", "event_id"), all.x = TRUE)
      pri[, ability_raw := .swp / .sw]
    }
    # Same filter the full path applies before computing the priors, so the
    # population behind prior_mu matches exactly.
    pri <- pri[n >= min_results & is.finite(ability_raw)]
    prior_all <- pri[, .(prior_mu = mean(ability_raw, na.rm = TRUE),
                         sigma_between = stats::sd(ability_raw, na.rm = TRUE)),
                     by = event_id]
    # `k`, the robust-sigma scale factor below, is a MEDIAN OVER THE POPULATION
    # of athletes with n >= 10. Computing it from the retained athletes alone
    # changes sigma, ability_se and shrinkage -- measured, not assumed: with
    # only the entrants kept, ability moved up to 7e-4 and shrinkage up to 1e-2
    # while ability_raw, n, w_total and prior_mu stayed bit-identical, which is
    # what isolated this line as the remaining dependency.
    #
    # Those athletes are only ~19% of athlete-events, so keeping them makes `k`
    # EXACT for about a fifth of the work. An approximation would have been
    # faster still, and today is a poor day to trade exactness for speed on a
    # quantity that feeds every ability estimate.
    n_by <- dt[, .(n = .N), by = .(athlete_id, event_id)]
    k_ids <- unique(as.character(n_by[n >= 10L]$athlete_id))
    dt <- dt[as.character(athlete_id) %in% union(keep_ids, k_ids)]
    if (!nrow(dt)) return(.empty_ability())
  }

  ab <- dt[, {
    ok <- w > 0
    mu <- if (sum(ok)) {
      if (isTRUE(robust_location) && !isTRUE(decouple_peak)) {
        sig_ref <- data.table::first(cv_prior)
        if (!is.finite(sig_ref) || sig_ref <= 0) sig_ref <- .CITIUS_FALLBACK_CV
        .asymmetric_huber_mean(perf[ok], w[ok], sig_target = sig_ref, k = 2.5)
      } else {
        stats::weighted.mean(perf[ok], w[ok])
      }
    } else NA_real_

    mu_peak <- if (sum(ok) && isTRUE(decouple_peak)) {
      sig_ref <- data.table::first(cv_prior)
      if (!is.finite(sig_ref) || sig_ref <= 0) sig_ref <- .CITIUS_FALLBACK_CV
      .asymmetric_huber_mean(perf[ok], w[ok], sig_target = sig_ref, k = 2.5)
    } else mu

    s  <- .weighted_sd(perf[ok], w[ok])
    s_rob <- .weighted_upper_sd(perf[ok], w[ok])
    a_ref <- if ("age" %in% names(.SD) && sum(ok & !is.na(age))) {
      stats::weighted.mean(age[ok & !is.na(age)], w[ok & !is.na(age)])
    } else NA_real_
    .(ability_raw      = mu,
      ability_raw_peak = mu_peak,
      sigma_raw        = s,
      sigma_rob        = s_rob,
      n                = .N,
      w_total          = sum(w),
      n_eff            = sum(w)^2 / sum(w^2),
      age_ref          = a_ref,
      cv_prior         = data.table::first(cv_prior),
      last_date        = max(date, na.rm = TRUE))
  }, by = .(athlete_id, event_id)]

  ab <- ab[n >= min_results & !is.na(ability_raw)]
  if (!nrow(ab)) return(.empty_ability())

  # Event-level priors drive the shrinkage strength. With `only` set these were
  # computed above from the WHOLE population, because taking them from the
  # retained athletes alone would shrink an elite field toward its own mean
  # instead of the event's.
  if (is.null(prior_all)) {
    ab[, `:=`(
      prior_mu = mean(ability_raw, na.rm = TRUE),
      sigma_between = stats::sd(ability_raw, na.rm = TRUE)
    ), by = event_id]
  } else {
    ab <- merge(ab, prior_all, by = "event_id", all.x = TRUE, sort = FALSE)
    # An event with no prior would silently fall back to whatever comes next;
    # better to use the retained athletes than NA, but say so.
    if (anyNA(ab$prior_mu)) {
      miss <- unique(ab[is.na(prior_mu)]$event_id)
      cli::cli_warn("No population prior for {length(miss)} event{?s}; using the retained athletes.")
      ab[is.na(prior_mu), prior_mu := mean(ability_raw, na.rm = TRUE), by = event_id]
      ab[is.na(sigma_between), sigma_between := stats::sd(ability_raw, na.rm = TRUE), by = event_id]
    }
  }

  # THREE separate faults were found in this block on 2026-07-31, all of which
  # inflate the spread of thinly-raced athletes and hand them win probability
  # they have not earned.
  #
  # THE SEVERITY HERE WAS OVERSTATED -- corrected 2026-08-12. This block used to
  # claim thin athletes (`w_total` < 1) were credited 0.0509 gold and won 0.0412,
  # a ratio of 0.81. Re-measured over the full 380-meet cache (42,765 scored
  # athlete-races, races whose winner is in the field), the credited figure
  # reproduces at 0.0503 but the realised one does not: 0.0477, a ratio of
  # **0.948**. Deep athletes (`w_total` > 10) come in at 1.040, matching the
  # logged 1.03. Same direction, about a third of the severity.
  #
  # Sizing that matters for anyone tempted to build on this: perfectly
  # recalibrating every evidence-depth bucket to its own realised ratio -- an
  # UPPER BOUND on any fix of this class -- is worth -0.15% gold Brier and
  # -0.18% medal. Treat these as defect fixes judged on do-no-harm, never as
  # improvements. The real driver of thin-athlete mis-rating is the unremoved
  # race effect; see docs/reference/modelling-traps.md.
  #
  # 1. The sample spread is not robust. One impossible mark in a three-mark
  #    history produced `sigma` 6.6x the event value. `.weighted_upper_sd()`
  #    estimates the same quantity from the upper half, where contamination
  #    cannot reach.
  # 2. The shrinkage TARGET was `cv_prior`, which the registry documents as a
  #    "fallback placeholder, not an estimate" -- 0.008 for the 100m against a
  #    MEASURED `sigma_within` of 0.0172. Every athlete in the package was
  #    being pulled toward a number less than half the truth.
  # 3. The shrinkage WEIGHT was `n_eff`, which measures only how evenly weight
  #    is spread. Ability shrinkage uses `w_total` for reasons argued 30 lines
  #    above; dispersion shrinkage must use it for the same reasons.
  # The three fixes are independently switchable because they OPPOSE each other
  # over thin athletes: `target` widens them (the old `cv_prior` was half the
  # measured value) while `estimator` narrows contaminated ones. The bundle
  # measured -0.56% on gold Brier; splitting it may raise that rather than
  # merely explain it. Attribution needs one switch per fix.
  # DEFAULT IS THE VALIDATED PAIR, NOT ALL THREE.
  #
  # `crob` measured -0.56% on gold Brier (p = 0.0072) and was adopted on that
  # basis -- but it ran while a recycling bug held the `target` fix inert, so
  # what it validated was estimator + weight. Shrinking toward `cv_prior`
  # instead of the measured `sigma_within` is still wrong on its face, and the
  # fix is available here, but wrong-on-its-face and better-in-the-backtest are
  # separate claims: that is precisely what `csens` demonstrated the same day.
  # `target` ships only once an arm has measured it.
  sigma_mode <- match.arg(sigma_mode)
  parts <- if (isTRUE(robust_sigma)) {
    match.arg(sigma_parts, c("estimator", "weight", "target", "target_shrink"),
              several.ok = TRUE)
  } else character()
  use_estimator     <- "estimator" %in% parts
  use_target        <- "target" %in% parts
  use_weight        <- "weight" %in% parts
  use_target_shrink <- "target_shrink" %in% parts
  # Both on is not a stronger version of either -- `target` already routes the
  # measured value into the emitted sigma, so `target_shrink` would be a silent
  # no-op and the arm unattributable. Refuse rather than pick one.
  if (use_target && use_target_shrink) {
    cli::cli_abort(c(
      "{.arg sigma_parts} cannot contain both {.val target} and {.val target_shrink}.",
      i = "{.val target} routes the measured spread into the emitted sigma; {.val target_shrink} routes it into the shrinkage path only. Together the second does nothing."
    ))
  }

  if (use_estimator) {
    # Put the good-side estimate back on the pooled-spread scale, calibrated
    # from THIS population rather than a constant. Well-observed athletes are
    # the reference because their `sigma_raw` is trustworthy: whatever ratio
    # holds for them is the ratio the estimator needs everywhere.
    #
    # Doing it this way is what keeps the experiment clean. The good-side
    # spread is 0.72-0.86 of the pooled spread depending on event, so using it
    # raw would shrink every athlete's sigma by a systematic ~20% -- a scale
    # change riding along with a robustness change, and no way to tell which
    # one moved the result.
    ref <- ab[n >= 10L & is.finite(sigma_rob) & sigma_rob > 0 &
                is.finite(sigma_raw) & sigma_raw > 0]
    k <- if (nrow(ref) >= 20L) stats::median(ref$sigma_raw / ref$sigma_rob) else 1
    if (!is.finite(k) || k <= 0) k <- 1
    ab[, sigma := data.table::fifelse(is.finite(sigma_rob) & sigma_rob > 0,
                                      sigma_rob * k, NA_real_)]
    # No usable good side at all: fall back to the event value, NOT to
    # `sigma_raw`. Falling back to the raw spread restores exactly the
    # contaminated number this estimator exists to avoid -- which is the bug
    # the first version of this shipped with.
  } else {
    ab[, sigma := sigma_raw]
  }

  # The MEASURED within-athlete spread for the event, or NA where the
  # calibration has none. Extracted because two callers need it below and the
  # vectorised-comparison trap in it must not be written twice.
  .measured_sigma_within <- function() {
    if (is.null(calibration) || is.null(calibration$events)) return(NULL)
    tgt <- data.table::as.data.table(calibration$events)
    if (!all(c("event_id", "sigma_within") %in% names(tgt))) return(NULL)
    k <- match(ab$event_id, tgt$event_id)
    sw <- tgt$sigma_within[k]
    # NOT isTRUE(): on a vector it returns a single FALSE, so `!isTRUE(...)`
    # is a length-one TRUE that recycles and blanks the WHOLE column. That is
    # the same defect found in this file at line 507 on 2026-07-31, written
    # again here hours later. Vectorised comparison only.
    if ("calibrated" %in% names(tgt)) {
      ok_cal <- tgt$calibrated[k]
      sw[is.na(ok_cal) | !ok_cal] <- NA_real_
    }
    sw
  }

  # Shrink toward the MEASURED within-athlete spread for the event, falling back
  # to the registry placeholder only where no calibration exists.
  ab[, sigma_target := cv_prior]
  if (use_target) {
    sw <- .measured_sigma_within()
    if (!is.null(sw)) ab[is.finite(sw) & sw > 0, sigma_target := sw[is.finite(sw) & sw > 0]]
  }

  # DECOUPLING (`sigma_parts = "target_shrink"`, default OFF).
  #
  # `sigma` does two unrelated jobs: it sets the shrinkage strength through
  # `kappa = sigma^2 / sigma_between^2`, and it is the dispersion handed to
  # `simulate_event()`. The `target` arm raised both at once and was REFUTED on
  # 2026-08-12 -- gold Brier +0.42% (t = +3.77, p = 0.000172) over 1,316 paired
  # races -- because widening a thin athlete's DRAW hands back more win
  # probability than the extra shrinkage removes. Win probability rewards being
  # unpredictable (OPTIMISATION-FRAMEWORK.md item 3).
  #
  # This applies the measured target to the shrinkage path ONLY, leaving the
  # simulator's sigma on the placeholder. It isolates the half that helped.
  # `target` and `target_shrink` are mutually exclusive: with both on, the
  # second is a no-op and the arm would be unattributable.
  sigma_shr_target <- NULL
  if (use_target_shrink && !use_target) {
    sw <- .measured_sigma_within()
    if (!is.null(sw)) {
      sigma_shr_target <- ab$sigma_target
      ok <- is.finite(sw) & sw > 0
      sigma_shr_target[ok] <- sw[ok]
    }
  }
  ab[, sigma := data.table::fifelse(is.na(sigma) | sigma <= 0, sigma_target, sigma)]

  # A two-race athlete's sample spread is close to meaningless on its own, so
  # blend toward the event value by absolute evidence.
  shrink_w <- if (use_weight) ab$w_total else ab$n_eff
  k_pn <- .sigma_pseudo_n()
  # Mirror the blend with the measured target BEFORE `sigma` is overwritten, so
  # the two paths differ in exactly one input and nothing else.
  if (!is.null(sigma_shr_target)) {
    ab[, sigma_shr := (shrink_w * sigma + k_pn * sigma_shr_target) /
                      (shrink_w + k_pn)]
  }
  ab[, sigma := (shrink_w * sigma + k_pn * sigma_target) /
                (shrink_w + k_pn)]

  # Rescale to the context being FORECAST. sigma is fitted across the pooled
  # history, but the target is a top-tier final, and that is a narrower slice of
  # conditions than the corpus average for field events and a wider one for road.
  # Measured ratios of championship spread to pooled spread predict the model's
  # dispersion error almost exactly (cor 0.80 across families; throw 0.681 vs a
  # measured sd(z) of 0.698, road 1.141 vs 1.142).
  #
  # Applied HERE, to `ab$sigma`, because that is the column `simulate_event()`
  # reads. A previous attempt to widen `calibration$events$sigma_within` was
  # bit-for-bit inert for exactly that reason.
  if (!is.null(calibration$sigma_context)) {
    sc <- data.table::as.data.table(calibration$sigma_context)
    reg <- .citius_event_registry[, c("event_id", "family")]
    fam <- reg$family[match(ab$event_id, reg$event_id)]
    ratio <- sc$ratio[match(fam, sc$family)]
    ratio[!is.finite(ratio) | ratio <= 0] <- 1
    ab[, sigma := sigma * ratio]
    if ("sigma_shr" %in% names(ab)) ab[, sigma_shr := sigma_shr * ratio]
    ctx_ratio <- ratio
  } else {
    ctx_ratio <- rep(1, nrow(ab))
  }
  sc_env <- .sigma_scale_env()
  if (sc_env != 1) {
    ab[, sigma := sigma * sc_env]
    if ("sigma_shr" %in% names(ab)) ab[, sigma_shr := sigma_shr * sc_env]
  }

  # sigma_marks (2026-09-06): the spread used for the MARK DISTRIBUTION only.
  #
  # The emitted `sigma` above drives placings, and three attempts to replace it
  # (event constant twice, two-sided + pseudo-n 40) all lost medal logloss --
  # its one-sided upper-tail estimator carries an upside signal that wins
  # races. But that same sigma ranks athletes by hold-out consistency at
  # Spearman 0.07 and gave Noah Lyles a 5% chance of beating the world record.
  # The two jobs need two numbers. This one is the two-sided sigma_raw, shrunk
  # hard toward the event target (pseudo-n 40, the setting that tied the event
  # constant on hold-out log score), scaled by the same per-family context
  # ratio. simulate_event() uses it for `perf_std` -- the mark distribution
  # and median_mark -- and leaves `perf`, the ranking, on `sigma`.
  k_m <- .sigma_marks_pseudo_n()
  sm <- data.table::fifelse(is.finite(ab$sigma_raw) & ab$sigma_raw > 0, ab$sigma_raw, ab$sigma_target)
  sm <- (shrink_w * sm + k_m * ab$sigma_target) / (shrink_w + k_m)
  ab[, sigma_marks := sm * ctx_ratio * sc_env]

  # `sigma_mode = "event"` gives every athlete their event's measured spread.
  #
  # Not a modelling preference -- a test. Per-athlete sigma REORDERS the field
  # at the simulation stage: in the men's 100m, rank correlation with recent
  # form falls from 0.736 at the ability stage to 0.573 at p_gold, because the
  # win probability rewards being unpredictable. Lyles is rated 2nd on ability
  # and 4th on p_gold; Seville 3rd and 13th. Flattening sigma asks whether that
  # reordering carries information or destroys it.
  if (identical(sigma_mode, "event")) {
    ab[, sigma := sigma_target]
    if ("sigma_shr" %in% names(ab)) ab[, sigma_shr := sigma_shr_target]
  }

  ab[, sigma_between := data.table::fifelse(
    is.na(sigma_between) | sigma_between <= 0, sigma, sigma_between
  )]
  # `kappa` reads the SHRINKAGE sigma, which is the emitted one unless
  # `target_shrink` asked for them to differ. Everything downstream of `kappa`
  # -- shrinkage, ability, ability_se -- follows it; `sigma` itself is left
  # alone because that is what `simulate_event()` draws with.
  sig_k <- if ("sigma_shr" %in% names(ab)) ab$sigma_shr else ab$sigma
  ab[, kappa := (sig_k^2) / (sigma_between^2)]
  # Shrink on total weight, not n_eff: a decade-old record carries almost no
  # weight and should regress to the event mean regardless of how many results
  # it contains. This is what makes stale athletes fall out of contention on
  # their own, rather than needing a hand-set staleness cutoff.
  ab[, shrinkage := kappa / (w_total + kappa)]
  ab[, ability := (1 - shrinkage) * ability_raw + shrinkage * prior_mu]

  # recent_mean: the ingredient of the MARK centre, as distinct from `ability`,
  # the centre of the ranking. Same split as `sigma_marks` above and for the
  # same reason -- the two jobs want different numbers.
  #
  # `ability` is a decayed, context-adjusted, trimmed, shrunk estimate built to
  # order a field. As a point forecast of the next mark it is systematically
  # optimistic: measured on 2024+ held out, +0.24pp of a mark more optimistic
  # than a plain last-5 mean, which is enough to lose whole events on mark
  # error. Blending toward recent raw form removes most of that.
  #
  # THE BLEND IS NOT APPLIED HERE, and that is deliberate. Callers modify
  # `ability` after this function returns -- backtest_athletics.R applies
  # apply_momentum() and project_ability() to it, and reshrink_to_field() shifts
  # it too. A pre-blended column would silently stop tracking those, so an aged
  # athlete's ranking would move while their predicted mark stayed put. Emitting
  # the raw ingredient lets simulate_event() blend against whatever `ability`
  # finally is.
  #
  # Athletes with fewer than three recent marks get NA and keep `ability`.
  ab[, recent_mean := NA_real_]
  if (!is.null(.rec) && nrow(.rec)) {
    ab[, athlete_id := as.character(athlete_id)]
    ab[, recent_mean := NULL]
    ab <- merge(ab, .rec[, .(athlete_id, event_id, recent_mean)],
                by = c("athlete_id", "event_id"), all.x = TRUE, sort = FALSE)
  }
  # `ability_raw_peak` comes out of the grouped aggregation on EVERY call -- a
  # `by` expression has to return the same columns for every group, so it could
  # not be omitted conditionally there. Drop it here when decoupling was not
  # asked for. Left in place, the presence test below is always true, so
  # `ability_peak` is emitted for every caller and `simulate_event()` takes its
  # dual-path branch -- a second full n_sims x n_ath matrix -- on every
  # simulation in the package. That is inert today only because `mu_peak` falls
  # back to `mu`, making the column bit-identical to `ability`. The gate would
  # otherwise stop meaning "decoupling was requested" and start meaning
  # "estimate_ability ran", which is not something a later edit to `mu_peak`
  # would fail loudly on.
  if (!isTRUE(decouple_peak) && "ability_raw_peak" %in% names(ab)) {
    ab[, ability_raw_peak := NULL]
  }
  if (isTRUE(decouple_peak) && "ability_raw_peak" %in% names(ab)) {
    ab[, ability_peak := (1 - shrinkage) * ability_raw_peak + shrinkage * prior_mu]
  }

  # Uncertainty in the ABILITY estimate, so it follows the shrinkage sigma for
  # the same reason `kappa` does. Identical to `sigma` unless `target_shrink`.
  ab[, ability_se := sig_k / sqrt(w_total + kappa)]

  if (!is.null(only)) ab <- ab[as.character(athlete_id) %in% as.character(only)]

  # `sigma_raw` and `sigma_rob` are returned alongside the emitted `sigma`
  # because without them the spread pipeline cannot be audited from outside.
  #
  # 2026-09-05: per-athlete sigma predicts an athlete's FUTURE scatter at
  # pearson 0.057 where a plain SD of their own past marks manages 0.097, and
  # it runs at about half the true level. Six hypotheses were tested from
  # outside the function and eliminated -- the robust estimator, the decay
  # window, `k` varying with sample size, the constant blend, precision
  # weighting, and the context-adjustment chain. Every input reconstructable
  # externally lands at ~0.0148 against an internal 0.0090, so the remaining
  # gap is between these two quantities and the emitted one, and NONE of it was
  # observable because neither was returned. That is six diagnostics' worth of
  # work a two-column addition would have saved.
  #
  # Additive only: existing callers select by name and are unaffected.
  cols <- c("athlete_id", "event_id", "ability", "ability_raw", "sigma",
            "sigma_raw", "sigma_rob", "sigma_marks", "recent_mean",
            "ability_se", "n", "n_eff", "w_total", "shrinkage", "prior_mu",
            "age_ref", "last_date")
  if ("ability_peak" %in% names(ab)) cols <- c(cols, "ability_peak")
  cols <- intersect(cols, names(ab))
  ab[, cols, with = FALSE][]
}


#' Re-shrink ability toward the field rather than the whole event
#'
#' Empirical Bayes shrinks a thinly-evidenced athlete toward `prior_mu`, the
#' UNCONDITIONAL mean ability in the event — computed across everyone rated,
#' including a long tail of club athletes who will never contest a final. The
#' athletes actually entered in a championship are a selected subset well above
#' that mean, so shrinking them toward it drags them below their true level, and
#' the more they are shrunk the worse it gets.
#'
#' Measured on the athletics backtest: a finalist sits a median **+1.36%** above
#' the unconditional event mean (800m W +3.32%, Long Jump M +2.91%), and the
#' predicted-mark bias runs from −0.07% for barely-shrunk athletes to **−2.18%**
#' for those shrunk more than 60%.
#'
#' The prior enters the shrinkage linearly, so re-conditioning is exact and needs
#' no refit:
#' \deqn{ability_{new} = ability_{old} + shrinkage 	imes (\mu_{new} - \mu_{old})}
#'
#' The prior is built from `ability_raw`, never from the shrunk `ability`, which
#' would be circular — shrinking toward a mean that is itself the result of
#' shrinking compounds toward the centre with every pass.
#'
#' @param ability An ability table from [estimate_ability()], carrying
#'   `ability_raw`, `shrinkage` and `prior_mu`.
#' @param field Optional character vector of `athlete_id`s defining the
#'   population to shrink toward. Defaults to every athlete in `ability`, which
#'   is a no-op — pass the entrants of the race being predicted.
#' @param weight How far to move from the unconditional prior to the field
#'   prior, in `[0, 1]`. `1` shrinks fully toward the field.
#' @return `ability` with `ability`, `prior_mu` and `ability_se` updated.
#' @seealso [estimate_ability()]
#' @export
condition_prior <- function(ability, field = NULL, weight = 1) {
  ab <- data.table::copy(data.table::as.data.table(ability))
  if (!all(c("ability_raw", "shrinkage", "prior_mu") %in% names(ab))) {
    cli::cli_abort("{.arg ability} must come from {.fn estimate_ability} and carry {.field ability_raw}, {.field shrinkage} and {.field prior_mu}.")
  }
  if (!nrow(ab)) return(ab[])
  # A true no-op. Treating field = NULL as "every athlete in `ability`" is only
  # a no-op when `ability` covers the population prior_mu was computed over --
  # and estimate_ability(only = entrants) deliberately breaks that, returning
  # entrants only while keeping the POPULATION prior_mu. The default then
  # silently conditioned on the entrants, applying exactly the finalist-selection
  # shift a caller passing no field is asking not to apply.
  if (is.null(field)) return(ab[])
  sel <- as.character(ab$athlete_id) %in% as.character(field)
  if (!any(sel)) {
    cli::cli_warn("No athlete in {.arg field} matched; prior left unconditioned.")
    return(ab[])
  }
  ab[, .fp := mean(ability_raw[sel[.I]], na.rm = TRUE), by = event_id]
  # A field of one carries no information about the population it is drawn from.
  ab[, .nf := sum(sel[.I]), by = event_id]
  ab[.nf < 2L | !is.finite(.fp), .fp := prior_mu]
  ab[, .new_mu := prior_mu + weight * (.fp - prior_mu)]
  ab[, ability := ability + shrinkage * (.new_mu - prior_mu)]
  if ("ability_peak" %in% names(ab)) ab[, ability_peak := ability_peak + shrinkage * (.new_mu - prior_mu)]
  ab[, prior_mu := .new_mu]
  ab[, c(".fp", ".nf", ".new_mu") := NULL]
  ab[]
}


#' The empty ability table, in the SAME schema the non-empty path emits
#'
#' One definition, used by every early return in [estimate_ability()]. Before
#' this existed the empty case was produced by recursing with `results[0]` from
#' three call sites -- two positional (the argument-shift trap a review had
#' already caught once in this function) and one named that silently dropped
#' `adjust_race` when the signature grew. The old top-guard table was also
#' missing `ability_se`, `w_total`, `prior_mu` and `age_ref`, so the empty case
#' had a different schema from every non-empty result.
#' @keywords internal
#' @noRd
.empty_ability <- function() {
  data.table::data.table(
    athlete_id = character(), event_id = character(), ability = numeric(),
    ability_raw = numeric(), sigma = numeric(),
    # Kept in step with the populated return above. An empty table whose
    # columns differ from a populated one is how a caller that binds the two
    # ends up with silent NAs.
    sigma_raw = numeric(), sigma_rob = numeric(),
    sigma_marks = numeric(), recent_mean = numeric(), ability_se = numeric(),
    n = integer(), n_eff = numeric(), w_total = numeric(),
    shrinkage = numeric(), prior_mu = numeric(), age_ref = numeric(),
    last_date = as.Date(character())
  )
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

#' Weighted quantile by the inverse of the weighted ECDF
#' @keywords internal
#' @noRd
.weighted_quantile <- function(x, w, p) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  x <- x[ok]; w <- w[ok]
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= p)[1L]]
}

#' Robust one-sided scale: the spread of the GOOD half only
#'
#' A race is won by the best draw, so what matters is how far above their own
#' level an athlete can reach. Estimating that from the upper half makes the
#' estimate immune to the lower tail by construction, however contaminated it
#' is -- and the lower tail is where the contamination lives, because a jogged
#' race, an injury, a foul-out or three failures at the opening height all
#' produce a mark far below an athlete's level and none of them are draws from
#' their performance distribution.
#'
#' Measured 2026-07-31: one impossible 17.33 s in a three-mark 100 m history
#' gave an athlete `sigma` = 0.1144 against an event value of 0.0172 -- 6.6x too
#' wide. He was predicted at 11.66 s and still out-ranked an athlete predicted
#' at 10.17 s, because the simulator converts spread into win probability. On
#' the same history this estimator returns ~0.015, the corrupt mark having no
#' influence at all.
#'
#' For a symmetric distribution `E[X^2 | X > 0] = Var(X)`, so the root mean
#' square of the positive deviations estimates the same scale the weighted SD
#' does -- computed only from the half that contamination cannot reach.
#'
#' **Not a quantile difference.** `q84 - q50` was tried first and is wrong here:
#' with three marks and recency-skewed weights both quantiles land on the SAME
#' observation, the estimate is zero, and the fallback restores the contaminated
#' value. It failed silently in exactly the thin-history case it exists for. The
#' semi-deviation uses every good-side point, so one is enough.
#'
#' The returned value is on the GOOD-side scale, which is systematically
#' narrower than the pooled spread (measured 0.72-0.86 of it, by event). The
#' caller rescales it back onto the pooled scale using the population itself, so
#' that switching estimators changes robustness WITHOUT changing the overall
#' level of sigma -- otherwise the arm would confound the two.
#' @keywords internal
#' @noRd
.weighted_upper_sd <- function(x, w) {
  if (length(x) < 3L || !sum(w > 0)) return(NA_real_)
  med <- .weighted_quantile(x, w, 0.5)
  if (!is.finite(med)) return(NA_real_)
  dev <- x - med
  up <- dev > 0 & is.finite(dev) & w > 0
  if (!any(up)) return(NA_real_)
  s <- sqrt(sum(w[up] * dev[up]^2) / sum(w[up]))
  if (!is.finite(s) || s <= 0) return(NA_real_)
  s
}

#' Add an athlete's current momentum back to a momentum-neutral ability
#'
#' [estimate_ability()] strips the momentum each past mark was set under, so the
#' ability it returns describes an athlete in an average state of readiness.
#' That is the right thing to average over a career and the wrong thing to
#' forecast with: a championship field is not in average form, and the athletes
#' who arrive having raced hardest are systematically under-rated by it.
#'
#' This restores the other half — the momentum the athlete actually carries into
#' the race being predicted.
#'
#' Measured per family on the athletics harvest, going from a decayed race count
#' of 1 to 5: throw +2.19%, road +1.98%, middle +0.95%, hurdles +0.89%,
#' jump +0.84%, sprint +0.65%, distance +0.54%, walk +0.44%. Field events gain
#' most from being in rhythm.
#'
#' @param ability An ability table from [estimate_ability()], carrying
#'   `event_id`.
#' @param momentum Named numeric vector of current momentum, indexed by
#'   `athlete_id`, or a table with `athlete_id` and `momentum`.
#' @param calibration A calibration carrying a `momentum` table.
#' @return `ability` with `ability` shifted and a `momentum_now` column added.
#' @seealso [estimate_ability()]
#' @export
apply_momentum <- function(ability, momentum, calibration) {
  ab <- data.table::copy(data.table::as.data.table(ability))
  if (is.null(calibration$momentum) || !nrow(calibration$momentum)) return(ab[])
  if (!nrow(ab)) return(ab[])
  mv <- if (is.numeric(momentum)) {
    data.table::data.table(athlete_id = names(momentum),
                           momentum_now = as.numeric(momentum))
  } else {
    m <- data.table::copy(data.table::as.data.table(momentum))
    # Accept either column name. setnames() errors when `old` is zero-length, so
    # it cannot be used as a soft rename however tempting `skip_absent` looks.
    if (!"momentum_now" %in% names(m) && "momentum" %in% names(m)) {
      data.table::setnames(m, "momentum", "momentum_now")
    }
    if (!"momentum_now" %in% names(m)) {
      cli::cli_abort("{.arg momentum} needs a {.field momentum} or {.field momentum_now} column.")
    }
    m[, .(athlete_id = as.character(athlete_id), momentum_now)]
  }
  ab[, athlete_id := as.character(athlete_id)]
  ab[mv, on = "athlete_id", momentum_now := i.momentum_now]
  ab[is.na(momentum_now), momentum_now := 0]
  reg <- .citius_event_registry[, c("event_id", "family")]
  fam <- reg$family[match(ab$event_id, reg$event_id)]
  b <- calibration$momentum$beta[match(fam, calibration$momentum$family)]
  b[!is.finite(b)] <- 0
  # Scale by (1 - shrinkage) for the same reason ageing is: applying a form
  # adjustment to a number that is mostly the event mean adjusts the population,
  # not the athlete.
  if ("shrinkage" %in% names(ab)) b <- b * (1 - ab$shrinkage)
  ab[, ability := ability + b * momentum_now]
  ab[]
}

.asymmetric_huber_mean <- function(x, w, sig_target = .CITIUS_FALLBACK_CV, k = 2.5) {
  if (!length(x) || sum(w) <= 0) return(NA_real_)
  mu <- stats::weighted.mean(x, w)
  if (length(x) < 3L || !is.finite(sig_target) || sig_target <= 0) return(mu)
  dev <- x - mu
  cutoff <- -k * sig_target
  bad <- dev < cutoff
  if (!any(bad)) return(mu)
  w_rob <- w
  w_rob[bad] <- w[bad] * (abs(cutoff) / abs(dev[bad]))
  stats::weighted.mean(x, w_rob)
}

