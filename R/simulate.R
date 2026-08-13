#' Magnitude of shared race-condition effects
#'
#' Returns the standard deviation of the *common* shock applied to every athlete
#' in a race, on the log performance scale. Wind, track surface, temperature,
#' altitude and pace-setting move an entire field together; pool conditions
#' barely move a field at all.
#'
#' The value is **measured**, not assumed. Supply a `calibration` from
#' [calibrate()] and the shock is the de-biased standard deviation of fitted
#' race effects for that event. Only the *shared* component belongs here;
#' athlete-specific variation is carried by `sigma` from [estimate_ability()].
#'
#' Without a calibration, or for an event with too few observed fields, this
#' falls back to the registry's `cv_prior` — a coarse placeholder that exists
#' only so the simulator runs before any data has been harvested. Fallback
#' values are not estimates and should not be treated as such; check
#' `calibration$events$calibrated` to see which events are on real numbers.
#'
#' @param event_id Character vector of canonical event ids.
#' @param calibration Optional `citius_calibration` from [calibrate()].
#' @return Numeric vector of shared-shock standard deviations.
#' @seealso [calibrate()]
#' @export
race_conditions <- function(event_id, calibration = NULL) {
  reg <- .citius_event_registry
  idx <- match(event_id, reg$event_id)
  fallback <- reg$cv_prior[idx] * 0.25
  fallback[is.na(fallback)] <- 0.003

  vapply(seq_along(event_id), function(i) {
    .calibrated_value(calibration, event_id[i], "condition_sd", fallback[i])
  }, numeric(1))
}


#' Athlete-specific sensitivity to shared race conditions
#'
#' Returns a multiplier per athlete describing how strongly each is moved by the
#' shared condition shock drawn in [simulate_event()]. A value of `1` means the
#' athlete is affected exactly as much as the field average; `1.5` means half
#' again as much; `0` means unaffected.
#'
#' This function is what makes conditions capable of changing *placings*. The
#' shared shock on its own is a main effect and cancels out of every pairwise
#' comparison, so it can only move absolute marks. Sensitivity turns it into an
#' interaction: if athletes respond differently to a headwind, a cold pool or a
#' slow pace, then conditions genuinely reorder the field.
#'
#' @param ability A `data.table` of entrants as passed to [simulate_event()].
#' @param event_id Canonical event id for the race.
#' @param calibration Optional `citius_calibration` from [calibrate()]. Without
#'   one, every athlete is assigned a sensitivity of 1, which makes the shock a
#'   pure main effect that cannot reorder the field.
#' @return Numeric vector of multipliers, one per row of `ability`, in the same
#'   order. Rescaled so the field mean is 1, preserving the magnitude
#'   [race_conditions()] intends.
#' @seealso [calibrate()]
#' @export
condition_sensitivity <- function(ability, event_id, calibration = NULL) {
  n <- nrow(ability)
  if (is.null(calibration) || is.null(calibration$athlete) ||
      !"sensitivity" %in% names(calibration$athlete)) {
    return(rep(1, n))
  }

  s <- calibration$athlete[, c("athlete_id", "sensitivity")]
  idx <- match(as.character(ability$athlete_id), s$athlete_id)
  out <- s$sensitivity[idx]
  out[!is.finite(out)] <- 1

  # Renormalise to mean 1: the field-average response is already represented by
  # race_conditions(), so sensitivity must carry only the relative differences.
  m <- mean(out)
  if (is.finite(m) && m > 0) out <- out / m
  out
}


#' Simulate an event and return outcome probabilities
#'
#' Runs a Monte Carlo over a single race. Each simulation draws one shared
#' condition shock for the field, then one performance per athlete, then ranks
#' them. Repeating this many times converts point estimates of ability into the
#' distributional quantities that are actually interesting: medal chances,
#' finishing-position spreads and the odds of beating a given mark.
#'
#' Athlete performances are drawn from a scaled t distribution rather than a
#' normal. Elite results have fatter tails than Gaussian noise implies — the
#' occasional breakthrough run and the occasional blow-up both happen more often
#' than a normal would allow — and a t with modest degrees of freedom captures
#' that without extra parameters.
#'
#' @param ability A `data.table` from [estimate_ability()], filtered to the
#'   entrants in this race. Must contain `athlete_id`, `event_id`, `ability`
#'   and `sigma`.
#' @param n_sims Number of simulations.
#' @param condition_sd Shared-shock standard deviation. Defaults to
#'   [race_conditions()] for the event.
#' @param df Degrees of freedom for the performance t distribution. Defaults to
#'   the measured value from `calibration` (see [fit_tail_df()]), or 20.
#' @param foul_prob Probability that an athlete records no valid performance —
#'   a foul-out or no-height in a field event, a DNF or DNS on the track.
#'   Defaults to the measured rate from `calibration`.
#' @param taper Systematic shift applied to every athlete, on the log
#'   performance scale. Positive values make the field faster; use this to
#'   represent championship tapering.
#' @param form_sd Irreducible day-to-day form variation, on the log performance
#'   scale. Unlike `ability_se` this does **not** shrink as evidence
#'   accumulates. Defaults to the measured value from `calibration`; see
#'   [fit_form_sd()].
#' @param calibration Optional `citius_calibration` from [calibrate()]. Supplies
#'   the measured shared-shock magnitude, per-athlete condition sensitivity and
#'   foul rate. Without it the simulator falls back to registry placeholders.
#' @param condition_prior_weight Weight in `[0, 1]` for re-shrinking each
#'   entrant toward this field's own mean via [condition_prior()] before
#'   simulating. `0` (the default) leaves the ability table as supplied; only
#'   applied when the table carries `ability_raw`, `shrinkage` and `prior_mu`.
#' @param seed Optional integer seed for reproducibility. The RNG state is
#'   restored on exit, so a seeded simulation does not change the draws of
#'   whatever runs after it.
#' @return An object of class `citius_sim`: a list with the raw `perf` matrix
#'   (`n_sims` x athletes), `rank` matrix, the `ability` input and the settings
#'   used.
#' @seealso [medal_probs()], [prob_better_than()]
#' @examples
#' ab <- data.frame(
#'   athlete_id = c("a", "b", "c"),
#'   event_id = "AT-100Metres-M",
#'   ability = -log(c(9.8, 9.9, 10.0)),
#'   sigma = 0.008
#' )
#' sim <- simulate_event(ab, n_sims = 2000, seed = 1)
#' medal_probs(sim)
#' @export
# NOTE: no `round_class` parameter. One existed here, was accepted, and was
# never read by a single line of the body -- so `round_class = "heat"` bought a
# final's simulation with no warning. Removed 2026-08-13; if heat simulation is
# ever built, it needs the round-specific foul table (`calibration$foul_round`,
# currently on the wiring guard's KNOWN_UNREAD register) and a measured
# coasting treatment, not just an argument.
simulate_event <- function(ability, n_sims = 10000L, condition_sd = NULL,
                           df = NULL, foul_prob = NULL, taper = 0,
                           form_sd = NULL, calibration = NULL,
                           condition_prior_weight = 0.0, seed = NULL) {
  ab <- data.table::as.data.table(ability)
  req <- c("athlete_id", "event_id", "ability", "sigma")
  missing <- setdiff(req, names(ab))
  if (length(missing)) {
    cli::cli_abort("{.arg ability} is missing required column{?s}: {.field {missing}}.")
  }
  ab <- ab[!is.na(ability) & !is.na(sigma)]
  if (is.numeric(condition_prior_weight) && condition_prior_weight > 0 &&
      all(c("ability_raw", "shrinkage", "prior_mu") %in% names(ab))) {
    ab <- condition_prior(ab, field = ab$athlete_id, weight = condition_prior_weight)
  }
  n_ath <- nrow(ab)
  if (n_ath < 2L) cli::cli_abort("Need at least 2 entrants with a valid ability estimate.")

  event_id <- ab$event_id[1]
  if (data.table::uniqueN(ab$event_id) > 1L) {
    cli::cli_warn("Multiple events supplied; using {.val {event_id}} for condition and foul settings.")
  }

  if (is.null(condition_sd)) condition_sd <- race_conditions(event_id, calibration)
  if (is.null(df)) {
    # Measured tail weight where available. The previous hard-coded 6 put about
    # three times too much mass beyond two standard deviations, manufacturing
    # upsets and leaving favourites systematically under-rated.
    df <- if (!is.null(calibration) && is.numeric(calibration$tail_df) &&
              is.finite(calibration$tail_df)) calibration$tail_df else 20
  }
  reg_idx <- match(event_id, .citius_event_registry$event_id)
  is_technical <- isTRUE(.citius_event_registry$technical[reg_idx])
  orientation <- .citius_event_registry$orientation[reg_idx]
  if (is.na(orientation)) orientation <- -1L
  if (is.null(foul_prob)) {
    # The measured rate of recording no valid performance across the event.
    foul_prob <- .calibrated_value(calibration, event_id, "foul_rate", NA_real_)
    if (!is.finite(foul_prob)) {
      foul_prob <- 0
      cli::cli_warn(
        c("No calibrated no-mark rate for {.val {event_id}}; simulating with none.",
          i = "Measure it with {.fn calibrate} on {.fn athletics_competition_results} output - the athlete endpoint omits no-marks."),
        .frequency = "once", .frequency_id = "citius_no_foul_rate"
      )
    }
  }

  # Seeded runs restore the caller's RNG state on exit. A bare set.seed() here
  # silently pinned the GLOBAL stream, so any unseeded stochastic code running
  # AFTER a seeded simulation was unknowingly deterministic off this seed --
  # hidden coupling, not reproducibility. Callers that pass `seed` re-seed on
  # every call, so their own results are unchanged by the restore.
  if (!is.null(seed)) {
    old_seed <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      get(".Random.seed", envir = globalenv(), inherits = FALSE)
    } else NULL
    on.exit(
      if (is.null(old_seed)) {
        rm(".Random.seed", envir = globalenv())
      } else {
        assign(".Random.seed", old_seed, envir = globalenv())
      },
      add = TRUE
    )
    set.seed(seed)
  }
  n_sims <- as.integer(n_sims)

  # One shared shock per simulated race, common to the whole field.
  cond <- stats::rnorm(n_sims, mean = 0, sd = condition_sd)

  # Scaled t: unit-variance t, then scaled by each athlete's own sigma.
  scale_t <- sqrt((df - 2) / df)
  noise <- matrix(stats::rt(n_sims * n_ath, df = df) * scale_t,
                  nrow = n_sims, ncol = n_ath)

  # Split the draw into a good side and a bad side. Performance is not
  # symmetric: measured within athlete on 802,099 marks, the bad-side spread is
  # 1.36x (high jump) to 1.81x (pole vault) the good-side spread, and the worst
  # events are the ones where you can fail out of a competition. A single
  # symmetric sigma therefore simulates a good tail 12-39% wider than anything
  # the athlete has ever produced -- and a race is decided by the BEST draw, so
  # that surplus turns straight into win probability. See [fit_asymmetry()].
  asym <- .asymmetry_ratios(event_id, calibration)
  if (!is.null(asym)) {
    # Reshape only -- the mean must not move. For symmetric X, E[X+] = E|X|/2,
    # so scaling the sides by r_up and r_dn shifts the mean by
    # (r_up - r_dn) * E|X| / 2. Subtracting it keeps `median_mark` where it was
    # and makes this a pure PLACINGS change, which is what it is pre-registered
    # as: if marks move, the arm is confounded and the result is not usable.
    m_abs <- mean(abs(noise))          # E|X| of the SYMMETRIC draw, before scaling
    noise <- noise * data.table::fifelse(noise > 0, asym[["up"]], asym[["dn"]])
    noise <- noise - (asym[["up"]] - asym[["dn"]]) * m_abs / 2
  }

  # Sensitivity turns the shared shock from a main effect into an interaction,
  # which is the only way conditions can reorder the field. See
  # condition_sensitivity().
  sens <- condition_sensitivity(ab, event_id, calibration)
  if (length(sens) != n_ath || anyNA(sens)) {
    cli::cli_abort("{.fn condition_sensitivity} must return one non-missing value per entrant.")
  }

  # Two distinct uncertainties, both required:
  #   sigma      - how much an athlete varies around their own true ability
  #   ability_se - how little we know that ability (the EB posterior sd)
  # Treating the point estimate as exact makes the simulator over-confident
  # wherever performance noise is small, which is most acute in swimming: with
  # ~75% of race variation shared (and shared shocks not reordering a field),
  # sigma alone leaves almost nothing to separate the contenders.
  ability_se <- if ("ability_se" %in% names(ab)) ab$ability_se else rep(0, n_ath)
  ability_se[!is.finite(ability_se)] <- 0

  # A third term, and the one that does NOT shrink with evidence: form on the
  # day. `ability_se` tends to zero as results accumulate, so without this the
  # simulator becomes arbitrarily confident about well-known athletes -- which
  # is exactly where it was worst. Out-of-sample standardised residuals had sd
  # 1.18 for athletes with w_total < 0.5 but 1.74 for w_total > 4. See
  # [fit_form_sd()] for why it is a constant rather than a proportion.
  # Global, not per-event: it is measured as one constant across families
  # because that demonstrably normalises sd(z) better than a per-family or
  # proportional term. So it is read off the calibration directly rather than
  # through .calibrated_value(), which indexes the per-event table.
  if (is.null(form_sd)) {
    form_sd <- if (!is.null(calibration) && !is.null(calibration$form_sd)) {
      calibration$form_sd
    } else NA_real_
    if (!is.finite(form_sd)) form_sd <- 0
  }

  est_error <- matrix(stats::rnorm(n_sims * n_ath), nrow = n_sims, ncol = n_ath) *
    matrix(ability_se, nrow = n_sims, ncol = n_ath, byrow = TRUE)
  form_error <- if (form_sd > 0) {
    matrix(stats::rnorm(n_sims * n_ath, sd = form_sd), nrow = n_sims, ncol = n_ath)
  } else 0
  ab_sim <- if ("ability_peak" %in% names(ab)) ab$ability_peak else ab$ability
  perf <- matrix(ab_sim, nrow = n_sims, ncol = n_ath, byrow = TRUE) +
    est_error + form_error +
    noise * matrix(ab$sigma, nrow = n_sims, ncol = n_ath, byrow = TRUE) +
    outer(cond, sens) + taper

  fouled <- NULL
  if (foul_prob > 0) {
    fouled <- matrix(stats::runif(n_sims * n_ath) < foul_prob,
                     nrow = n_sims, ncol = n_ath)
    perf[fouled] <- -Inf   # no valid mark: ranks last, does not read as a slow mark
  }

  perf_std <- if ("ability_peak" %in% names(ab)) {
    p_std <- matrix(ab$ability, nrow = n_sims, ncol = n_ath, byrow = TRUE) +
      est_error + form_error +
      noise * matrix(ab$sigma, nrow = n_sims, ncol = n_ath, byrow = TRUE) +
      outer(cond, sens) + taper
    if (!is.null(fouled)) p_std[fouled] <- -Inf
    colnames(p_std) <- ab$athlete_id
    p_std
  } else NULL

  rank <- .rank_desc(perf)
  colnames(perf) <- colnames(rank) <- ab$athlete_id

  structure(
    list(
      perf = perf, perf_std = perf_std, rank = rank, ability = ab, event_id = event_id,
      orientation = orientation, n_sims = n_sims,
      settings = list(condition_sd = condition_sd, df = df,
                      foul_prob = foul_prob, taper = taper, form_sd = form_sd)
    ),
    class = "citius_sim"
  )
}


#' Summarise a simulation into medal and placing probabilities
#'
#' @param sim A `citius_sim` from [simulate_event()].
#' @param top_n Additional placing threshold to report. Default 8 (an Olympic
#'   final's scoring places).
#' @return A `data.table` ordered by win probability, with `p_gold`, `p_medal`,
#'   `p_top_n`, `median_rank` and the median simulated mark.
#' @export
medal_probs <- function(sim, top_n = 8L) {
  stopifnot(inherits(sim, "citius_sim"))
  r <- sim$rank
  p_std <- if (!is.null(sim$perf_std)) sim$perf_std else sim$perf

  med_perf <- apply(p_std, 2L, function(x) stats::median(x[is.finite(x)]))

  out <- data.table::data.table(
    athlete_id  = colnames(r),
    p_gold      = colMeans(r == 1L),
    p_medal     = colMeans(r <= 3L),
    p_top_n     = colMeans(r <= top_n),
    median_rank = apply(r, 2L, stats::median),
    median_mark = perf_to_mark(med_perf, sim$orientation)
  )
  data.table::setnames(out, "p_top_n", paste0("p_top", top_n))
  data.table::setorder(out, -p_gold)
  out[]
}


#' Probability of beating a mark
#'
#' Answers "what are the odds anyone goes under 9.80?" or "what is the chance
#' this athlete clears 6 metres?".
#'
#' @param sim A `citius_sim` from [simulate_event()].
#' @param mark Numeric threshold in the event's natural units (seconds, metres
#'   or points).
#' @param who Either `"each"` for per-athlete probabilities or `"any"` for the
#'   probability that at least one athlete in the field beats the mark.
#' @return A `data.table` for `"each"`, or a single numeric for `"any"`.
#' @export
prob_better_than <- function(sim, mark, who = c("each", "any")) {
  stopifnot(inherits(sim, "citius_sim"))
  who <- match.arg(who)
  threshold <- to_perf(mark, sim$orientation)
  beats <- sim$perf > threshold

  if (who == "any") return(mean(rowSums(beats) > 0L))

  data.table::data.table(
    athlete_id = colnames(sim$perf),
    prob = colMeans(beats)
  )[order(-prob)][]
}


#' Rank each simulated race, best performance first
#'
#' Ranking must never loop over `n_sims` at R level — that is what makes
#' `apply(perf, 1, rank)` two orders of magnitude too slow. **Do not
#' reintroduce it.** Instead the whole matrix is melted to one long vector and
#' ranked within simulation by a single `frankv()` call, which is C code
#' throughout.
#'
#' This replaced an all-pairs column comparison that was also loop-free over
#' simulations but O(fields^2): correct, and fine at a final's 8 lanes, but
#' 2.6s per 96-lane race against `frankv`'s 0.14s. Measured, `frankv` is faster
#' at every field size from 8 up, so there is no crossover to preserve.
#'
#' Fouled athletes carry `-Inf` and would otherwise tie with each other; they
#' are ranked among themselves at random so that no fouled athlete is
#' systematically credited with a better placing than another. Every returned
#' row is a strict permutation of `1:n` — see the sentinel comment below for
#' the floating-point trap that silently broke this.
#'
#' @param perf Numeric matrix, simulations by athletes.
#' @return Integer matrix of ranks, 1 = winner.
#' @keywords internal
#' @noRd
.rank_desc <- function(perf) {
  n_sims <- nrow(perf)
  n_ath <- ncol(perf)

  tie <- matrix(stats::runif(n_sims * n_ath), nrow = n_sims)
  key <- perf + tie * 1e-9
  fouled <- !is.finite(perf)
  # Fouls must sort below every valid mark while still being ordered randomly
  # among themselves. The sentinel cannot be -1e300: a double's ULP there is
  # ~1e284, so adding a small offset is annihilated and every fouled athlete
  # collapses onto one tied rank -- which the pairwise ranker then handed out
  # as duplicate placings, crediting all of them with a medal whenever enough
  # of a small field fouled. Keys are log-scale marks and never approach -1e6,
  # so this sits safely below them with the random order fully representable.
  key[fouled] <- -1e6 - tie[fouled]

  # Rank within simulation: order by (sim, -key) and the position within each
  # sim's block of n_ath is the placing. The jitter above has already made
  # every key distinct, so ties.method is never actually exercised.
  sim <- rep.int(seq_len(n_sims), n_ath)
  flat <- data.table::frankv(list(sim, -as.vector(key)), ties.method = "first")
  matrix(as.integer(flat - (sim - 1L) * n_ath), nrow = n_sims, ncol = n_ath)
}


#' @export
print.citius_sim <- function(x, ...) {
  cli::cli_h3("citius simulation: {x$event_id}")
  cli::cli_text("{x$n_sims} sims across {ncol(x$perf)} athletes")
  cli::cli_text("shared condition sd: {signif(x$settings$condition_sd, 3)} | df: {x$settings$df}")
  print(utils::head(medal_probs(x), 8L))
  invisible(x)
}
