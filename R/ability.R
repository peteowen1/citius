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
  t_eff[, eff := eff - eff[tier_class == "top"][1]]
  if (!nrow(t_eff[tier_class == "top"])) t_eff[, eff := eff - max(eff)]

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
#'   by [athletics_athlete_results()] or [aquatics_results()].
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
                             adjust_context = TRUE, calibration = NULL,
                             robust_sigma = TRUE,
                             sigma_parts = c("estimator", "weight"),
                             sigma_mode = c("athlete", "event"),
                             only = NULL, peak_gamma = 0,
                             robust_location = FALSE,
                             decouple_peak = FALSE) {
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
    # Named, not positional. The positional form passed 13 arguments into 14
    # slots -- `only` landed in `sigma_mode`, and everything after it shifted by
    # one. Harmless only because `results[0]` returns at the `!nrow(results)`
    # guard above before any shifted argument is read, which is a property of
    # the call site rather than of the code, and would break silently the day
    # anything is added ahead of that guard.
    return(estimate_ability(results[0], as_of = as_of, half_life = half_life,
                            trim_tactical = trim_tactical,
                            min_results = min_results,
                            adjust_context = adjust_context,
                            calibration = calibration,
                            robust_sigma = robust_sigma,
                            sigma_parts = sigma_parts,
                            sigma_mode = sigma_mode, only = only,
                            peak_gamma = peak_gamma,
                            robust_location = robust_location,
                            decouple_peak = decouple_peak))
  }

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
  dt[, w := result_weight(date, tier = if ("tier" %in% names(dt)) tier else NA_character_,
                          round = if ("round" %in% names(dt)) round else NA_character_,
                          as_of = as_of, half_life = hl,
                          calibration = calibration)]

  if (is.numeric(peak_gamma) && peak_gamma > 0) {
    dt[, .q := data.table::frank(perf, ties.method = "first") / .N, by = .(athlete_id, event_id)]
    dt[, w := w * (.q^peak_gamma)]
    dt[, .q := NULL]
  }

  reg <- .citius_event_registry[, c("event_id", "tactical", "cv_prior")]
  dt <- merge(dt, reg, by = "event_id", all.x = TRUE, sort = FALSE)
  dt[is.na(tactical), tactical := FALSE]

  # Prefer a measured tactical signal over the registry's hand-set flag. Races
  # in tactical events skew slow, so a strongly negative skew in the fitted race
  # effects is direct evidence that times decouple from ability.
  if (!is.null(calibration) && !is.null(calibration$events)) {
    ti <- calibration$events[, c("event_id", "tactical_index", "calibrated")]
    dt <- merge(dt, ti, by = "event_id", all.x = TRUE, sort = FALSE)
    dt[calibrated %in% TRUE & is.finite(tactical_index), tactical := tactical_index < -0.5]
  }

  if (isTRUE(adjust_context)) {
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
  }

  if (trim_tactical > 0) {
    # Vectorised rank-and-filter, not `.SD[...]` per group. The `.SD` form made
    # data.table materialise a sub-table for every athlete-event group and cost
    # 74% of this function's runtime; the work itself is just "drop the worst
    # k marks", which needs no sub-table at all.
    dt[, .keep := TRUE]
    dt[tactical == TRUE, .grp_n := .N, by = .(athlete_id, event_id)]
    dt[tactical == TRUE & .grp_n >= 4L,
       .rk := data.table::frank(perf, ties.method = "first"),
       by = .(athlete_id, event_id)]
    # frank is ascending and perf is oriented so higher is better: rank 1 is the
    # worst mark, which is what the tactical trim removes.
    dt[tactical == TRUE & .grp_n >= 4L,
       .keep := .rk > floor(.grp_n * trim_tactical)]
    dt <- dt[.keep == TRUE]
    dt[, c(".keep", ".grp_n", ".rk") := NULL]
  }

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
        if (!is.finite(sig_ref) || sig_ref <= 0) sig_ref <- 0.02
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
    if (!nrow(dt)) return(estimate_ability(results[0], as_of, half_life, trim_tactical,
                                           min_results, adjust_context, calibration,
                                           robust_sigma, sigma_parts))
  }

  ab <- dt[, {
    ok <- w > 0
    mu <- if (sum(ok)) {
      if (isTRUE(robust_location) && !isTRUE(decouple_peak)) {
        sig_ref <- data.table::first(cv_prior)
        if (!is.finite(sig_ref) || sig_ref <= 0) sig_ref <- 0.02
        .asymmetric_huber_mean(perf[ok], w[ok], sig_target = sig_ref, k = 2.5)
      } else {
        stats::weighted.mean(perf[ok], w[ok])
      }
    } else NA_real_

    mu_peak <- if (sum(ok) && isTRUE(decouple_peak)) {
      sig_ref <- data.table::first(cv_prior)
      if (!is.finite(sig_ref) || sig_ref <= 0) sig_ref <- 0.02
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
  if (!nrow(ab)) return(estimate_ability(results[0], as_of, half_life, trim_tactical,
                                         min_results, adjust_context, calibration,
                                         robust_sigma, sigma_parts))

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
    match.arg(sigma_parts, c("estimator", "weight", "target"), several.ok = TRUE)
  } else character()
  use_estimator <- "estimator" %in% parts
  use_target    <- "target" %in% parts
  use_weight    <- "weight" %in% parts

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

  # Shrink toward the MEASURED within-athlete spread for the event, falling back
  # to the registry placeholder only where no calibration exists.
  ab[, sigma_target := cv_prior]
  if (use_target && !is.null(calibration) && !is.null(calibration$events)) {
    tgt <- data.table::as.data.table(calibration$events)
    if (all(c("event_id", "sigma_within") %in% names(tgt))) {
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
      ab[is.finite(sw) & sw > 0, sigma_target := sw[is.finite(sw) & sw > 0]]
    }
  }
  ab[, sigma := data.table::fifelse(is.na(sigma) | sigma <= 0, sigma_target, sigma)]

  # A two-race athlete's sample spread is close to meaningless on its own, so
  # blend toward the event value by absolute evidence.
  shrink_w <- if (use_weight) ab$w_total else ab$n_eff
  ab[, sigma := (shrink_w * sigma + 2 * sigma_target) / (shrink_w + 2)]

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
  }

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
  }

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

  ab[, ability_se := sigma / sqrt(w_total + kappa)]

  if (!is.null(only)) ab <- ab[as.character(athlete_id) %in% as.character(only)]

  cols <- c("athlete_id", "event_id", "ability", "ability_raw", "sigma",
            "ability_se", "n", "n_eff", "w_total", "shrinkage", "prior_mu",
            "age_ref", "last_date")
  if ("ability_peak" %in% names(ab)) cols <- c(cols, "ability_peak")
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

.asymmetric_huber_mean <- function(x, w, sig_target = 0.02, k = 2.5) {
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

