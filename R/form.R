#' Estimate irreducible form variation
#'
#' `sigma` measures how much an athlete varies around their own ability.
#' `ability_se` measures how little we know that ability, and shrinks toward
#' zero as evidence accumulates. Together they were the simulator's whole
#' predictive spread — and that spread was measurably too narrow.
#'
#' Out-of-sample standardised residuals had sd **1.55** rather than 1, and the
#' miss grew with evidence: sd 1.18 for athletes with `w_total < 0.5` against
#' 1.74 for `w_total > 4`. That direction is the diagnostic. If `ability_se`
#' were merely understated, thinly-observed athletes would be worst. Instead the
#' *best*-known athletes were worst, because `ability_se` drives their
#' uncertainty toward zero while their form on a given day still varies.
#'
#' What is missing is variance that **does not shrink with evidence**: taper,
#' illness, training phase, motivation on the day. This function measures it as
#' the excess of observed out-of-sample residual variance over what the model
#' claims:
#'
#' \deqn{\tau^2 = \mathrm{var}(\mathrm{perf} - \mathrm{ability}) -
#'   \mathrm{mean}(\sigma^2 + \mathrm{se}^2)}
#'
#' Held-out by construction: ability for each meet is estimated only from
#' results dated before it, so `tau` cannot absorb in-sample overfitting the way
#' a residual-variance estimate would.
#'
#' @section Why a constant, not a proportion:
#' `sigma` ranges from 0.015 (sprint) to 0.061 (throw), so "constant" and
#' "proportional to each family's spread" are materially different designs.
#' Measured on 11k held-out residuals, a constant normalises sd(z) far better —
#' mean \eqn{|sd - 1|} of 0.062 across families against 0.297 — and it is
#' self-limiting in the right direction: adding 0.017 in quadrature nearly
#' doubles a sprinter's 0.015 spread, where over-confidence was worst
#' (raw sd 1.88), but moves a thrower's 0.061 by 4%, where it was already fine
#' (raw sd 1.11). Throws and road running need no correction at all.
#'
#' @param results Canonical results with `perf`, `event_id`, `date` and
#'   `competition_id`.
#' @param calibration A `citius_calibration`, used for the ability estimates.
#' @param half_life Recency half-life in days, matching prediction-time use.
#' @param n_meets Meets to sample across the period. More is slower and the
#'   estimate is stable well below the default.
#' @param min_history Minimum prior rows before a meet is usable.
#' @return A one-row `data.table` with `form_sd`, `n` residuals, and the
#'   `sd_before` / `sd_after` standardised residual sd it achieves.
#' @examples
#' \dontrun{
#' fit_form_sd(clean, calibration, half_life = 730)
#' }
#' @export
fit_form_sd <- function(results, calibration = NULL, half_life = 730,
                        n_meets = 150L, min_history = 2000L) {
  dt <- data.table::as.data.table(results)
  need <- c("perf", "event_id", "date", "competition_id", "round")
  if (!all(need %in% names(dt))) {
    cli::cli_abort("{.arg results} must contain {.field {setdiff(need, names(dt))}}.")
  }
  dt <- dt[!is.na(event_id) & !is.na(perf) & !is.na(date)]
  if (!nrow(dt)) return(.empty_form_fit())

  finals <- dt[grepl("final", round, ignore.case = TRUE) &
                 !grepl("semi", round, ignore.case = TRUE)]
  if (!nrow(finals)) return(.empty_form_fit())

  pool <- unique(finals[, .(cut = min(date, na.rm = TRUE)), by = competition_id])
  pool <- pool[is.finite(as.numeric(cut))]
  data.table::setorder(pool, cut)
  if (nrow(pool) > n_meets) pool <- pool[round(seq(1, .N, length.out = n_meets))]

  out <- vector("list", nrow(pool))
  for (i in seq_len(nrow(pool))) {
    cid <- pool$competition_id[i]
    cut <- pool$cut[i]
    blk <- finals[competition_id == cid]
    past <- dt[date < cut & date >= cut - 4380 & event_id %in% unique(blk$event_id)]
    if (nrow(past) < min_history) next
    ab <- estimate_ability(past, as_of = cut, half_life = half_life,
                           calibration = calibration)
    if (!nrow(ab)) next
    m <- merge(blk[, .(athlete_id = as.character(athlete_id), event_id, perf)],
               ab[, .(athlete_id, event_id, ability, sigma, ability_se)],
               by = c("athlete_id", "event_id"))
    if (nrow(m)) out[[i]] <- m
  }
  res <- data.table::rbindlist(Filter(Negate(is.null), out))
  if (!nrow(res)) return(.empty_form_fit())

  res[, base_var := sigma^2 + ability_se^2]
  res <- res[is.finite(base_var) & base_var > 0 & is.finite(perf) & is.finite(ability)]
  if (nrow(res) < 100L) return(.empty_form_fit())

  # Solve for the tau that makes standardised residuals unit variance, rather
  # than moment-matching `var(resid) - mean(base_var)`. Those are NOT the same
  # quantity when base_var varies across rows, and here it varies by ~3x with
  # evidence: rows with small base_var dominate mean(z^2) while contributing
  # little to the unweighted variance. The moment-matched version returned zero
  # on data whose standardised residuals had sd 1.34 -- an estimator that
  # disagrees with its own target.
  r2 <- (res$perf - res$ability)^2
  ez2 <- function(tau) mean(r2 / (res$base_var + tau^2)) - 1
  form_sd <- if (ez2(0) <= 0) {
    0                                   # already dispersed enough; nothing to add
  } else {
    hi <- sqrt(max(r2))                 # brackets the root: ez2 is decreasing in tau
    if (ez2(hi) > 0) hi else stats::uniroot(ez2, c(0, hi), tol = 1e-6)$root
  }

  z0 <- (res$perf - res$ability) / sqrt(res$base_var)
  z1 <- (res$perf - res$ability) / sqrt(res$base_var + form_sd^2)
  data.table::data.table(form_sd = form_sd, n = nrow(res),
                         sd_before = stats::sd(z0), sd_after = stats::sd(z1))
}

#' @keywords internal
#' @noRd
.empty_form_fit <- function() {
  data.table::data.table(form_sd = NA_real_, n = 0L,
                         sd_before = NA_real_, sd_after = NA_real_)
}
