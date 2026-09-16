#' A per-event empirical quantile reference for `transfer_neighbour_ability()`
#'
#' [transfer_neighbour_ability()] needs to know an athlete's percentile rank
#' among EVERYONE rated in an event, not just the handful of entrants in the
#' race being predicted. But the fast path every backtest and live prediction
#' actually uses, `estimate_ability(only = <entrants>)`, deliberately never
#' computes or returns other athletes' `ability_raw` -- that is the whole
#' point of the 85%+ runtime saving documented in `citius/CLAUDE.md`. Calling
#' [estimate_ability()] again without `only=` just to get the population
#' shape, once per race, measured at **~21 seconds for 4 events** (421k rows)
#' -- across a M1 backtest's 900+ meets that is **3-5 added hours**,
#' for a quantity (the relative SHAPE of two events' ability distributions)
#' that moves slowly.
#'
#' This builds that reference ONCE, meant to be cached per coarse time bucket
#' (a calendar year, say) rather than per meet or per exact `as_of`. The
#' honest cost of that shortcut: a meet early in a bucket gets a reference
#' built from data that includes results from later in the same bucket --
#' mild look-ahead. It only affects the population's SHAPE (why 1500m draws a
#' deeper field than 5000m), never an individual athlete's own form, so it is
#' a much softer leak than a results leak, but it is one, and a caller
#' choosing bucket width is trading it against runtime.
#'
#' @param results A results table, as [estimate_ability()] takes.
#' @param events Character vector of `event_id`s to build a reference for --
#'   only the events that actually appear in some `neighbour_r2` table, not
#'   every event in the registry, since every additional event is more rows
#'   the underlying [estimate_ability()] call has to process.
#' @param as_of Passed through to [estimate_ability()]. The population is
#'   built from every result on or before this date.
#' @param ... Passed through to [estimate_ability()] (e.g. `calibration`).
#' @return A `data.table` with `event_id`, `pctl` and `ability_raw`, sorted
#'   within `event_id` by `pctl`, suitable as the `rank_reference` argument to
#'   [transfer_neighbour_ability()].
#' @seealso [transfer_neighbour_ability()]
#' @export
build_neighbour_rank_reference <- function(results, events, as_of = Sys.Date(), ...) {
  dt <- data.table::as.data.table(results)
  dt <- dt[!is.na(perf) & !is.na(event_id) & event_id %in% events]
  if (!is.null(dt$date)) dt <- dt[date <= as_of]
  ab <- suppressWarnings(estimate_ability(dt, as_of = as_of, ...))
  if (!nrow(ab)) {
    return(data.table::data.table(event_id = character(), pctl = numeric(),
                                   ability_raw = numeric()))
  }
  ab[, .rank_n := .N, by = event_id]
  ref <- ab[.rank_n >= 2L,
            .(pctl = (data.table::frank(ability_raw, ties.method = "average") - 0.5) / .rank_n,
              ability_raw = ability_raw),
            by = event_id]
  data.table::setorder(ref, event_id, pctl)
  ref[]
}

#' Blend a neighbouring event's ability into a thin or tactically-distorted rating
#'
#' [estimate_ability()] groups everything `by = .(athlete_id, event_id)`, so an
#' athlete's rating in one event never sees his marks in any other. That is
#' wrong when two events are close enough that form genuinely transfers:
#' Ingebrigtsen's 2026 5000m record was entirely tactical championship finals,
#' while three fast Diamond League 1500m finals in the same window were
#' invisible to his 5000m card (Budapest 2026, see `NEXT-STEPS.md` and
#' `docs/reference/hypothesis-registry.md`, "An athlete's form in a
#' NEIGHBOURING event...").
#'
#' This is a post-processing step on an already-estimated ability table, the
#' same pattern [condition_prior()] uses, rather than a change inside
#' [estimate_ability()] itself.
#'
#' Three standardisation issues were found and fixed while building this
#' against Ingebrigtsen's real 2026 numbers, worth naming because each one
#' silently reverses the direction of the transfer for the case that
#' motivated it, or makes it too expensive to run at all:
#'
#' 1. A raw `(ability_raw - prior_mu) / sigma_between` z-score is not
#'    comparable across events, because `sigma_between` differs a great deal
#'    between them -- a shallow field like 5000m versus a deep one like
#'    1500m, which draws far more sub-elite entries. Measured 2026-09-13:
#'    this ranked Ingebrigtsen's 5000m (z = 2.39, 47th of 7,622) ABOVE his
#'    1500m (z = 1.59, 4th of 14,373) even though 4th of 14,373 is the deeper
#'    result. Fixed by using percentile rank among everyone rated in that
#'    event, which is shape-invariant and matches what the `r2` figures were
#'    measured on (Spearman rank correlation).
#' 2. Converting that rank back into the target event's raw units by
#'    multiplying by the TARGET's `sigma_between` reintroduces the same
#'    parametric assumption on the other side. Fixed by mapping the
#'    (correlation-shrunk) implied percentile onto the target event's *own
#'    empirical* distribution of `ability_raw` (quantile-to-quantile).
#' 3. Getting that empirical distribution from `ability` itself requires
#'    every OTHER athlete's `ability_raw` in both events, which conflicts
#'    with `estimate_ability(only = <entrants>)`, the fast path every real
#'    caller uses and that deliberately never computes that. Fixed by taking
#'    the population reference as a separate, precomputed argument -- see
#'    [build_neighbour_rank_reference()] for how to build and cache one
#'    cheaply.
#'
#' Concretely: the neighbour's `ability_raw` is looked up against
#' `rank_reference` to get its percentile, converted to a normal quantile
#' `z`, shrunk toward the target event's median by the correlation
#' (`sqrt(r2) * z`, standard regression-to-the-mean for two variables
#' correlated at `r`), turned back into a probability with `pnorm()`, and
#' that probability is looked up against the target event's row of
#' `rank_reference` to get an implied raw ability on the target's actual
#' scale. That implied value is blended into the athlete's own `ability_raw`
#' (from `ability`, not `rank_reference`) with weight `r2 * (one race's
#' worth of weight in the target event for this athlete)`, so a neighbour
#' mark never counts for more than `r2` of a real race, and an athlete with
#' several real races in the target event is barely moved by it. `ability`
#' is then rebuilt from the blended `ability_raw` using the athlete's
#' EXISTING `shrinkage` -- this adds evidence about where the athlete's true
#' ability sits, it does not re-argue how confident the estimator should be,
#' so `shrinkage`/`kappa`/`w_total` are left alone.
#'
#' @param ability An ability table from [estimate_ability()] -- the fast
#'   `only=`-restricted path is fine, as long as `results` there spanned both
#'   the target and neighbour events for these athletes (so their neighbour
#'   `ability_raw` is present too, just not the rest of the population's).
#' @param neighbour_r2 A `data.table`/`data.frame` with `event_id`,
#'   `neighbour_event_id` and `r2` -- the squared Spearman correlation between
#'   an athlete's within-event-season standing in the two events, elite cut.
#'   Deliberately not baked in: these are measured numbers (see
#'   `docs/reference/hypothesis-registry.md`, corrected 2026-09-12 after an
#'   orientation bug), not model constants, and only pairs that were actually
#'   measured should be passed. Assumes a positive correlation, which is what
#'   every measured pair so far is. Defaults to `NULL`, a true no-op.
#' @param rank_reference A population quantile table from
#'   [build_neighbour_rank_reference()], covering both sides of every edge in
#'   `neighbour_r2`. Required whenever `neighbour_r2` is supplied.
#' @return `ability` with `ability_raw` and `ability` updated for every
#'   athlete-event row whose neighbour event is also present in `ability`;
#'   everything else, including rows with no matching neighbour, is untouched.
#' @seealso [estimate_ability()], [condition_prior()],
#'   [build_neighbour_rank_reference()]
#' @export
transfer_neighbour_ability <- function(ability, neighbour_r2 = NULL, rank_reference = NULL) {
  ab <- data.table::copy(data.table::as.data.table(ability))
  if (is.null(neighbour_r2) || !nrow(ab)) return(ab[])

  req <- c("athlete_id", "event_id", "ability_raw", "prior_mu", "shrinkage",
           "w_total", "n_eff")
  missing_cols <- setdiff(req, names(ab))
  if (length(missing_cols)) {
    cli::cli_abort(c(
      "{.arg ability} must come from {.fn estimate_ability}.",
      "x" = "Missing column{?s}: {.field {missing_cols}}."
    ))
  }

  nb <- data.table::as.data.table(neighbour_r2)
  req_nb <- c("event_id", "neighbour_event_id", "r2")
  if (!all(req_nb %in% names(nb))) {
    cli::cli_abort("{.arg neighbour_r2} must have columns {.field {req_nb}}.")
  }
  if (!nrow(nb)) return(ab[])
  if (is.null(rank_reference) || !nrow(rank_reference)) {
    cli::cli_abort(c(
      "{.arg rank_reference} is required whenever {.arg neighbour_r2} is supplied.",
      "i" = "Build one with {.fn build_neighbour_rank_reference}."
    ))
  }
  ref <- data.table::as.data.table(rank_reference)
  req_ref <- c("event_id", "pctl", "ability_raw")
  if (!all(req_ref %in% names(ref))) {
    cli::cli_abort("{.arg rank_reference} must have columns {.field {req_ref}}.")
  }
  data.table::setorder(ref, event_id, pctl)

  # Forward lookup (value -> percentile): needs pctl as x, ability_raw as y,
  # inverted at call time. Approx requires a strictly increasing x, so ties in
  # ability_raw (a plateau in the reference) are nudged apart by a
  # vanishingly small amount rather than dropped -- dropping would silently
  # thin the reference exactly where the population is most bunched.
  pctl_of <- function(event, value) {
    r <- ref[event_id == event]
    if (nrow(r) < 2L) return(rep(NA_real_, length(value)))
    x <- r$ability_raw + seq_len(nrow(r)) * .Machine$double.eps * pmax(abs(r$ability_raw), 1)
    stats::approx(x, r$pctl, xout = value, rule = 2)$y
  }
  raw_at <- function(event, pctl) {
    r <- ref[event_id == event]
    if (nrow(r) < 2L) return(rep(NA_real_, length(pctl)))
    stats::approx(r$pctl, r$ability_raw, xout = pctl, rule = 2)$y
  }

  own <- ab[, .(athlete_id, event_id, ability_raw, prior_mu, shrinkage,
                w_total, n_eff)]

  edges <- merge(nb, own, by = "event_id", allow.cartesian = TRUE)
  data.table::setnames(
    edges,
    c("ability_raw", "prior_mu", "shrinkage", "w_total", "n_eff"),
    c("tgt_ability_raw", "tgt_prior_mu", "tgt_shrinkage", "tgt_w_total", "tgt_n_eff")
  )

  neigh <- own[, .(athlete_id, neighbour_event_id = event_id,
                    nb_ability_raw = ability_raw)]
  edges <- merge(edges, neigh, by = c("athlete_id", "neighbour_event_id"))
  if (!nrow(edges)) return(ab[])

  edges[, nb_pctl := pctl_of(neighbour_event_id[1], nb_ability_raw), by = neighbour_event_id]
  edges <- edges[is.finite(nb_pctl)]
  if (!nrow(edges)) return(ab[])

  edges[, implied_pctl := stats::pnorm(sqrt(r2) * stats::qnorm(nb_pctl))]
  edges[, implied_raw := raw_at(event_id[1], implied_pctl), by = event_id]
  edges <- edges[is.finite(implied_raw)]
  if (!nrow(edges)) return(ab[])

  # "One real race's worth of weight in the target event, for this athlete" --
  # w_total is the athlete's total weighted evidence there, n_eff its
  # effective race count, so w_total / n_eff is the average per-race weight.
  edges[, w_race := tgt_w_total / pmax(tgt_n_eff, 1e-6)]
  edges[, w_neighbour := r2 * w_race]

  edges[, ability_raw_new := (tgt_w_total * tgt_ability_raw + w_neighbour * implied_raw) /
                              (tgt_w_total + w_neighbour)]
  edges[, ability_new := (1 - tgt_shrinkage) * ability_raw_new + tgt_shrinkage * tgt_prior_mu]

  # An athlete can have more than one usable neighbour into the same target
  # event (a 5000m man with both a 3000m and a 10000m on the books). Take the
  # edge with the larger r2 rather than averaging edges of different
  # reliability together.
  data.table::setorder(edges, athlete_id, event_id, -r2)
  edges <- edges[, .SD[1L], by = .(athlete_id, event_id)]

  ab <- merge(ab, edges[, .(athlete_id, event_id, ability_raw_new, ability_new)],
              by = c("athlete_id", "event_id"), all.x = TRUE)
  ab[!is.na(ability_raw_new), `:=`(ability_raw = ability_raw_new, ability = ability_new)]
  ab[, c("ability_raw_new", "ability_new") := NULL]
  ab[]
}
