#' Simulate a championship through its rounds
#'
#' [simulate_event()] answers "who wins this race, given the field". A
#' championship asks something harder: the final field is not known, because
#' athletes must first survive heats and semi-finals. Medal probabilities that
#' condition on a known final overstate every contender, since none of them is
#' certain to be there.
#'
#' @section What is drawn once and what is drawn per race:
#' This is the modelling decision that matters, and it is not the same for each
#' term:
#'
#' * `ability_se` (how little we know the athlete) and `form_sd` (their form
#'   this week) are drawn **once per championship**. They describe the athlete
#'   over the meet, so an athlete who is below par is below par in the heat and
#'   again in the final. Redrawing them per round would let a bad heat be
#'   forgotten by the final and make qualification far too forgiving.
#' * `sigma` (race-to-race execution) and the shared race shock are drawn
#'   **per race**, because they are properties of the race.
#'
#' Getting this backwards is the easy mistake: it makes progression look like
#' independent coin flips and understates how strongly a heat predicts a final.
#'
#' @section Lane allocation:
#' Athletes are snake-seeded on ability, which is what championships approximate
#' when they seed on season's best — the strongest are spread across heats
#' rather than drawn together. Random allocation would be materially different:
#' it lets two contenders collide in a heat and eliminate one, inflating upsets.
#'
#' @param ability Entrants, as returned by [estimate_ability()].
#' @param structure A list of rounds, each a list with `races` (how many races
#'   in the round), `advance` (automatic qualifiers per race) and optionally
#'   `fastest_losers` (further qualifiers on time across the round). The last
#'   round is the final and needs only `races = 1`.
#' @param n_sims Simulations.
#' @param calibration Optional `citius_calibration`.
#' @param seed Optional integer seed.
#' @return A `data.table` with one row per athlete: `p_reach_*` for each round
#'   after the first, plus `p_gold`, `p_medal` and `p_final`.
#' @examples
#' \dontrun{
#' simulate_rounds(entrants, structure = list(
#'   list(races = 3, advance = 2, fastest_losers = 2),
#'   list(races = 1)))
#' }
#' @export
simulate_rounds <- function(ability, structure, n_sims = 10000L,
                            calibration = NULL, seed = NULL) {
  ab <- data.table::as.data.table(ability)
  if (!nrow(ab)) cli::cli_abort("{.arg ability} is empty.")
  if (!length(structure)) cli::cli_abort("{.arg structure} needs at least one round.")
  if (!is.null(seed)) set.seed(seed)

  n_ath <- nrow(ab)
  event_id <- ab$event_id[1]
  sigma <- ab$sigma
  a_se <- if ("ability_se" %in% names(ab)) ab$ability_se else rep(0, n_ath)
  a_se[!is.finite(a_se)] <- 0
  form_sd <- if (!is.null(calibration) && !is.null(calibration$form_sd)) {
    calibration$form_sd
  } else 0
  if (!is.finite(form_sd)) form_sd <- 0

  # Drawn ONCE for the whole championship - see the note above. These follow the
  # athlete from the heat to the final.
  meet_offset <- matrix(stats::rnorm(n_sims * n_ath), n_sims, n_ath) *
    matrix(a_se, n_sims, n_ath, byrow = TRUE) +
    matrix(stats::rnorm(n_sims * n_ath, sd = form_sd), n_sims, n_ath)
  base <- matrix(ab$ability, n_sims, n_ath, byrow = TRUE) + meet_offset

  df <- .calibrated_value(calibration, event_id, "tail_df", NA_real_)
  if (!is.finite(df)) df <- if (!is.null(calibration) && is.finite(calibration$tail_df %||% NA)) {
    calibration$tail_df
  } else 20
  cond_sd <- race_conditions(event_id, calibration)
  sens <- condition_sensitivity(ab, event_id, calibration)

  alive <- matrix(TRUE, n_sims, n_ath)
  out <- list()

  for (ri in seq_along(structure)) {
    rd <- structure[[ri]]
    n_races <- max(1L, as.integer(rd$races %||% 1L))

    # Race allocation. In the first round athletes are snake-seeded on ability;
    # later rounds snake-seed on standing among survivors, which varies by
    # simulation and so is computed from the live ranks.
    if (ri == 1L) {
      seed_rank <- matrix(rank(-ab$ability, ties.method = "first"),
                          n_sims, n_ath, byrow = TRUE)
    } else {
      seed_rank <- .rank_alive(base, alive)
    }
    race_of <- .snake(seed_rank, n_races)
    race_of[!alive] <- NA_integer_

    # Per-race terms: execution noise and the shared shock, redrawn each round.
    noise <- matrix(stats::rt(n_sims * n_ath, df = df), n_sims, n_ath) /
      sqrt(df / (df - 2))
    perf <- base + noise * matrix(sigma, n_sims, n_ath, byrow = TRUE)
    for (k in seq_len(n_races)) {
      shock <- stats::rnorm(n_sims, sd = cond_sd)
      inr <- !is.na(race_of) & race_of == k
      perf[inr] <- perf[inr] + (outer(shock, sens)[inr])
    }
    perf[!alive] <- -Inf

    if (ri == length(structure)) {
      pos <- .rank_alive(perf, alive)
      out$p_gold <- colMeans(alive & pos == 1L)
      out$p_medal <- colMeans(alive & pos <= 3L)
      break
    }

    adv <- .advancers(perf, race_of, alive, n_races,
                      as.integer(rd$advance %||% 0L),
                      as.integer(rd$fastest_losers %||% 0L))
    alive <- adv
    out[[paste0("p_reach_r", ri + 1L)]] <- colMeans(alive)
  }

  res <- data.table::data.table(athlete_id = ab$athlete_id, event_id = event_id)
  # set() rather than [[<-, which takes a shallow copy and warns.
  for (nm in names(out)) data.table::set(res, j = nm, value = out[[nm]])
  final_col <- if (length(structure) > 1L) {
    out[[paste0("p_reach_r", length(structure))]]
  } else rep(1, n_ath)
  data.table::set(res, j = "p_final", value = final_col)
  data.table::setorder(res, -p_gold)
  res[]
}


#' Rank within simulation among live athletes only
#' @keywords internal
#' @noRd
.rank_alive <- function(perf, alive) {
  p <- perf
  p[!alive] <- -Inf
  r <- .rank_desc(p)
  r[!alive] <- NA_integer_
  r
}

#' Snake-seed ranks into races
#'
#' Serpentine rather than modulo: ranks 1..n over 3 races go 1,2,3,3,2,1,...
#' Modulo would load race 1 with every top seed.
#' @keywords internal
#' @noRd
.snake <- function(rank_mat, n_races) {
  r0 <- rank_mat - 1L
  cyc <- r0 %% (2L * n_races)
  ifelse(cyc < n_races, cyc + 1L, 2L * n_races - cyc)
}

#' Who advances: automatic qualifiers per race, then fastest losers
#' @keywords internal
#' @noRd
.advancers <- function(perf, race_of, alive, n_races, advance, fastest_losers) {
  n_sims <- nrow(perf)
  out <- matrix(FALSE, n_sims, ncol(perf))
  if (advance > 0L) {
    for (k in seq_len(n_races)) {
      inr <- !is.na(race_of) & race_of == k & alive
      pk <- perf
      pk[!inr] <- -Inf
      rk <- .rank_desc(pk)
      out <- out | (inr & rk <= advance)
    }
  }
  if (fastest_losers > 0L) {
    # Ranked on mark across everyone who missed automatic qualification,
    # which is how championships fill the remaining lanes.
    left <- perf
    left[out | !alive] <- -Inf
    rl <- .rank_desc(left)
    out <- out | (alive & !out & rl <= fastest_losers)
  }
  out
}
