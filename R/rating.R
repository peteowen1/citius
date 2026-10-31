#' Rating scale: expected medal chance against a reference field
#'
#' Each athlete's probability of medalling in a *reference field* for their
#' event -- "this athlete medals 40% of the time against a championship field".
#' Unlike the z-score, percentile and %-of-best scales it is meaningful without
#' knowing anything about log-marks, but it is also the only one that is not a
#' pure transform of `ability`: it needs the simulator's own semantics (shared
#' condition shocks and per-athlete sensitivity, `tail_df`, `foul_rate`,
#' asymmetry, estimation error, form). Those stay in [simulate_event()]; this
#' function never re-implements them.
#'
#' **Method.** The reference field and the athletes being rated go through one
#' [simulate_event()] call together, so every athlete meets the same condition
#' shock as the reference field in every simulation. In each simulation the
#' bronze line is the third-best performance *among the reference athletes*,
#' and a rated athlete medals when their own draw beats it. That is one
#' simulation of the field, not one per athlete; athletes are processed in
#' blocks only to bound memory.
#'
#' **What the number means.**
#' * An athlete who is *not* in the reference field is rated as an extra
#'   entrant joining it: P(beat the third-best of the reference).
#' * An athlete who *is* in the reference field (same `athlete_id` and the same
#'   `ability`) is rated as a member of it: P(finish in the top three of that
#'   field). Each member's own draw is excluded from their bronze line.
#'
#' Probabilities therefore sum to more than 3 across a table that contains the
#' reference field plus others; they are *not* the medal odds of one race.
#'
#' **The reference field is a choice hidden inside the number**, so it is
#' returned as an attribute and should be quoted with it.
#'
#' * `"top8"` (the default; also `"top5"`, `"top12"`, ...; at least 4) takes the best `N`
#'   rows of `ability` by `ability`. It answers "against the field as it stands
#'   in this table", and **moves as the field moves**: a rating computed from a
#'   2019 table and one from a 2026 table are against different fields and are
#'   not comparable. It is the right reading for a single snapshot.
#' * A `data.frame` of entrants (the same columns as `ability`) is a **fixed
#'   reference**. Freeze it once -- say the top 8 of the 2024 final -- and pass
#'   the same table every time, and ratings are comparable across years. The
#'   price is that the reference is arbitrary and ages: as the sport moves on,
#'   everyone drifts towards 100%.
#'
#' Nothing here decides between them; publishing a rating means choosing one
#' and saying which.
#'
#' @param ability A `data.table` from [estimate_ability()] for **one event**:
#'   `athlete_id`, `event_id`, `ability` and `sigma`. Rows with a missing
#'   `ability` or `sigma` get `NA`.
#' @param event_id The canonical event id. Every row of `ability` must be this
#'   event.
#' @param calibration Optional `citius_calibration` from [calibrate()]. Without
#'   one the simulator's registry placeholders apply (see [simulate_event()]).
#' @param reference `"top8"`-style string, or a `data.frame` of entrants; see
#'   Details.
#' @param n_sims Number of simulations of the reference field.
#' @param context Optional `list(meet_tier = , round_class = )`, passed to
#'   [simulate_event()]. Use `list(meet_tier = "M1", round_class = "final")` for
#'   a championship final.
#' @param seed Optional integer seed. The RNG state is restored on exit.
#' @return A numeric vector, one probability per row of `ability` in the same
#'   order. Attributes: `reference` (the reference field's `athlete_id` and
#'   `ability`), `reference_label` (a one-line description) and `n_sims`.
#' @seealso [simulate_event()], [medal_probs()]
#' @examples
#' ab <- data.frame(
#'   athlete_id = letters[1:12],
#'   event_id = "AT-100Metres-M",
#'   ability = -log(seq(9.80, 10.20, length.out = 12)),
#'   sigma = 0.008
#' )
#' rating_medal_chance(ab, "AT-100Metres-M", n_sims = 2000, seed = 1)
#' @export
rating_medal_chance <- function(ability, event_id, calibration = NULL,
                                reference = "top8", n_sims = 5000L,
                                context = NULL, seed = NULL) {
  ab <- data.table::as.data.table(ability)
  req <- c("athlete_id", "event_id", "ability", "sigma")
  missing <- setdiff(req, names(ab))
  if (length(missing)) {
    cli::cli_abort("{.arg ability} is missing required column{?s}: {.field {missing}}.")
  }
  if (!is.character(event_id) || length(event_id) != 1L || is.na(event_id)) {
    cli::cli_abort("{.arg event_id} must be a single event id.")
  }
  if (!all(ab$event_id == event_id, na.rm = TRUE) || anyNA(ab$event_id)) {
    cli::cli_abort(c(
      "Every row of {.arg ability} must be event {.val {event_id}}.",
      i = "Medal chance is defined against one event's field; rate one event at a time."
    ))
  }
  n_sims <- as.integer(n_sims)
  if (length(n_sims) != 1L || is.na(n_sims) || n_sims < 1L) {
    cli::cli_abort("{.arg n_sims} must be a positive integer.")
  }

  out <- rep(NA_real_, nrow(ab))
  ok <- is.finite(ab$ability) & is.finite(ab$sigma)
  ref <- .rating_reference(reference, ab[ok], event_id)
  label <- attr(ref, "label")

  # The simulation sees one table, so the reference and the rated athletes must
  # share a column set. A column carried by one side only (ability_peak is the
  # one that matters: it replaces `ability` in the draw) would otherwise be NA
  # for the other and turn that side's performances into NaN.
  cand <- ab[ok]
  common <- intersect(names(ref), names(cand))
  dropped <- setdiff(union(names(ref), names(cand)), common)
  if (length(dropped)) {
    cli::cli_warn(c(
      "The reference field and {.arg ability} carry different columns; using only the shared ones.",
      i = "Dropped: {.field {dropped}}."
    ))
  }
  ref <- ref[, common, with = FALSE]
  cand <- cand[, common, with = FALSE]
  cand[, athlete_id := as.character(athlete_id)]
  ref[, athlete_id := as.character(athlete_id)]
  nref <- nrow(ref)

  # A rated athlete who IS a reference athlete is scored as a member (own draw
  # excluded from the bronze line); the ability must match as well as the id,
  # because a frozen reference holds an athlete's ability as of a past date.
  j_ref <- match(cand$athlete_id, ref$athlete_id)
  is_member <- !is.na(j_ref) & cand$ability == ref$ability[j_ref]

  # The simulator divides condition sensitivities by the field mean. Pin that to
  # the reference field, or an athlete's number would depend on which other
  # athletes were in their block.
  sens_scale <- mean(.sensitivity_raw(ref, calibration))
  if (!is.finite(sens_scale) || sens_scale <= 0) sens_scale <- 1

  # One seed for every block, so each block meets the same shared shock (and the
  # same reference draws, up to the position of the later random streams).
  if (is.null(seed)) seed <- sample.int(.Machine$integer.max, 1L)

  # Memory bound only: 250 athletes x n_sims draws per matrix. It cannot change
  # what a rating means, and is not a model quantity.
  block <- 250L
  free <- which(!is_member)
  blocks <- if (length(free)) split(free, ceiling(seq_along(free) / block)) else list(integer())
  res <- rep(NA_real_, nrow(cand))

  for (b in seq_along(blocks)) {
    idx <- blocks[[b]]
    field <- data.table::rbindlist(list(ref, cand[idx]), use.names = TRUE)
    sim <- simulate_event(field, n_sims = n_sims, calibration = calibration,
                          context = context, seed = seed,
                          sensitivity_scale = sens_scale)
    perf <- sim$perf
    # simulate_event() drops rows it cannot simulate; positions below assume it
    # dropped none. Fail loudly rather than score the wrong athlete.
    if (ncol(perf) != nrow(field)) {
      cli::cli_abort("Simulation returned {ncol(perf)} athletes for {nrow(field)} entrants.")
    }
    rp <- perf[, seq_len(nref), drop = FALSE]

    if (b == 1L && any(is_member)) {
      m <- which(is_member)
      res[m] <- vapply(j_ref[m], function(j) {
        mean(rowSums(rp[, -j, drop = FALSE] > rp[, j]) < 3L)
      }, numeric(1))
    }
    if (length(idx)) {
      bronze <- .third_best(rp)
      res[idx] <- colMeans(perf[, nref + seq_along(idx), drop = FALSE] > bronze)
    }
  }

  out[ok] <- res
  attr(out, "reference") <- ref[, list(athlete_id, ability)]
  attr(out, "reference_label") <- label
  attr(out, "n_sims") <- n_sims
  out
}


# Resolve `reference` into a table of entrants, with a one-line `label`
# attribute. "topN" takes the N best by `ability` from the supplied table.
.rating_reference <- function(reference, ab, event_id) {
  req <- c("athlete_id", "event_id", "ability", "sigma")
  if (is.character(reference) && length(reference) == 1L &&
      grepl("^top[0-9]+$", reference)) {
    n <- as.integer(sub("^top", "", reference))
    if (nrow(ab) < n) {
      cli::cli_warn("Only {nrow(ab)} athlete{?s} with an ability estimate; {.val {reference}} reference uses all of them.")
      n <- nrow(ab)
    }
    ref <- data.table::copy(ab[order(-ability)][seq_len(n)])
    lab <- sprintf("top %d by ability among the %d rated athletes supplied (moves with the field)",
                   n, nrow(ab))
  } else if (is.data.frame(reference)) {
    ref <- data.table::as.data.table(reference)
    missing <- setdiff(req, names(ref))
    if (length(missing)) {
      cli::cli_abort("{.arg reference} is missing required column{?s}: {.field {missing}}.")
    }
    if (!all(ref$event_id == event_id, na.rm = TRUE)) {
      cli::cli_abort("Every row of {.arg reference} must be event {.val {event_id}}.")
    }
    ref <- ref[is.finite(ability) & is.finite(sigma)]
    lab <- sprintf("fixed reference field of %d supplied entrants", nrow(ref))
  } else {
    cli::cli_abort(c(
      "{.arg reference} must be a string like {.val top8}, or a data frame of entrants.",
      x = "Got {.cls {class(reference)}}."
    ))
  }
  if (nrow(ref) < 4L) {
    cli::cli_abort(c(
      "The reference field needs at least 4 athletes with an ability estimate; it has {nrow(ref)}.",
      i = "With fewer, nearly everyone medals and the rating says nothing."
    ))
  }
  attr(ref, "label") <- lab
  ref
}


# Third-largest value in each row of a numeric matrix. Sorting one long vector
# keeps this loop-free over simulations (see .rank_desc()), and being
# deterministic it leaves a seeded caller's results reproducible. -Inf (no valid
# mark) sorts last, so a row with fewer than three valid marks has bronze -Inf.
.third_best <- function(m) {
  sim <- as.vector(row(m))
  key <- as.vector(m)
  matrix(key[order(sim, -key)], nrow = ncol(m))[3L, ]
}
