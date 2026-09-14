#' Per-position finishing probabilities for a simulated field
#'
#' [medal_probs()] reduces `sim$rank` to three summaries -- `p_gold`,
#' `p_medal`, `p_top8` -- and the rest of the distribution is discarded with
#' the simulation object. This keeps the whole top-`k` instead, one column per
#' finishing position, so "what were his odds of finishing 4th" is answerable
#' later without re-simulating.
#'
#' **Wide, capped at `k`, deliberately.** A long form (one row per athlete per
#' position) scales with field size SQUARED per race: measured on the corpus
#' 2026-09-14, the largest single race is an 816-starter marathon, which is 816
#' rows wide against **665,856** long. Across every 2026 race it is 1,032,553
#' rows wide against 13,016,281 long. Capping at `k` makes the row count a
#' fixed one-per-athlete-per-race whatever the field size, which is the whole
#' point -- field sizes run from a median of 4 to a maximum of 944.
#'
#' `k = 8` is the domain's own boundary rather than an arbitrary one: athletics
#' scores points to eighth place. Nothing is lost by the cap, because the
#' probability of finishing `k`-th or worse is `1 - sum(p_pos_1 .. p_pos_k)`.
#'
#' @param sim A `citius_sim` from [simulate_event()].
#' @param k Highest finishing position to report. Positions beyond the field
#'   size are still emitted, as zeros, so every race in a store has the same
#'   columns and they can be bound without a fill rule.
#' @return A `data.table` with `athlete_id` and `p_pos_1` .. `p_pos_k`, ordered
#'   by `p_pos_1` descending. Rows sum to at most 1; the shortfall is the
#'   probability of finishing worse than `k`.
#' @seealso [medal_probs()], which this complements rather than replaces.
#' @examples
#' \dontrun{
#' sim <- simulate_event(proj, n_sims = 20000)
#' position_probs(sim)          # p_pos_1 .. p_pos_8
#' }
#' @export
position_probs <- function(sim, k = 8L) {
  stopifnot(inherits(sim, "citius_sim"))
  k <- as.integer(k)
  if (!is.finite(k) || k < 1L) cli::cli_abort("{.arg k} must be a positive integer.")
  r <- sim$rank

  # colMeans over a logical matrix per position: k passes over the simulation
  # matrix rather than one tabulate() per athlete, for the same reason
  # .rank_desc() is vectorised -- never loop at R level over the big dimension.
  out <- data.table::data.table(athlete_id = colnames(r))
  for (i in seq_len(k)) {
    data.table::set(out, j = paste0("p_pos_", i), value = colMeans(r == i))
  }
  data.table::setorderv(out, "p_pos_1", -1L)
  out[]
}
