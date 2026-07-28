#' Full finishing-position distribution
#'
#' [medal_probs()] collapses the simulation to gold and medal probability, which
#' throws away most of what was simulated. The rank matrix already holds the
#' whole distribution: how often each athlete finished 1st, 2nd, 4th, last.
#'
#' Fourth place is the obvious use — the cruellest position in the sport and one
#' no medal-probability summary can express — but the distribution also carries
#' the shape of a forecast. Two athletes can share a 15% medal chance while one
#' is a likely 4th with upside and the other is a likely 8th who occasionally
#' produces something extraordinary. Those are different athletes to back.
#'
#' @param sim A `citius_sim` from [simulate_event()].
#' @param max_position Highest position to report individually. Positions beyond
#'   it are pooled into one `NA` row per athlete so the probabilities still sum
#'   to 1.
#' @param wide Return one column per position rather than long format.
#' @return A `data.table`. Long (default): `athlete_id`, `position`, `prob`,
#'   `cum_prob`. Wide: `athlete_id` then `pos_1`, `pos_2`, ...
#' @examples
#' \dontrun{
#' sim <- simulate_event(entrants, n_sims = 10000)
#' position_probs(sim)
#' position_probs(sim, wide = TRUE)
#' }
#' @export
position_probs <- function(sim, max_position = 8L, wide = FALSE) {
  if (!inherits(sim, "citius_sim")) {
    cli::cli_abort("{.arg sim} must be a {.cls citius_sim} from {.fn simulate_event}.")
  }
  rank <- sim$rank
  n_sims <- nrow(rank)
  ath <- colnames(rank)
  max_position <- min(as.integer(max_position), ncol(rank))

  out <- vector("list", length(ath))
  for (i in seq_along(ath)) {
    r <- rank[, i]
    tab <- tabulate(r, nbins = ncol(rank)) / n_sims
    keep <- tab[seq_len(max_position)]
    beyond <- sum(tab) - sum(keep)
    out[[i]] <- data.table::data.table(
      athlete_id = ath[i],
      position = c(seq_len(max_position), if (beyond > 1e-12) NA_integer_),
      prob = c(keep, if (beyond > 1e-12) beyond))
  }
  res <- data.table::rbindlist(out)
  # Cumulative over the reported positions only; the pooled NA row is excluded
  # because "top NA" is not a question anyone asks.
  res[, cum_prob := cumsum(data.table::fifelse(is.na(position), 0, prob)),
      by = athlete_id]
  res[is.na(position), cum_prob := NA_real_]

  if (!wide) {
    data.table::setorder(res, athlete_id, position, na.last = TRUE)
    return(res[])
  }
  w <- data.table::dcast(res[!is.na(position)], athlete_id ~ position,
                         value.var = "prob", fill = 0)
  data.table::setnames(w, setdiff(names(w), "athlete_id"),
                       paste0("pos_", setdiff(names(w), "athlete_id")))
  w[]
}
