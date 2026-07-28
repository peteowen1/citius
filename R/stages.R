#' Medal probabilities from each vantage point in a championship
#'
#' A single medal probability answers "who wins?" without saying *when it was
#' asked*. The honest answer changes as the meet progresses, and the change is
#' itself the interesting part: an athlete at 30% before the heats who is 55%
#' after them has told you something, and one who is eliminated has told you
#' more.
#'
#' Three vantage points, each answering a different question:
#'
#' \describe{
#'   \item{`entry`}{Everyone entered. "Who wins if they all turn up?" —
#'     including qualification uncertainty for every round.}
#'   \item{`contested`}{Everyone who actually started a round. Removes
#'     withdrawals, which are a field error rather than a model error.}
#'   \item{`final`}{The known finalists. Pure head-to-head, no qualification
#'     risk left.}
#' }
#'
#' Comparing `entry` with `contested` isolates how much a forecast was damaged
#' by withdrawals; comparing `contested` with `final` isolates how much was
#' decided by qualification rather than by the final itself.
#'
#' @param ability Ability estimates covering every athlete in `fields`.
#' @param fields Named list of athlete_id vectors, one per stage, in order.
#' @param structure Round structure for stages where qualification is still
#'   pending, as taken by [simulate_rounds()]. `NULL` means a single race.
#' @param n_sims Simulations per stage.
#' @param calibration Optional `citius_calibration`.
#' @param seed Optional integer seed.
#' @return A `data.table` with one row per athlete per stage: `stage`,
#'   `athlete_id`, `p_gold`, `p_medal`, `p_final`, plus `delta_gold` against the
#'   previous stage.
#' @examples
#' \dontrun{
#' stage_probs(ability,
#'             fields = list(entry = entered, contested = started, final = finalists),
#'             structure = list(list(races = 3, advance = 2, fastest_losers = 2),
#'                              list(races = 1)))
#' }
#' @export
stage_probs <- function(ability, fields, structure = NULL, n_sims = 10000L,
                        calibration = NULL, seed = NULL) {
  ab <- data.table::as.data.table(ability)
  if (!length(fields)) cli::cli_abort("{.arg fields} is empty.")
  if (is.null(names(fields))) names(fields) <- paste0("stage", seq_along(fields))

  out <- vector("list", length(fields))
  for (i in seq_along(fields)) {
    ent <- ab[athlete_id %in% as.character(fields[[i]])]
    if (nrow(ent) < 2L) next
    # The final stage has no qualification left, so it is a single race whatever
    # `structure` says. Using the staged simulator there would invent a round
    # that has already happened.
    is_last <- i == length(fields)
    r <- if (is_last || is.null(structure)) {
      mp <- medal_probs(simulate_event(ent, n_sims = n_sims,
                                       calibration = calibration, seed = seed))
      mp[, .(athlete_id, p_gold, p_medal, p_final = 1)]
    } else {
      simulate_rounds(ent, structure = structure, n_sims = n_sims,
                      calibration = calibration, seed = seed)[
                        , .(athlete_id, p_gold, p_medal, p_final)]
    }
    r[, `:=`(stage = names(fields)[i], stage_index = i, field_size = nrow(ent))]
    out[[i]] <- r
  }
  res <- data.table::rbindlist(Filter(Negate(is.null), out), fill = TRUE)
  if (!nrow(res)) return(res)

  data.table::setorder(res, athlete_id, stage_index)
  # Eliminated athletes simply stop appearing, so a missing later stage is
  # informative rather than an error; delta is NA for their first stage only.
  res[, delta_gold := p_gold - data.table::shift(p_gold), by = athlete_id]
  data.table::setcolorder(res, c("stage", "stage_index", "athlete_id", "field_size",
                                 "p_gold", "p_medal", "p_final", "delta_gold"))
  res[]
}
