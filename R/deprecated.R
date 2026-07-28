#' Deprecated unprefixed athletics functions
#'
#' The feed adapters are now prefixed by the federation they call, so a reader
#' can tell from the name which source a function talks to:
#'
#' | was | now |
#' |-----|-----|
#' | `competition_results()` | [athletics_competition_results()] |
#' | `athlete_results()` | [athletics_athlete_results()] |
#' | `athlete_profile()` | [athletics_athlete_profile()] |
#' | `find_athlete()` | [athletics_find_athlete()] |
#' | `find_competition()` | [athletics_find_competition()] |
#' | `harvest_competitions()` | [athletics_harvest_competitions()] |
#'
#' The swimming adapters were already `aquatics_*`, so `competition_results()`
#' read as though it were sport-agnostic when it only ever called World
#' Athletics. Prefixing by **federation** rather than by sport is deliberate:
#' World Aquatics governs six sports, so `swimming_results()` would be wrong the
#' moment diving is added, while `aquatics_results()` stays correct.
#'
#' These shims warn once per session and will be removed. They exist so a
#' downstream consumer mid-build does not break on the rename.
#'
#' @param ... Passed through to the replacement.
#' @return Whatever the replacement returns.
#' @name citius-deprecated
NULL

.deprecate <- function(old, new) {
  cli::cli_warn(
    c("{.fn {old}} is deprecated; use {.fn {new}}.",
      i = "Feed adapters are now prefixed by federation ({.fn athletics_} / {.fn aquatics_})."),
    .frequency = "once", .frequency_id = paste0("citius_deprecated_", old)
  )
}

#' @rdname citius-deprecated
#' @export
competition_results <- function(...) {
  .deprecate("competition_results", "athletics_competition_results")
  athletics_competition_results(...)
}

#' @rdname citius-deprecated
#' @export
athlete_results <- function(...) {
  .deprecate("athlete_results", "athletics_athlete_results")
  athletics_athlete_results(...)
}

#' @rdname citius-deprecated
#' @export
athlete_profile <- function(...) {
  .deprecate("athlete_profile", "athletics_athlete_profile")
  athletics_athlete_profile(...)
}

#' @rdname citius-deprecated
#' @export
find_athlete <- function(...) {
  .deprecate("find_athlete", "athletics_find_athlete")
  athletics_find_athlete(...)
}

#' @rdname citius-deprecated
#' @export
find_competition <- function(...) {
  .deprecate("find_competition", "athletics_find_competition")
  athletics_find_competition(...)
}

#' @rdname citius-deprecated
#' @export
harvest_competitions <- function(...) {
  .deprecate("harvest_competitions", "athletics_harvest_competitions")
  athletics_harvest_competitions(...)
}
