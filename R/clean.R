#' Resolve duplicate and impossible records
#'
#' Complements [flag_implausible()], which handles marks. This handles the two
#' other defects a validation sweep over the harvest turned up, neither of which
#' any model checked for.
#'
#' **An athlete appearing twice in one race** (1,114 cases). Typically a field
#' event where the feed lists more than one attempt, one carrying the official
#' placing and one no placing at all. Every model assumes one performance per
#' athlete per race — `decompose_races()` fits a single athlete effect per race,
#' so a duplicate silently double-weights that athlete in the race effect. The
#' best mark is kept, since that is what the placing reflects.
#'
#' **Impossible ages** (3 cases, including a 5000m runner born in 1884). A bad
#' birthdate corrupts the aging curve, but the performance itself is fine, so
#' `age` and `birthdate` are cleared rather than the row dropped. Discarding the
#' mark would lose real data to fix a metadata error.
#'
#' Both are reported rather than done quietly: a cleaning step that silently
#' changes row counts is how a harvest bug hides.
#'
#' @param results Canonical results.
#' @param age_range Plausible competitor ages. Generous by design — this catches
#'   corrupt birthdates, not unusual athletes.
#' @return The input with duplicates collapsed and impossible ages cleared.
#' @examples
#' \dontrun{
#' clean <- clean_results(flag_implausible(champs))
#' }
#' @export
clean_results <- function(results, age_range = c(10, 70)) {
  dt <- data.table::copy(if (data.table::is.data.table(results)) results
                         else data.table::as.data.table(results))
  if (!nrow(dt)) return(dt[])

  if ("age" %in% names(dt)) {
    bad <- !is.na(dt$age) & (dt$age < age_range[1] | dt$age > age_range[2])
    if (any(bad)) {
      cli::cli_alert_info(
        "Cleared {sum(bad)} impossible age{?s} (outside {age_range[1]}-{age_range[2]}); marks kept."
      )
      dt[bad, age := NA_real_]
      if ("birthdate" %in% names(dt)) dt[bad, birthdate := as.Date(NA)]
    }
  }

  if (all(c("race_key", "athlete_id") %in% names(dt))) {
    n0 <- nrow(dt)
    # Order so the best mark leads within each athlete-race, then keep the
    # first. `perf` is oriented so higher is always better, and NA marks
    # (no-marks) sort last -- they must be kept when they are all an athlete
    # has, because no-mark rates are themselves a measured quantity.
    data.table::setorderv(dt, c("race_key", "athlete_id", "perf"),
                          c(1L, 1L, -1L), na.last = TRUE)
    dt <- unique(dt, by = c("race_key", "athlete_id"))
    if (nrow(dt) < n0) {
      cli::cli_alert_info(
        "Collapsed {n0 - nrow(dt)} duplicate athlete-race record{?s} to the best mark."
      )
    }
  }
  dt[]
}
