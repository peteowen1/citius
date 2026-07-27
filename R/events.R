#' Canonical event registry
#'
#' The single source of truth for what an event *is*: its units, which
#' direction counts as better, and the structural facts that change how it must
#' be modelled. Every source-specific event name is normalised onto this
#' registry by [match_event()], so adding a new data feed never requires
#' touching the models.
#'
#' Four columns carry real modelling weight and are worth understanding:
#'
#' \describe{
#'   \item{`orientation`}{`-1` where lower is better (all times), `+1` where
#'     higher is better (jumps, throws, points). Consumed by [to_perf()].}
#'   \item{`tactical`}{`TRUE` where championship finals are routinely run as
#'     sit-and-kick races, so the winning *time* is uninformative about ability.
#'     Variance estimated from these races is inflated and must be handled
#'     separately — see [estimate_variance()]. Empirically this is severe:
#'     an elite 1500m runner's raw career spread is roughly 4.5 percent, but
#'     most of that is tactics, not form.}
#'   \item{`technical`}{`TRUE` for events with a discrete failure mode (fouls,
#'     no-height) that is *not* a draw from the continuous performance
#'     distribution and must be simulated as a separate Bernoulli process.}
#'   \item{`cv_prior`}{Prior on within-athlete coefficient of variation, on the
#'     log scale. Used to shrink noisy per-athlete variance estimates. Swimming
#'     values are markedly lower than athletics because the pool removes wind
#'     and, largely, tactics.}
#' }
#'
#' @return A `data.table` with one row per canonical event.
#' @examples
#' ev <- citius_events()
#' ev[ev$sport == "Swimming" & ev$sex == "W", c("event_id", "cv_prior")]
#' @export
citius_events <- function() {
  copy_dt(.citius_event_registry)
}

# --- athletics ---------------------------------------------------------------

.athletics_defs <- function() {
  d <- function(discipline, family, unit, orientation, tactical, technical, cv_prior,
                sexes = c("M", "W")) {
    data.table::data.table(
      sport = "Athletics", discipline = discipline, family = family,
      unit = unit, orientation = orientation, tactical = tactical,
      technical = technical, cv_prior = cv_prior, sex = sexes
    )
  }

  data.table::rbindlist(list(
    d("100 Metres",              "sprint",   "seconds", -1L, FALSE, FALSE, 0.008),
    d("200 Metres",              "sprint",   "seconds", -1L, FALSE, FALSE, 0.009),
    d("400 Metres",              "sprint",   "seconds", -1L, FALSE, FALSE, 0.010),
    d("800 Metres",              "middle",   "seconds", -1L, TRUE,  FALSE, 0.013),
    d("1500 Metres",             "middle",   "seconds", -1L, TRUE,  FALSE, 0.016),
    d("5000 Metres",             "distance", "seconds", -1L, TRUE,  FALSE, 0.020),
    d("10,000 Metres",           "distance", "seconds", -1L, TRUE,  FALSE, 0.022),
    d("Marathon",                "road",     "seconds", -1L, FALSE, FALSE, 0.030),
    d("110 Metres Hurdles",      "hurdles",  "seconds", -1L, FALSE, FALSE, 0.013, "M"),
    d("100 Metres Hurdles",      "hurdles",  "seconds", -1L, FALSE, FALSE, 0.013, "W"),
    d("400 Metres Hurdles",      "hurdles",  "seconds", -1L, FALSE, FALSE, 0.013),
    d("3000 Metres Steeplechase","distance", "seconds", -1L, TRUE,  FALSE, 0.020),
    d("20 Kilometres Race Walk", "walk",     "seconds", -1L, FALSE, FALSE, 0.025),
    d("High Jump",               "jump",     "metres",   1L, FALSE, TRUE,  0.022),
    d("Pole Vault",              "jump",     "metres",   1L, FALSE, TRUE,  0.030),
    d("Long Jump",               "jump",     "metres",   1L, FALSE, TRUE,  0.025),
    d("Triple Jump",             "jump",     "metres",   1L, FALSE, TRUE,  0.022),
    d("Shot Put",                "throw",    "metres",   1L, FALSE, TRUE,  0.030),
    d("Discus Throw",            "throw",    "metres",   1L, FALSE, TRUE,  0.040),
    d("Hammer Throw",            "throw",    "metres",   1L, FALSE, TRUE,  0.035),
    d("Javelin Throw",           "throw",    "metres",   1L, FALSE, TRUE,  0.045),
    d("Decathlon",               "combined", "points",   1L, FALSE, FALSE, 0.030, "M"),
    d("Heptathlon",              "combined", "points",   1L, FALSE, FALSE, 0.030, "W")
  ))
}

# --- swimming ----------------------------------------------------------------

.swimming_defs <- function() {
  d <- function(discipline, family, cv_prior, sexes = c("M", "W")) {
    data.table::data.table(
      sport = "Swimming", discipline = discipline, family = family,
      unit = "seconds", orientation = -1L, tactical = FALSE,
      technical = FALSE, cv_prior = cv_prior, sex = sexes
    )
  }

  data.table::rbindlist(list(
    d("50m Freestyle",     "swim_sprint",   0.007),
    d("100m Freestyle",    "swim_sprint",   0.007),
    d("200m Freestyle",    "swim_middle",   0.008),
    d("400m Freestyle",    "swim_middle",   0.009),
    d("800m Freestyle",    "swim_distance", 0.011),
    d("1500m Freestyle",   "swim_distance", 0.012),
    # 50m of each stroke is contested at the Commonwealth Games and World
    # Championships but not at the Olympics. The registry covers the union of
    # major-Games programmes, not the Olympic subset.
    d("50m Backstroke",    "swim_sprint",   0.008),
    d("100m Backstroke",   "swim_sprint",   0.008),
    d("200m Backstroke",   "swim_middle",   0.009),
    d("50m Breaststroke",  "swim_sprint",   0.009),
    d("100m Breaststroke", "swim_sprint",   0.009),
    d("200m Breaststroke", "swim_middle",   0.010),
    d("50m Butterfly",     "swim_sprint",   0.008),
    d("100m Butterfly",    "swim_sprint",   0.008),
    d("200m Butterfly",    "swim_middle",   0.010),
    d("200m Individual Medley", "swim_im",  0.009),
    d("400m Individual Medley", "swim_im",  0.010)
  ))
}

.build_event_registry <- function() {
  ev <- data.table::rbindlist(list(.athletics_defs(), .swimming_defs()),
                              use.names = TRUE)
  ev[, event_id := paste0(
    ifelse(sport == "Swimming", "SW", "AT"), "-",
    gsub("[^A-Za-z0-9]+", "", discipline), "-", sex
  )]
  data.table::setcolorder(ev, c("event_id", "sport", "discipline", "sex", "family"))
  data.table::setkeyv(ev, "event_id")
  ev[]
}

.citius_event_registry <- .build_event_registry()


#' Match source event names onto the canonical registry
#'
#' Data feeds spell events inconsistently (`"10000 Metres"` vs `"10,000
#' Metres"`, `"Men's 100m Freestyle"` vs `"100m Freestyle"`). This resolves a
#' source string plus a sex code to a canonical `event_id`.
#'
#' Unmatched names return `NA` rather than guessing. Silently mapping an
#' unrecognised event onto a plausible-looking neighbour would corrupt an
#' athlete's history in a way that is very hard to detect downstream.
#'
#' @param discipline Character vector of source event names.
#' @param sex Character vector of sex codes (`"M"`/`"W"`, or World Aquatics
#'   style `"Men"`/`"Women"`).
#' @return Character vector of `event_id`s, `NA` where unmatched.
#' @examples
#' match_event(c("100 Metres", "Men's 100m Freestyle"), c("M", "M"))
#' @export
match_event <- function(discipline, sex) {
  n <- max(length(discipline), length(sex))
  discipline <- rep_len(as.character(discipline), n)
  sex <- rep_len(as.character(sex), n)

  sex_norm <- toupper(substr(trimws(sex), 1, 1))
  sex_norm[sex_norm == "F"] <- "W"

  key <- .normalise_discipline(discipline)
  reg <- .citius_event_registry
  lookup <- stats::setNames(reg$event_id, paste0(.normalise_discipline(reg$discipline), "|", reg$sex))

  unname(lookup[paste0(key, "|", sex_norm)])
}

.normalise_discipline <- function(x) {
  x <- tolower(trimws(as.character(x)))
  # Drop leading gender qualifiers used by World Aquatics ("Men's 100m Freestyle")
  x <- sub("^(men|women|man|woman)('s)?\\s+", "", x)
  x <- gsub("[,'’]", "", x)
  x <- gsub("\\bmetres\\b|\\bmeters\\b|\\bmetre\\b|\\bmeter\\b", "m", x)
  x <- gsub("\\bkilometres\\b|\\bkilometers\\b", "km", x)
  x <- gsub("\\bindividual medley\\b", "im", x)
  x <- gsub("\\bsteeplechase\\b", "sc", x)
  x <- gsub("\\brace walk\\b|\\bracewalk\\b", "walk", x)
  x <- gsub("[^a-z0-9]+", "", x)
  x
}
