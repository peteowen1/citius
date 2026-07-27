#' World Aquatics API base URL
#'
#' The official World Aquatics results API. Unlike the athletics feed this is
#' first-party and unauthenticated, with an archive reaching back to the 1896
#' Games and forward to scheduled future events.
#'
#' @return Character scalar base URL.
#' @export
aquatics_base_url <- function() {
  getOption("citius.aquatics_base", "https://api.worldaquatics.com/fina")
}


#' List World Aquatics competitions
#'
#' @param page Zero-indexed page number.
#' @param page_size Results per page (the API caps this internally).
#' @param sort Sort spec passed through to the API, e.g. `"dateFrom,desc"`.
#' @return A `data.table` with `competition_id`, `name`, `official_name`,
#'   `date_from`, `date_to`, `city`, `country`.
#' @examples
#' \dontrun{
#' aquatics_competitions(page_size = 20)
#' }
#' @export
aquatics_competitions <- function(page = 0L, page_size = 50L, sort = "dateFrom,desc") {
  url <- sprintf("%s/competitions?page=%d&pageSize=%d&sort=%s",
                 aquatics_base_url(), as.integer(page), as.integer(page_size),
                 utils::URLencode(sort, reserved = TRUE))
  res <- citius_get_json(url)
  content <- res$content %||% list()
  if (!length(content)) {
    return(data.table::data.table(
      competition_id = integer(), name = character(), official_name = character(),
      date_from = as.Date(character()), date_to = as.Date(character()),
      city = character(), country = character()
    ))
  }

  data.table::rbindlist(lapply(content, function(c_) {
    loc <- c_$location %||% list()
    data.table::data.table(
      competition_id = as.integer(c_$id %||% NA_integer_),
      name           = c_$name %||% NA_character_,
      official_name  = c_$officialName %||% NA_character_,
      date_from      = as_date_safe(c_$dateFrom %||% NA),
      date_to        = as_date_safe(c_$dateTo %||% NA),
      city           = loc$city %||% NA_character_,
      country        = loc$countryCode %||% NA_character_
    )
  }), use.names = TRUE, fill = TRUE)[]
}


#' List the swimming disciplines contested at a World Aquatics competition
#'
#' @param competition_id Integer competition id.
#' @param sport Sport name to filter to. Defaults to `"Swimming"`; the same
#'   competition also carries Diving, Water Polo and others.
#' @return A `data.table` with `discipline_id`, `discipline_name`, `gender`.
#' @examples
#' \dontrun{
#' aquatics_disciplines(5)  # Olympic Games Tokyo 2020
#' }
#' @export
aquatics_disciplines <- function(competition_id, sport = "Swimming") {
  url <- sprintf("%s/competitions/%d/events", aquatics_base_url(), as.integer(competition_id))
  res <- citius_get_json(url)
  sports <- res$Sports %||% list()

  keep <- Filter(function(s) identical(s$Name %||% "", sport), sports)
  if (!length(keep)) {
    return(data.table::data.table(
      discipline_id = character(), discipline_name = character(), gender = character()
    ))
  }

  data.table::rbindlist(lapply(keep[[1]]$DisciplineList %||% list(), function(d) {
    data.table::data.table(
      discipline_id   = d$Id %||% NA_character_,
      discipline_name = d$DisciplineName %||% NA_character_,
      gender          = d$Gender %||% NA_character_
    )
  }), use.names = TRUE, fill = TRUE)[]
}


#' Fetch all results for a World Aquatics discipline
#'
#' Returns every swim across every round — heats, semi-finals and final — as
#' individual rows. Heat-level swims are kept rather than reduced to a personal
#' best because they are the bulk of the evidence about an athlete's typical
#' performance, and discarding them would bias variance estimates downward in
#' exactly the way top-lists do.
#'
#' Summary rows returned by the API (`"Heats Summary"`, `"Semifinals Summary"`)
#' are dropped, since they duplicate individual swims and would double-count.
#'
#' @param discipline_id Character discipline GUID (see [aquatics_disciplines()]).
#' @return A `data.table` in the canonical result schema, plus swimming-specific
#'   `reaction_time`, `lane`, `fina_points` and `split_50`.
#' @examples
#' \dontrun{
#' d <- aquatics_disciplines(5)
#' aquatics_results(d$discipline_id[1])
#' }
#' @export
aquatics_results <- function(discipline_id) {
  url <- sprintf("%s/events/%s", aquatics_base_url(), discipline_id)
  res <- citius_get_json(url)
  if (is.null(res)) return(.empty_result_dt())

  heats <- res$Heats %||% list()
  heats <- Filter(function(h) !isTRUE(h$IsSummary) &&
                    !grepl("Summary", h$Name %||% "", fixed = TRUE), heats)
  if (!length(heats)) return(.empty_result_dt())

  discipline_name <- res$DisciplineName %||% NA_character_
  gender <- res$Gender %||% NA_character_

  rows <- lapply(heats, function(h) {
    results <- h$Results %||% list()
    if (!length(results)) return(NULL)
    data.table::rbindlist(lapply(results, function(x) {
      splits <- x$Splits %||% list()
      data.table::data.table(
        athlete_id    = as.character(x$PersonId %||% NA_character_),
        athlete_name  = x$FullName %||% NA_character_,
        date          = as_date_safe(h$Date %||% NA),
        sport         = "Swimming",
        discipline    = discipline_name,
        round         = h$PhaseName %||% h$Name %||% NA_character_,
        heat_name     = h$Name %||% NA_character_,
        mark_string   = x$Time %||% NA_character_,
        place         = suppressWarnings(as.integer(x$Rank %||% NA_integer_)),
        heat_place    = suppressWarnings(as.integer(x$HeatRank %||% NA_integer_)),
        country       = x$NAT %||% NA_character_,
        lane          = suppressWarnings(as.integer(x$Lane %||% NA_integer_)),
        reaction_time = suppressWarnings(as.numeric(x$RT %||% NA_real_)),
        fina_points   = suppressWarnings(as.numeric(x$Points %||% NA_real_)),
        age_at_result = suppressWarnings(as.numeric(x$AthleteResultAge %||% NA_real_)),
        medal         = x$MedalTag %||% NA_character_,
        split_50      = if (length(splits)) parse_mark(splits[[1]]$Time %||% NA) else NA_real_
      )
    }), use.names = TRUE, fill = TRUE)
  })

  dt <- data.table::rbindlist(Filter(Negate(is.null), rows), use.names = TRUE, fill = TRUE)
  if (!nrow(dt)) return(.empty_result_dt())

  dt[, mark := parse_mark(mark_string)]
  dt[, indoor := NA]
  dt[, wind := NA_real_]
  dt[, legal := TRUE]

  sex <- ifelse(grepl("^W|female", gender, ignore.case = TRUE), "W", "M")
  .finalise_results(dt, sex = sex)
}
