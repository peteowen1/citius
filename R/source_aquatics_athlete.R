#' Complete swim career for one athlete
#'
#' The competition endpoint serves only World Aquatics' own sanctioned events —
#' probing ten national championships returned results for **none** of them. It
#' therefore cannot see a swimmer who has never made a global final, which was
#' 37% of the Glasgow finalists.
#'
#' This endpoint can. For Hosszú Katinka it returns 1,893 swims across 160
#' meets, against 1,191 across 28 from the competition route, and the extra 132
#' meets are exactly the missing category: *HUN Championships 2024*, *Hungarian
#' National Championships (50m)*, *Sette Colli*, *Mare Nostrum*, *CANA Africa
#' Junior*.
#'
#' Same lesson as the athletics feed, where the athlete endpoint took results per
#' athlete-event from a median of 1 to 7: **the meet-discovery route and the
#' athlete route see different data, and the athlete route sees more.**
#'
#' @section Sex must be supplied:
#' The response carries no sex, and `match_event()` needs it — `"Women's 200m
#' Medley"` is unambiguous but `"200m Freestyle"` alone is not. Pass the sex you
#' already hold; without it `event_id` is `NA` and the swim is unusable.
#'
#' @param athlete_id World Aquatics athlete id (a UUID).
#' @param sex `"M"`, `"W"` or `NULL`. Supply it wherever known.
#' @return A `data.table` in the canonical result schema.
#' @examples
#' \dontrun{
#' aquatics_athlete_results("e4b790ab-e1a6-465b-91be-87dea82cf9da", sex = "W")
#' }
#' @export
aquatics_athlete_results <- function(athlete_id, sex = NULL) {
  url <- sprintf("%s/athletes/%s/results", aquatics_base_url(), as.character(athlete_id))
  # Fetch errors PROPAGATE, exactly like athletics_athlete_results(). A first
  # fix warned and returned an empty table instead -- which looked louder but
  # kept the real bug: every harvester wraps this call in its own
  # tryCatch(error = ...) precisely to distinguish "fetch failed, retry next
  # run" from "genuinely empty, cache it", and a function that converts errors
  # to empty tables internally makes that distinction impossible at the only
  # place it matters. A definitive 404 still returns NULL (a genuine
  # "no results" answer), so the empty table below remains cacheable fact.
  r <- citius_get_json(url)
  if (is.null(r) || is.null(r$Results) || !length(r$Results)) return(.empty_result_dt())
  res <- r$Results
  nm <- r$FullName %||% NA_character_

  dt <- data.table::rbindlist(lapply(res, function(x) {
    data.table::data.table(
      athlete_id   = as.character(athlete_id),
      athlete_name = nm,
      date         = as_date_safe(x$Date %||% NA),
      sport        = "Swimming",
      discipline   = x$DisciplineName %||% NA_character_,
      round        = x$PhaseName %||% NA_character_,
      mark_string  = x$Time %||% NA_character_,
      place        = suppressWarnings(as.integer(x$Rank %||% NA_integer_)),
      # CompetitionType is "International", "National", "Continental"... which is
      # exactly the tier signal the model wants, and is not available from the
      # competition route at all.
      comp_name    = x$CompetitionName %||% NA_character_,
      comp_type    = x$CompetitionType %||% NA_character_,
      venue_city   = x$CompetitionCity %||% NA_character_,
      venue_country = x$CompetitionCountry %||% NA_character_,
      country      = x$NAT %||% NA_character_,
      club         = x$ClubName %||% NA_character_,
      age          = suppressWarnings(as.numeric(x$AthleteResultAge %||% NA_real_)),
      result_score = suppressWarnings(as.numeric(x$Points %||% NA_real_)),
      medal        = x$MedalTag %||% NA_character_,
      record_type  = x$RecordType %||% NA_character_,
      sex          = sex %||% NA_character_
    )
  }), use.names = TRUE, fill = TRUE)
  if (!nrow(dt)) return(.empty_result_dt())

  dt[, mark := parse_mark(mark_string)]
  dt[, event_id := match_event(discipline, sex)]
  reg <- .citius_event_registry[, c("event_id", "orientation")]
  dt <- merge(dt, reg, by = "event_id", all.x = TRUE, sort = FALSE)
  # NA orientation must stay NA. Defaulting an unmatched event to -1L
  # (time-event) silently produced a WRONG-SIGNED perf for unmatched FIELD
  # events, undoing the guarantee match_event() exists to give.
  dt[, perf := to_perf(mark, orientation)]
  # No competition_id is returned, so the race key is built from what uniquely
  # identifies a race here: meet, event, phase and date.
  dt[, race_key := paste(comp_name, discipline, round, date, sep = "|")]
  dt[]
}
