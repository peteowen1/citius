#' World Athletics API base URL
#'
#' A community-maintained REST wrapper over World Athletics' unofficial GraphQL
#' endpoint. It is used in preference to hitting GraphQL directly because the
#' upstream API key rotates; the wrapper absorbs that churn and additionally
#' normalises marks to integer units.
#'
#' Override with `options(citius.athletics_base = "...")` if the host moves or
#' you stand up your own mirror. Treating this as configuration rather than a
#' constant is deliberate: World Athletics put their results services out to
#' tender from January 2026, so the plumbing is expected to shift.
#'
#' @return Character scalar base URL.
#' @export
athletics_base_url <- function() {
  getOption("citius.athletics_base", "https://worldathletics.nimarion.de")
}


#' Search World Athletics for an athlete
#'
#' @param name Athlete name, full or partial.
#' @return A `data.table` of candidate athletes ordered by match quality, with
#'   columns `athlete_id`, `first_name`, `last_name`, `country`, `sex`,
#'   `birthdate`, `distance` (Levenshtein). Zero rows if nothing matched.
#' @examples
#' \dontrun{
#' find_athlete("Ingebrigtsen")
#' }
#' @export
find_athlete <- function(name) {
  url <- paste0(athletics_base_url(), "/athletes/search?name=", utils::URLencode(name, reserved = TRUE))
  res <- citius_get_json(url)
  if (is.null(res) || !length(res)) return(.empty_athlete_dt())

  dt <- data.table::rbindlist(lapply(res, function(a) {
    data.table::data.table(
      athlete_id = as.integer(a$id %||% NA_integer_),
      first_name = a$firstname %||% NA_character_,
      last_name  = a$lastname %||% NA_character_,
      country    = a$country %||% NA_character_,
      sex        = a$sex %||% NA_character_,
      birthdate  = as_date_safe(a$birthdate %||% NA),
      distance   = as.integer(a$levenshteinDistance %||% NA_integer_)
    )
  }), use.names = TRUE, fill = TRUE)

  data.table::setorder(dt, distance)
  dt[]
}

.empty_athlete_dt <- function() {
  data.table::data.table(
    athlete_id = integer(), first_name = character(), last_name = character(),
    country = character(), sex = character(), birthdate = as.Date(character()),
    distance = integer()
  )
}


#' Resolve a mark, preferring the displayed string over the numeric field
#'
#' World Athletics exposes each result twice: `mark` as a display string and
#' `performanceValue` as an integer in centimetres or milliseconds. The integer
#' is **not reliable** — round marks lose their trailing zeros upstream, so a
#' `"6.00"` metre vault arrives as `performanceValue = 6` rather than `600` and
#' converts to six centimetres. Roughly 6 percent of one elite vaulter's career
#' is affected.
#'
#' The display string is therefore treated as authoritative. It parses
#' unambiguously for every event type, because the unit is a property of the
#' event rather than of the string: `"3:37.84"` is seconds, `"6.00"` is metres,
#' `"9058"` is points. `performanceValue` is kept only as a fallback for the
#' rare rows with no string.
#'
#' This ordering matters more than it looks. The corrupted values are extreme
#' enough that a robust outlier filter will quietly discard them, leaving output
#' that looks clean while real performances have gone missing.
#'
#' @param mark_string Display marks.
#' @param value_raw `performanceValue` integers.
#' @param is_technical Whether the event is a field event (centimetres) rather
#'   than a track event (milliseconds).
#' @return Numeric marks in seconds, metres or points.
#' @keywords internal
#' @noRd
.resolve_mark <- function(mark_string, value_raw, is_technical) {
  mark <- parse_mark(mark_string)
  fallback <- data.table::fifelse(is_technical, value_raw / 100, value_raw / 1000)
  data.table::fifelse(is.na(mark), fallback, mark)
}


#' Fetch a World Athletics athlete profile
#'
#' @param athlete_id Integer World Athletics athlete id.
#' @return A one-row `data.table` with `athlete_id`, `first_name`, `last_name`,
#'   `country`, `sex` and `birthdate`, or zero rows if not found.
#' @examples
#' \dontrun{
#' athlete_profile(14536762)
#' }
#' @export
athlete_profile <- function(athlete_id) {
  url <- paste0(athletics_base_url(), "/athletes/", as.integer(athlete_id))
  a <- citius_get_json(url)
  if (is.null(a)) return(.empty_athlete_dt()[0])

  data.table::data.table(
    athlete_id = as.integer(a$id %||% athlete_id),
    first_name = a$firstname %||% NA_character_,
    last_name  = a$lastname %||% NA_character_,
    country    = a$country %||% NA_character_,
    sex        = a$sex %||% NA_character_,
    birthdate  = as_date_safe(a$birthdate %||% NA)
  )[]
}


#' Fetch an athlete's full World Athletics result history
#'
#' Returns every recorded performance, not a top-list. This distinction is
#' critical: ability can be estimated from bests, but *variance* cannot. A
#' top-list is truncated at the good end, so fitting spread to one would badly
#' understate how often an athlete has an ordinary day.
#'
#' The results feed itself carries no sex or date-of-birth, both of which are
#' needed to resolve canonical events and to place a performance on an aging
#' curve. They are looked up from the athlete profile unless supplied.
#'
#' @param athlete_id Integer World Athletics athlete id (see [find_athlete()]).
#' @param sex Optional `"M"`/`"W"`. Looked up from the profile when `NULL`.
#' @param birthdate Optional `Date`. Looked up from the profile when `NULL`;
#'   used to compute `age` at each performance.
#' @return A `data.table` in the canonical result schema — one row per
#'   performance with `athlete_id`, `date`, `event_id`, `mark`, `perf`, `place`,
#'   `round`, `wind`, `indoor`, `legal`, `tier`, `age`, `venue_country`,
#'   `result_score`. Zero rows if the athlete has no results.
#' @examples
#' \dontrun{
#' athlete_results(14653717)
#' }
#' @export
athlete_results <- function(athlete_id, sex = NULL, birthdate = NULL) {
  if (is.null(sex) || is.null(birthdate)) {
    prof <- athlete_profile(athlete_id)
    if (nrow(prof)) {
      sex <- sex %||% prof$sex
      birthdate <- birthdate %||% prof$birthdate
    }
  }

  url <- paste0(athletics_base_url(), "/athletes/", as.integer(athlete_id), "/results")
  res <- citius_get_json(url)
  if (is.null(res) || !length(res)) return(.empty_result_dt())

  dt <- data.table::rbindlist(lapply(res, function(r) {
    loc <- r$location %||% list()
    data.table::data.table(
      athlete_id    = as.integer(athlete_id),
      date          = as_date_safe(r$date %||% NA),
      sport         = "Athletics",
      discipline    = r$discipline %||% NA_character_,
      competition   = r$competition %||% NA_character_,
      competition_id = as.integer(r$competitionId %||% NA_integer_),
      # performanceValue is milliseconds for track, centimetres for field
      value_raw     = as.numeric(r$performanceValue %||% NA_real_),
      mark_string   = r$mark %||% NA_character_,
      is_technical  = isTRUE(r$isTechnical),
      place         = suppressWarnings(as.integer(r$place %||% NA_integer_)),
      round         = r$race %||% NA_character_,
      wind          = as.numeric(r$wind %||% NA_real_),
      indoor        = isTRUE(loc$indoor),
      legal         = isTRUE(r$legal),
      tier          = r$category %||% NA_character_,
      venue_country = loc$country %||% NA_character_,
      venue_city    = loc$city %||% NA_character_,
      venue_stadium = loc$stadium %||% NA_character_,
      result_score  = as.numeric(r$resultScore %||% NA_real_)
    )
  }), use.names = TRUE, fill = TRUE)

  dt[, mark := .resolve_mark(mark_string, value_raw, is_technical)]

  bd <- if (length(birthdate)) as.Date(birthdate)[1] else as.Date(NA)
  dt[, age := as.numeric(date - bd) / 365.25]

  .finalise_results(dt, sex = if (length(sex)) sex[1] else NA_character_)
}

.empty_result_dt <- function() {
  data.table::data.table(
    athlete_id = integer(), date = as.Date(character()), sport = character(),
    discipline = character(), event_id = character(), mark = numeric(),
    perf = numeric(), place = integer(), round = character(), wind = numeric(),
    indoor = logical(), legal = logical(), tier = character(), age = numeric(),
    venue_country = character(), result_score = numeric(), sex = character()
  )
}

#' Attach canonical event ids and the oriented performance scale
#' @keywords internal
#' @noRd
.finalise_results <- function(dt, sex = NULL) {
  if (is.null(sex)) sex <- dt$sex %||% NA_character_
  dt[, sex := rep_len(as.character(sex), .N)]
  dt[, event_id := match_event(discipline, sex)]

  reg <- .citius_event_registry[, c("event_id", "orientation")]
  dt <- merge(dt, reg, by = "event_id", all.x = TRUE, sort = FALSE)
  dt[, perf := to_perf(mark, data.table::fifelse(is.na(orientation), -1L, orientation))]
  dt[]
}


#' Fetch every result from a World Athletics competition
#'
#' Returns results grouped by race, which is the unit calibration needs. An
#' athlete-by-athlete harvest can tell you how one person varies over time, but
#' it cannot separate "this athlete had an off day" from "this race was slow for
#' everyone" — and that separation is the whole basis for estimating shared
#' condition effects. Only whole fields make it identifiable.
#'
#' Results must be paged by competition day. The endpoint's default response is
#' **not** the whole meet — it returns a single day's slice, and each `day`
#' value yields a different set of events rather than filtering the default.
#' For the 2025 World Championships the default gives 12 events and 541 results
#' while paging days 1-10 gives 49 events and 3,153. Anything harvested without
#' paging is capturing roughly a sixth of what is there.
#'
#' @param competition_id Integer competition id (see [find_competition()]).
#' @param days Competition days to page through. The default spans the longest
#'   championships; days beyond the meet return nothing and cost one request.
#' @return A `data.table` in the canonical result schema with an additional
#'   `race_key` uniquely identifying each race, and `athlete_name`.
#' @examples
#' \dontrun{
#' competition_results(7147633)  # XXII Commonwealth Games
#' }
#' @export
competition_results <- function(competition_id, days = 1:12) {
  base <- paste0(athletics_base_url(), "/competitions/",
                 as.integer(competition_id), "/results")

  pages <- lapply(days, function(d) {
    r <- tryCatch(citius_get_json(paste0(base, "?day=", d)), error = function(e) NULL)
    r$events %||% list()
  })
  events <- unlist(pages, recursive = FALSE)
  # Fall back to the unpaged call for competitions that ignore `day`.
  if (!length(events)) events <- (citius_get_json(base) %||% list())$events %||% list()
  if (!length(events)) return(.empty_result_dt())

  rows <- lapply(events, function(ev) {
    sex <- ev$sex %||% NA_character_
    disc <- ev$discipline %||% NA_character_
    tech <- isTRUE(ev$isTechnical)
    tier <- ev$category %||% NA_character_

    lapply(ev$races %||% list(), function(rc) {
      results <- rc$results %||% list()
      if (!length(results)) return(NULL)
      # `raceId` identifies the ROUND, not the race: all 11 heats of a 100m
      # carry the same one. `raceNumber` is the per-heat discriminator. Without
      # it every heat of an event collapsed into a single race_key, so
      # decompose_races() fitted one shared condition effect across heats that
      # were run separately -- at Glasgow 2026 the wind across those 11 heats
      # ranged 0.6 to 3.9 m/s. Between-heat variation then had nowhere to go but
      # the residual, deflating condition_sd and inflating sigma_within.
      race_key <- paste(competition_id, ev$eventId %||% disc,
                        rc$raceId %||% rc$race, .race_discriminator(rc),
                        sep = "|")

      data.table::rbindlist(lapply(results, function(x) {
        ath <- (x$athletes %||% list())[[1]] %||% list()
        loc <- x$location %||% list()
        data.table::data.table(
          athlete_id   = as.character(ath$id %||% NA),
          athlete_name = trimws(paste(ath$firstname %||% "", ath$lastname %||% "")),
          birthdate    = as_date_safe(ath$birthdate %||% NA),
          date         = as_date_safe(x$date %||% rc$date %||% NA),
          sport        = "Athletics",
          discipline   = disc,
          sex_code     = sex,
          competition_id = as.integer(competition_id),
          race_key     = race_key,
          round        = rc$race %||% NA_character_,
          value_raw    = as.numeric(x$performanceValue %||% NA_real_),
          mark_string  = x$mark %||% NA_character_,
          is_technical = tech,
          place        = suppressWarnings(as.integer(x$place %||% NA_integer_)),
          wind         = as.numeric(x$wind %||% NA_real_),
          indoor       = isTRUE(loc$indoor),
          # Wind legality is a property of the reading, not something to assume.
          # Marks over +2.0 m/s are ineligible for records; hardcoding TRUE here
          # silently treated 7,423 wind-aided marks as legal. They remain usable
          # once wind-adjusted (see adjust_wind), but the flag must be honest.
          legal        = is.na(x$wind %||% NA) | (as.numeric(x$wind %||% 0) <= 2.0),
          tier         = tier,
          venue_country = loc$country %||% NA_character_,
          # City and stadium are needed for altitude, which is a real systematic
          # effect - measured at roughly -0.3% over 5000m even through a crude
          # country proxy. Country alone cannot resolve it, because most South
          # African and Kenyan meets are at altitude but many are not.
          venue_city   = loc$city %||% NA_character_,
          venue_stadium = loc$stadium %||% NA_character_
        )
      }), use.names = TRUE, fill = TRUE)
    })
  })

  dt <- data.table::rbindlist(Filter(Negate(is.null), unlist(rows, recursive = FALSE)),
                              use.names = TRUE, fill = TRUE)
  if (!nrow(dt)) return(.empty_result_dt())

  # Day pages overlap, so the same performance can arrive more than once.
  dt <- unique(dt, by = c("race_key", "athlete_id", "mark_string", "place"))

  dt[, mark := .resolve_mark(mark_string, value_raw, is_technical)]
  dt[, age := as.numeric(date - birthdate) / 365.25]
  dt[, event_id := match_event(discipline, sex_code)]

  reg <- .citius_event_registry[, c("event_id", "orientation")]
  dt <- merge(dt, reg, by = "event_id", all.x = TRUE, sort = FALSE)
  dt[, perf := to_perf(mark, data.table::fifelse(is.na(orientation), -1L, orientation))]
  .warn_implausible_fields(dt)
  dt[]
}


#' Per-race discriminator within a round
#'
#' `raceNumber` separates the heats of a round, but some competitions supply
#' `-1` for it (a not-available sentinel) on every race, which collapses the
#' round back into one key — the exact bug this is meant to prevent.
#'
#' The fallback derives an id from the race's own field. It must be **content**
#' derived rather than positional: day pages overlap, so the same race arrives
#' more than once, and a positional index would give it two different keys and
#' defeat the deduplication in [competition_results()]. The same athletes always
#' hash the same way, however many times the race is fetched.
#'
#' Two heats of one round would have to contain athlete ids summing identically
#' to collide, which is not a realistic risk within a single round.
#'
#' @param rc One `races` element from the feed.
#' @return A length-1 character discriminator.
#' @keywords internal
#' @noRd
.race_discriminator <- function(rc) {
  rn <- suppressWarnings(as.integer(rc$raceNumber %||% NA))
  if (!is.na(rn) && rn > 0L) return(as.character(rn))
  ids <- vapply(rc$results %||% list(), function(x) {
    a <- (x$athletes %||% list())[[1]] %||% list()
    as.character(a$id %||% NA)
  }, character(1))
  ids <- ids[!is.na(ids) & ids != "NA"]
  if (!length(ids)) return("NA")
  paste0("f", sum(as.numeric(ids)) %% 1e9, "n", length(ids))
}


#' Warn when a race key holds more athletes than can be in the race
#'
#' A race key is only meaningful if it identifies one physical race, and nothing
#' else in the pipeline checks that. `raceId` was once used as the key and turned
#' out to identify the *round*: every heat of an event collapsed into a single
#' 76-athlete "race", which [decompose_races()] then fitted with one shared
#' condition effect. Nothing was duplicated or missing, so no test failed — the
#' only visible symptom was a field size no track could hold.
#'
#' Only lane events are checked. Sprints and hurdles are bounded by lane count;
#' everything else legitimately runs deep, a marathon into the hundreds.
#'
#' @param dt Parsed results carrying `race_key`, `event_id` and `round`.
#' @return `dt`, invisibly. Called for its warning.
#' @keywords internal
#' @noRd
.warn_implausible_fields <- function(dt) {
  if (!all(c("race_key", "event_id") %in% names(dt))) return(invisible(dt))
  reg <- .citius_event_registry
  lane <- reg$event_id[reg$family %in% c("sprint", "hurdles")]
  d <- dt[!is.na(race_key) & event_id %in% lane]
  if (!nrow(d)) return(invisible(dt))
  # 10 rather than 9: a dead heat or a reinstated athlete can add a lane.
  bad <- d[, .N, by = race_key][N > 10L]
  if (nrow(bad)) {
    cli::cli_warn(c(
      "{nrow(bad)} lane-event race{?s} hold more than 10 athletes (largest {max(bad$N)}).",
      i = "A race key must identify one physical race; this usually means heats are pooled.",
      i = "Check the feed's per-race discriminator - {.field raceId} is the round, {.field raceNumber} the race."
    ), .frequency = "once", .frequency_id = "citius_pooled_races")
  }
  invisible(dt)
}


#' Harvest several competitions into one canonical table
#'
#' The right harvest unit for calibration. Competition-level results give whole
#' fields, which is what makes the shared race shock identifiable, and they
#' retain results with no valid mark, which is what makes no-mark rates
#' measurable. The athlete-level endpoint provides neither: it cannot group
#' athletes into races, and it silently drops fouls and DNFs.
#'
#' Competitions that error or return nothing are skipped with a message rather
#' than aborting the harvest, since a single unavailable id should not discard
#' everything already collected.
#'
#' @param competition_ids Integer vector of competition ids.
#' @return A `data.table` in the canonical schema with `race_key` attached,
#'   ready for [calibrate()]. **Do not filter out missing marks** before
#'   calibrating — they are the no-mark signal.
#' @examples
#' \dontrun{
#' harvest_competitions(c(7190593, 7153115))
#' }
#' @export
harvest_competitions <- function(competition_ids) {
  out <- lapply(competition_ids, function(cid) {
    r <- tryCatch(competition_results(cid), error = function(e) {
      cli::cli_alert_warning("Competition {cid} failed: {conditionMessage(e)}")
      NULL
    })
    if (is.null(r) || !nrow(r)) {
      cli::cli_alert_warning("Competition {cid} returned no results.")
      return(NULL)
    }
    cli::cli_alert_success("Competition {cid}: {nrow(r)} result{?s}, {data.table::uniqueN(r$race_key)} race{?s}.")
    r
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(.empty_result_dt())
  data.table::rbindlist(out, use.names = TRUE, fill = TRUE)[]
}


#' Look up a World Athletics competition by name
#'
#' @param name Competition name, full or partial (e.g. `"XXIII Commonwealth"`).
#' @return A `data.table` of matching competitions with `competition_id`,
#'   `name`, `city`, `country`, `start`, `end`, `tier`, `has_results`.
#' @examples
#' \dontrun{
#' find_competition("XXIII Commonwealth Games")
#' }
#' @export
find_competition <- function(name) {
  url <- paste0(athletics_base_url(), "/competitions?name=", utils::URLencode(name, reserved = TRUE))
  res <- citius_get_json(url)
  if (is.null(res) || !length(res)) {
    return(data.table::data.table(
      competition_id = integer(), name = character(), city = character(),
      country = character(), start = as.Date(character()), end = as.Date(character()),
      tier = character(), has_results = logical()
    ))
  }

  data.table::rbindlist(lapply(res, function(c_) {
    loc <- c_$location %||% list()
    data.table::data.table(
      competition_id = as.integer(c_$id %||% NA_integer_),
      name    = c_$name %||% NA_character_,
      city    = loc$city %||% NA_character_,
      country = loc$country %||% NA_character_,
      start   = as_date_safe(c_$start %||% NA),
      end     = as_date_safe(c_$end %||% NA),
      tier    = c_$rankingCategory %||% NA_character_,
      has_results = isTRUE(c_$hasResults)
    )
  }), use.names = TRUE, fill = TRUE)[]
}
