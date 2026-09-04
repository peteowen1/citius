#' Fetch a competition's full results directly from worldathletics.org
#'
#' A third source in the same family as [athletics_calendar()] and
#' [athletics_athlete_official_profile()]: `worldathletics.org/competition/
#' calendar-results/results/{competition_id}` is a Next.js page whose
#' server-fetched data is embedded in the response HTML as
#' `window.__NEXT_DATA__`, fetchable via a plain GET, no browser needed.
#'
#' Built to solve a real, concrete problem: on 2026-08-30, 26 competitions
#' (15 historic Diamond League editions plus 11 other T1/T2 meets) were
#' found to consistently fail with HTTP 500 from the community wrapper
#' (`worldathletics.nimarion.de`, what [athletics_competition_results()]
#' uses) -- confirmed NOT a stale-id problem (World Athletics' own site
#' resolves every one of them at the SAME id, with `hasResults = TRUE`), so
#' the wrapper itself has a bug specific to these competitions. This
#' function fetches from a different first-party surface entirely, verified
#' 2026-08-30 to return real, complete results for at least one of the 26
#' (competition 7214476, 17 events).
#'
#' The response nests four levels deep: competition -> `eventTitles` ->
#' `events` -> `races` -> `results`. Flattened here into one row per result.
#'
#' @param competition_id Integer World Athletics competition id -- the same
#'   namespace as [athletics_calendar()]'s `id` and this package's
#'   `athletics_competition_results()`.
#' @return A `data.table`, one row per result, with `competition_id`,
#'   `comp_name`, `venue`, `comp_start`, `comp_end`, `ranking_category`,
#'   `event_name`, `event_id`, `sex_code` (`"M"`/`"W"`, from the response's
#'   `gender`), `is_relay`, `round` (the response's `race` field, e.g.
#'   `"Final"`), `race_id`, `race_number`, `race_date` (frequently `NA` --
#'   see Details), `athlete_id` (parsed from the competitor's `urlSlug`,
#'   e.g. `"ukraine/mykhaylo-kokhan-14686964"` -> `14686964`; `NA` if the
#'   slug doesn't end in a parseable id), `athlete_name`, `athlete_iaaf_id`,
#'   `birthdate`, `nationality`, `mark`, `place`, `points`, `wind`,
#'   `records`, `remark`. Attribute `"fetch_ok"` follows the same
#'   confirmed-vs-unconfirmed discipline as the other two functions in this
#'   family: `TRUE` for a well-formed response (whether or not it has
#'   results -- e.g. a competition with `hasResults = FALSE` on the
#'   calendar can render a page with empty `eventTitles`), `FALSE` when the
#'   fetch itself could not be confirmed (missing/unparseable
#'   `__NEXT_DATA__`, most often a WAF/bot-detection interstitial returning
#'   HTTP 200 with no real payload). A definitive 404 from
#'   [citius_get_html()] is `fetch_ok = TRUE` (a confirmed answer -- the
#'   competition id doesn't resolve here), matching the fix already applied
#'   to the other two functions in this family.
#'
#' @section Known schema gap -- NOT a drop-in replacement for `championship_results.rds`:
#' This endpoint's fields do not map cleanly onto `CITIUS_DB_SCHEMA
#' $championship_results` (`R/db_schema.R`). What's genuinely absent here
#' and would need separate derivation before any merge into the corpus:
#' `race_key` (no equivalent field -- would need constructing from
#' `competition_id`/`event_id`/`race_id`, and this endpoint's `race_id`
#' appears to be a small shared code across competitions, e.g. `10` for a
#' plain final, not competition-unique -- verify uniqueness before using it
#' as a key component), `discipline`/`discipline_code` (only a free-text
#' `event_name` like `"Men's Hammer Throw"` is given, mixing discipline and
#' sex into one string), `venue_country`/`venue_city`/`venue_stadium` (only
#' one free-text `venue` string, e.g. `"Zdzisław Krzyszkowiak Stadium,
#' Bydgoszcz (POL)"`), `is_technical`/`legal`/`tier`/`orientation`/`perf`
#' (all derived/computed columns in this package's model, not present on
#' any raw source), `age` (would need `birthdate` combined with `race_date`,
#' which is frequently `NULL` in the raw response -- confirmed on
#' competition 7214476, only `comp_start`/`comp_end` are reliably
#' populated), `value_raw`/`mark_string` (only a single already-formatted
#' `mark` string is given, e.g. `"79.84"`, not the raw/display split this
#' package's schema expects), `indoor`, `comp_day`, `birthdate_year_only`.
#' Using this function's output to backfill the corpus needs a real mapping
#' layer, not attempted here.
#'
#' @examples
#' \dontrun{
#' athletics_calendar_results(7214476)
#' }
#' @export
athletics_calendar_results <- function(competition_id) {
  competition_id <- as.integer(competition_id)
  url <- paste0("https://worldathletics.org/competition/calendar-results/results/", competition_id)

  doc <- citius_get_html(url)
  # Same discipline as athletics_calendar()/athletics_athlete_official_profile():
  # citius_get_html() returning NULL is a CONFIRMED 404, not an ambiguous
  # failure (see its own docs -- any other non-404 error status aborts loudly
  # instead of returning NULL).
  if (is.null(doc)) return(.empty_calendar_results_dt(fetch_ok = TRUE))
  node <- xml2::xml_find_first(doc, "//script[@id='__NEXT_DATA__']")
  if (inherits(node, "xml_missing")) {
    cli::cli_warn(c(
      "No {.val __NEXT_DATA__} script found at {.url {url}}.",
      i = "Page structure changed, or the request was blocked (WAF/interstitial). Treating as UNCONFIRMED, not \"no results\" -- check attr(result, \"fetch_ok\")."
    ))
    return(.empty_calendar_results_dt(fetch_ok = FALSE))
  }

  j <- tryCatch(
    jsonlite::fromJSON(xml2::xml_text(node), simplifyVector = FALSE),
    error = function(e) {
      cli::cli_warn(c(
        "Could not parse {.val __NEXT_DATA__} JSON at {.url {url}}: {conditionMessage(e)}",
        i = "Likely a truncated response. Treating as UNCONFIRMED, not \"no results\" -- check attr(result, \"fetch_ok\")."
      ))
      NULL
    }
  )
  if (is.null(j)) return(.empty_calendar_results_dt(fetch_ok = FALSE))

  cer <- j$props$pageProps$calendarEventsResults
  comp <- cer$competition
  event_titles <- cer$eventTitles

  if (is.null(comp) || is.null(event_titles) || !length(event_titles)) {
    # A confirmed, well-formed response with legitimately no results (e.g. a
    # competition the calendar already flags hasResults = FALSE).
    return(.empty_calendar_results_dt(fetch_ok = TRUE))
  }

  # Flatten eventTitles -> events -> races -> results. Written as nested
  # rbindlist calls, one level at a time, rather than a single dense
  # comprehension -- each level's shape is different enough (events carries
  # per-discipline metadata, races carries per-round metadata, results
  # carries per-athlete data) that collapsing them in one pass would hide
  # which field comes from which level.
  event_rows <- lapply(event_titles, function(et) {
    events <- et$events %||% list()
    if (!length(events)) return(NULL)
    data.table::rbindlist(lapply(events, function(ev) {
      races <- ev$races %||% list()
      if (!length(races)) return(NULL)
      race_rows <- data.table::rbindlist(lapply(races, function(race) {
        results <- race$results %||% list()
        if (!length(results)) return(NULL)
        data.table::rbindlist(lapply(results, function(r) {
          cp <- r$competitor %||% list()
          # urlSlug looks like "ukraine/mykhaylo-kokhan-14686964" -- the
          # trailing run of digits after the final hyphen is the athlete_id
          # (same aaId namespace confirmed elsewhere in this package).
          # Verified against several real results 2026-08-30; falls back to
          # NA rather than guessing if a slug doesn't match the pattern.
          slug <- cp$urlSlug %||% NA_character_
          aid <- if (!is.na(slug)) {
            m <- regmatches(slug, regexpr("(\\d+)$", slug))
            if (length(m) && nzchar(m)) as.integer(m) else NA_integer_
          } else NA_integer_
          data.table::data.table(
            event_name       = ev$event %||% NA_character_,
            event_id         = as.integer(ev$eventId %||% NA_integer_),
            sex_code          = data.table::fifelse(is.null(ev$gender), NA_character_,
                                                     as.character(ev$gender)),
            is_relay          = isTRUE(ev$isRelay),
            round             = race$race %||% NA_character_,
            race_id           = as.integer(race$raceId %||% NA_integer_),
            race_number       = as.integer(race$raceNumber %||% NA_integer_),
            race_date         = as_date_safe(race$date %||% NA),
            athlete_id        = aid,
            athlete_name      = cp$name %||% NA_character_,
            athlete_iaaf_id   = as.integer(cp$iaafId %||% NA_integer_),
            birthdate         = tryCatch(
                                   as.Date(cp$birthDate %||% NA, format = "%d %b %Y"),
                                   error = function(e) as.Date(NA)),
            nationality       = r$nationality %||% NA_character_,
            mark              = r$mark %||% NA_character_,
            place             = r$place %||% NA_character_,
            points            = r$points %||% NA_character_,
            wind              = r$wind %||% NA_character_,
            records           = r$records %||% NA_character_,
            remark            = r$remark %||% NA_character_
          )
        }), use.names = TRUE, fill = TRUE)
      }), use.names = TRUE, fill = TRUE)
      race_rows
    }), use.names = TRUE, fill = TRUE)
  })
  dt <- data.table::rbindlist(event_rows, use.names = TRUE, fill = TRUE)

  if (!nrow(dt)) return(.empty_calendar_results_dt(fetch_ok = TRUE))

  dt[, `:=`(
    competition_id   = competition_id,
    comp_name        = comp$name %||% NA_character_,
    venue            = comp$venue %||% NA_character_,
    comp_start       = as_date_safe(comp$startDate %||% NA),
    comp_end         = as_date_safe(comp$endDate %||% NA),
    ranking_category = comp$rankingCategory %||% NA_character_
  )]
  data.table::setattr(dt, "fetch_ok", TRUE)
  data.table::setcolorder(dt, c("competition_id", "comp_name", "venue", "comp_start",
                                 "comp_end", "ranking_category"))
  dt[]
}

.empty_calendar_results_dt <- function(fetch_ok = TRUE) {
  out <- data.table::data.table(
    competition_id = integer(), comp_name = character(), venue = character(),
    comp_start = as.Date(character()), comp_end = as.Date(character()),
    ranking_category = character(), event_name = character(),
    event_id = integer(), sex_code = character(), is_relay = logical(),
    round = character(), race_id = integer(), race_number = integer(),
    race_date = as.Date(character()), athlete_id = integer(),
    athlete_name = character(), athlete_iaaf_id = integer(),
    birthdate = as.Date(character()), nationality = character(),
    mark = character(), place = character(), points = character(),
    wind = character(), records = character(), remark = character()
  )
  data.table::setattr(out, "fetch_ok", fetch_ok)
  out
}
