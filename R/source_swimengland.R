#' Swim England rankings base URL
#'
#' The rankings database run by Swim England on behalf of Aquatics GB, Swim
#' Wales and Scottish Swimming. Configuration rather than a constant, for the
#' same reason as [aquatics_base_url()] — results services get re-tendered.
#'
#' @return Base URL string.
#' @export
swimengland_base_url <- function() {
  getOption("citius.swimengland_base", "https://www.swimmingresults.org")
}

#' Stroke codes used by the Swim England rankings
#'
#' The feed keys events by an integer that is neither ordered nor contiguous —
#' backstroke is 13–15 while breaststroke is 7–9 — so it is tabulated rather
#' than computed.
#'
#' @return A `data.table` of `stroke_code` and `discipline`.
#' @export
swimengland_strokes <- function() {
  data.table::data.table(
    stroke_code = c(1L, 2L, 3L, 4L, 5L, 6L, 13L, 14L, 15L, 7L, 8L, 9L,
                    10L, 11L, 12L, 18L, 19L, 16L, 17L),
    discipline = c("50m Freestyle", "100m Freestyle", "200m Freestyle",
                   "400m Freestyle", "800m Freestyle", "1500m Freestyle",
                   "50m Backstroke", "100m Backstroke", "200m Backstroke",
                   "50m Breaststroke", "100m Breaststroke", "200m Breaststroke",
                   "50m Butterfly", "100m Butterfly", "200m Butterfly",
                   "100m Individual Medley", "150m Individual Medley",
                   "200m Individual Medley", "400m Individual Medley"))
}

#' Fetch one page of Swim England event rankings
#'
#' Returns the ranked list for one event, pool, sex, year and nationality. Each
#' row is a swimmer's **best** time for that combination, so a year-by-year
#' sweep yields a season-best series per athlete rather than a single number.
#'
#' **These are bests, not full histories.** Ranked lists are truncated at the
#' good end by construction, so they support ability estimation but **not**
#' variance estimation — see the note in `estimate_ability()` about top-lists.
#' The returned rows carry `is_best = TRUE` so a downstream calibration can
#' exclude them rather than silently treating them as ordinary results.
#'
#' @param stroke Stroke code, see [swimengland_strokes()].
#' @param pool `"L"` long course or `"S"` short course. **Never pool the two** —
#'   short course is roughly 5% faster, which is several times the measured
#'   within-athlete spread.
#' @param sex `"M"` or `"F"`.
#' @param year Four-digit year, or `"A"` for all-time.
#' @param nationality One of `A` British, `E` England, `S` Scotland, `W` Wales,
#'   `J` Jersey, `G` Guernsey, `I` Isle of Man, `X` all members.
#' @param start First rank to return (1-based); the feed pages from here.
#' @param n Rows per request. The server caps this at 100 however much is asked
#'   for, so larger values silently do nothing.
#' @return A `data.table` in the canonical result schema, plus `tiref` (the
#'   stable Swim England swimmer id, which makes cross-year linking exact rather
#'   than name-based).
#' @export
swimengland_rankings <- function(stroke, pool = "L", sex = "M", year = "A",
                                 nationality = "A", start = 1L, n = 100L) {
  url <- sprintf(paste0(
    "%s/eventrankings/eventrankings.php?Pool=%s&Stroke=%s&Sex=%s&TargetYear=%s",
    # AgeAt MUST be empty for the Open age group -- any value, including the
    # ones the form itself offers, returns "Invalid Age At".
    "&AgeGroup=OP&AgeAt=&StartNumber=%s&RecordsToView=%s&Level=N",
    "&TargetNationality=%s&TargetRegion=P&TargetCounty=XXXX&TargetClub=XXXX"),
    swimengland_base_url(), pool, stroke, sex, year, start, n, nationality)

  # Fetch errors PROPAGATE -- see the note in aquatics_athlete_results(): the
  # harvesters' own tryCatch is what separates "retry next run" from "cache the
  # empty page", and swallowing the error here breaks that at the source. A
  # definitive 404 returns NULL and stays a legitimate empty result.
  html <- citius_get_html(url)
  if (is.null(html)) return(.empty_se_dt())
  tabs <- rvest::html_elements(html, "table")
  # A page that fetched but does not hold the expected structure is a SCHEMA
  # signal, not an empty rankings list -- a site redesign would otherwise read
  # as "nobody ranked anywhere" for an entire sweep. Rate-limited: one warning
  # identifies the problem; two thousand identical ones bury it.
  if (!length(tabs)) {
    cli::cli_warn("Swim England page has no tables; returning empty. Site structure may have changed.",
                  .frequency = "once", .frequency_id = "citius_se_no_tables")
    return(.empty_se_dt())
  }
  # The page carries more than one table and the rankings are the biggest, but
  # sizing them by parsing each one costs 0.042s against 0.096s of network --
  # 20% of the request spent on work that is thrown away. Counting <tr> nodes
  # answers the same question 42x faster.
  sizes <- vapply(tabs, function(x) length(rvest::html_elements(x, "tr")), integer(1))
  best <- tabs[[which.max(sizes)]]
  tab <- tryCatch(rvest::html_table(best, fill = TRUE), error = function(e) {
    cli::cli_warn("Swim England table failed to parse; returning empty. Site structure may have changed.",
                  .frequency = "once", .frequency_id = "citius_se_parse_fail")
    NULL
  })
  if (is.null(tab) || !nrow(tab)) return(.empty_se_dt())
  tab <- data.table::as.data.table(tab)
  need <- c("Rank", "Name", "Time", "Date")
  if (!all(need %in% names(tab))) {
    cli::cli_warn("Swim England table lacks column{?s} {.field {setdiff(need, names(tab))}}; returning empty.",
                  .frequency = "once", .frequency_id = "citius_se_missing_cols")
    return(.empty_se_dt())
  }

  # Pull the swimmer id per ROW rather than from the page, so a row without a
  # link cannot shift every subsequent id by one.
  rows <- rvest::html_elements(best, "tr")
  tiref <- vapply(rows, function(r) {
    a <- rvest::html_element(r, "a[href*='tiref=']")
    h <- rvest::html_attr(a, "href")
    if (is.na(h)) NA_character_ else sub(".*tiref=([0-9]+).*", "\\1", h)
  }, character(1))
  # Header rows carry no link; align by dropping leading non-data rows. (An
  # always-true filter that claimed to do this was removed 2026-08-14 -- the
  # tail() below is the whole alignment.)
  tiref <- utils::tail(tiref, nrow(tab))

  disc <- swimengland_strokes()[stroke_code == as.integer(stroke), discipline]
  out <- data.table::data.table(
    athlete_id   = paste0("SE", tiref),
    tiref        = tiref,
    athlete_name = trimws(as.character(tab$Name)),
    club         = if ("Ranked Club" %in% names(tab)) trimws(as.character(tab$`Ranked Club`)) else NA_character_,
    # Guarded like `club` and `comp_name`: an absent column is NULL, and
    # as.integer(NULL) is integer(0), which crashes the data.table() call on a
    # length mismatch instead of degrading to NA like its siblings.
    yob          = if ("YoB" %in% names(tab)) suppressWarnings(as.integer(tab$YoB)) else NA_integer_,
    sport        = "Swimming",
    discipline   = if (length(disc)) disc else NA_character_,
    mark_string  = trimws(as.character(tab$Time)),
    place        = suppressWarnings(as.integer(tab$Rank)),
    comp_name    = if ("Meet" %in% names(tab)) trimws(as.character(tab$Meet)) else NA_character_,
    date         = .se_parse_date(tab$Date),
    course       = if (pool == "L") "LCM" else "SCM",
    sex          = sex,
    nationality  = nationality,
    season       = year,
    # Ranked lists are truncated at the good end. Flagging it here means a
    # calibration can exclude them explicitly instead of a future reader having
    # to know that this source is different from the others.
    is_best      = TRUE)
  out[!is.na(athlete_name) & nzchar(athlete_name)]
}

#' Parse the feed's dd/mm/yy dates without the POSIX century roll
#'
#' `as.Date(x, "%d/%m/%y")` maps two-digit years 00-68 to the 2000s and 69-99
#' to the 1900s, so an all-time (`year = "A"`) archive row from 1965 silently
#' lands in 2065-adjacent territory. The feed cannot contain future results, so
#' the rule here is: a parsed year later than next year belongs to the previous
#' century.
#' @keywords internal
#' @noRd
.se_parse_date <- function(x) {
  d <- as.Date(as.character(x), format = "%d/%m/%y")
  # An unparseable date is not merely missing: result_weight() gives an NA
  # date FULL recency weight (documented choice in ability.R), so silent NAs
  # here quietly promote mis-formatted rows. Count them out loud.
  n_bad <- sum(is.na(d) & !is.na(x) & nzchar(trimws(as.character(x))))
  if (n_bad) {
    cli::cli_warn("{n_bad} Swim England date{?s} failed to parse as dd/mm/yy and {?is/are} NA.",
                  .frequency = "once", .frequency_id = "citius_se_bad_dates")
  }
  yr <- as.integer(format(d, "%Y"))
  ceiling_yr <- as.integer(format(Sys.Date(), "%Y")) + 1L
  roll <- !is.na(yr) & yr > ceiling_yr
  if (any(roll)) {
    d[roll] <- as.Date(sprintf("%04d-%s", yr[roll] - 100L, format(d[roll], "%m-%d")))
  }
  d
}

.empty_se_dt <- function() {
  data.table::data.table(
    athlete_id = character(), tiref = character(), athlete_name = character(),
    club = character(), yob = integer(), sport = character(),
    discipline = character(), mark_string = character(), place = integer(),
    comp_name = character(), date = as.Date(character()), course = character(),
    sex = character(), nationality = character(), season = character(),
    is_best = logical())
}
