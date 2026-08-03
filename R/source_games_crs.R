#' Parse a Games Competition Results System export
#'
#' Major multi-sport Games run their results through a Competition Results
#' System — Glasgow 2026 uses Microplus — which serves a public results app
#' backed by an authenticated API. The API cannot be reached unattended
#' (Cloudflare plus a runtime bearer token), but the app renders everything
#' publicly, so results are captured from the rendered pages and exported as
#' JSON. This parses that export into the canonical schema.
#'
#' The export format is deliberately compact: a `p` array of pages
#' (`route`, `when`, `title`) and an `r` array of rows referencing pages by
#' index.
#'
#' Events the registry does not recognise — relays, Para classifications —
#' resolve to `NA` `event_id` rather than being forced onto a nearby event.
#' They are retained so the export stays faithful to what was scraped; filter
#' on `!is.na(event_id)` before modelling.
#'
#' @param path Path to the exported JSON.
#' @return A `data.table` in the canonical result schema, with swimming extras
#'   `reaction_time`, `lane` and `country`, plus `race_key` for [calibrate()].
#' @examples
#' \dontrun{
#' res <- parse_crs_export("citiusdata/data/glasgow2026_swimming.json")
#' res[!is.na(event_id) & round == "Final"]
#' }
#' @export
parse_crs_export <- function(path) {
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  pages <- raw$p %||% list()
  rows <- raw$r %||% list()
  if (!length(rows)) return(.empty_result_dt())

  pg <- data.table::rbindlist(lapply(pages, function(p) {
    data.table::data.table(route = p[[1]], when = p[[2]], title = p[[3]])
  }))
  pg[, page_idx := .I - 1L]

  dt <- data.table::rbindlist(lapply(rows, function(r) {
    data.table::data.table(
      page_idx = as.integer(r[[1]]),
      heat = as.character(r[[2]]),
      place_raw = as.character(r[[3]]),
      lane = suppressWarnings(as.integer(r[[4]])),
      country = as.character(r[[5]]),
      athlete_name = as.character(r[[6]]),
      reaction_time = suppressWarnings(as.numeric(
        if (nzchar(r[[7]])) r[[7]] else NA)),
      mark_string = as.character(r[[8]])
    )
  }))

  dt <- merge(dt, pg, by = "page_idx", all.x = TRUE, sort = FALSE)

  dt[, sex := .crs_sex(route)]
  dt[, discipline := .crs_discipline(title)]
  dt[, round := .crs_round(title, heat)]
  dt[, date := .crs_date(when)]
  dt[, sport := "Swimming"]
  dt[, mark := parse_mark(mark_string)]
  dt[, place := suppressWarnings(as.integer(place_raw))]
  # DSQ/DNS/DNF carry no valid performance; keep the row, drop the mark.
  dt[!grepl("^[0-9]+$", place_raw), mark := NA_real_]
  dt[, event_id := match_event(discipline, sex)]

  reg <- .citius_event_registry[, c("event_id", "orientation")]
  dt <- merge(dt, reg, by = "event_id", all.x = TRUE, sort = FALSE)
  # NA orientation must stay NA. Defaulting an unmatched event to -1L
  # (time-event) silently produced a WRONG-SIGNED perf for unmatched FIELD
  # events, undoing the guarantee match_event() exists to give.
  dt[, perf := to_perf(mark, orientation)]

  # Athletes have no stable id in the export; the name is the only key.
  dt[, athlete_id := athlete_name]
  dt[, race_key := paste(route, heat, sep = "|")]
  dt[, c("page_idx", "place_raw") := NULL]
  dt[]
}

#' @keywords internal
#' @noRd
.crs_sex <- function(route) {
  # `regmatches(x, regexpr(...))` DROPS non-matching elements rather than
  # returning NA in place, so the result is shorter than the input whenever any
  # route lacks a sex segment -- and `rep_len` then recycles from the start,
  # silently shifting a valid-looking "M" or "W" onto every row after the gap.
  # Verified: routes M, W, <none>, W, M returned M, W, W, M, M.
  #
  # A wrong-but-plausible sex is the worst possible failure here, because
  # match_event() returning NA is the package's only guard against silent
  # corruption and this routed straight around it -- a women's 200 Freestyle
  # would be filed as men's. Assign back by position, the way .crs_date() below
  # already does.
  pos <- regexpr("/(M|W|X)/", route)
  out <- rep(NA_character_, length(route))
  hit <- pos > 0
  if (any(hit)) out[hit] <- gsub("/", "", regmatches(route, pos))
  out[!is.na(out) & out == "X"] <- NA_character_   # mixed relays have no sex
  out
}

#' @keywords internal
#' @noRd
.crs_discipline <- function(title) {
  x <- toupper(as.character(title))
  x <- sub("^(MEN|WOMEN|MIXED)'?S?\\s+", "", x)
  # Strip the round suffix; the round is captured separately.
  x <- sub("\\s*-?\\s*(HEATS?|SEMI-?FINALS?|FINAL|SLOWEST HEAT.*|FASTEST HEAT.*)\\s*$", "", x)
  trimws(x)
}

#' @keywords internal
#' @noRd
.crs_round <- function(title, heat) {
  t <- toupper(as.character(title))
  out <- rep(NA_character_, length(t))
  out[grepl("HEAT", t)] <- "Heat"
  out[grepl("SEMI", t)] <- "Semifinal"
  out[grepl("FINAL", t) & !grepl("SEMI", t)] <- "Final"
  # The per-heat label is more specific where present
  h <- trimws(as.character(heat))
  data.table::fifelse(nzchar(h) & !is.na(h), h, out)
}

#' @keywords internal
#' @noRd
.crs_date <- function(when) {
  d <- regmatches(when, regexpr("[0-9]{1,2} [A-Za-z]{3} [0-9]{4}", when))
  if (!length(d)) return(as.Date(rep(NA, length(when))))
  out <- rep(as.Date(NA), length(when))
  hit <- regexpr("[0-9]{1,2} [A-Za-z]{3} [0-9]{4}", when) > 0
  out[hit] <- as.Date(d, format = "%d %b %Y")
  out
}
