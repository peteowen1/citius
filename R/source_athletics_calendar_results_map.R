#' Map athletics_calendar_results() output onto championship_results' schema
#'
#' `athletics_calendar_results()`'s own `@section` documents real,
#' irreducible gaps against `CITIUS_DB_SCHEMA$championship_results`
#' (`db_schema.R`) -- this function does NOT fill those gaps, it maps what
#' is genuinely derivable and leaves the rest `NA`, matching the schema
#' guard's warn-only (not abort) policy for missing columns. Built to
#' backfill the 22 competitions found resolvable via the direct-results
#' path after the `worldathletics.nimarion.de` wrapper failed on them
#' (2026-08-30) -- see `docs/reference/harvesting.md` and this session's
#' history for context. This is a one-off backfill helper, not a new
#' standing harvest path.
#'
#' Derivable, not left NA, because they are computable from what the source
#' DOES give (not fabricated):
#' - `event_id` (canonical citius id) via `match_event()` on the raw
#'   `event_name` + `sex_code` -- the source's OWN `event_id` is World
#'   Athletics' internal id, a different namespace, kept only as an input to
#'   `race_key` (mirroring `source_athletics.R`'s existing convention of
#'   building `race_key` from the raw source id before `match_event()` ever
#'   runs), never written to the output's `event_id` column.
#' - `is_technical`, `orientation` via the matched event's row in
#'   `citius_events()` -- registry properties, not per-result data.
#' - `perf` via `to_perf(mark, orientation)`, once `mark` and `orientation`
#'   are known.
#' - `date` prefers `race_date`; where that is `NA` (common -- see the
#'   fetch function's own docs) falls back to `comp_start`, which is always
#'   populated. This is an approximation for a multi-day meet's later
#'   rounds, not a defect -- callers needing exact per-round dates cannot
#'   get them from this source at all.
#' - `race_key`: `competition_id | wa_event_id | discipline | race_id |
#'   round | discriminator` (`round` is the response's own race label, e.g.
#'   `"Heat 1"`/`"Final"` -- real supplied data, folded in unconditionally
#'   since it costs nothing when already distinct and is exactly what
#'   separates two races when `race_id` AND `race_number` both collide),
#'   mirroring `source_athletics.R`'s existing pattern of
#'   keying on the RAW source event id plus a race identifier plus the
#'   discipline text (not `race_id` alone, which this endpoint's own docs
#'   note is a small shared code across competitions, e.g. `"10"` for a
#'   plain final -- not competition-unique on its own). The discriminator is
#'   `race_number` ONLY where it actually distinguishes every race within
#'   that (competition, event) -- non-`NA`, positive, not duplicated. Where
#'   it doesn't (found live on this endpoint, not hypothetical: a sentinel
#'   like `-1`/`1` repeated across an event's races), it falls back to a
#'   content hash of the race's athlete ids, exactly mirroring
#'   `source_athletics.R`'s `.race_discriminator()` -- reusing that internal
#'   function directly would require it to operate on nested feed JSON this
#'   already-flattened input doesn't have, so this re-implements the same
#'   rule against a flat `data.table` instead. **Trusting `race_number` raw
#'   is exactly the bug that corrupted `condition_sd`/`sigma_within` in the
#'   2026-07-27 Glasgow incident** (`docs/incidents/
#'   race-key-heat-collapse-2026-07-27.md`) -- silently, no test failed.
#'   **Residual, honestly-unfixable gap**: if `race_id`, `race_number` AND
#'   `round` ALL collide for two genuinely different races in one event,
#'   this function cannot separate them -- the flattened input has already
#'   lost whatever distinguished them in the source's nested JSON (their
#'   original array position), and no field survives to reconstruct it
#'   from. Tested with a synthetic worst case 2026-08-30: confirmed this
#'   collapses to one `race_key` rather than erroring. Not observed on real
#'   data (round labels are effectively always distinct per real WA
#'   competition); flagged as a known limit, not assumed impossible.
#'
#' Left `NA`, genuinely absent from this source, not guessed:
#' `birthdate_year_only`, `sport` (set to the fixed `"Athletics"` literal,
#' matching `source_athletics.R`'s convention, not left `NA`),
#' `discipline_code`, `event_name` (this source has no separate age-division
#' qualifier field distinct from `discipline`), `comp_day`, `value_raw`
#' (no raw `performanceValue` integer, only the pre-formatted display
#' `mark` string), `indoor`, `legal`, `tier`, `comp_tier`, `age` (would need
#' a reliable per-result date, which `date` above is only an approximation
#' of), `venue_country`/`venue_city`/`venue_stadium` -- attempted via
#' best-effort parse of the single free-text `venue` field (pattern
#' `"<stadium>, <city> (<COUNTRY>)"`), left `NA` when the string doesn't
#' match that shape rather than guessing a split.
#'
#' @param results Output of [athletics_calendar_results()].
#' @return A `data.table` with `CITIUS_DB_SCHEMA$championship_results`'s
#'   column set (any column this source cannot supply is present as an
#'   all-`NA` column, not omitted, so `rbindlist(fill = TRUE)` against the
#'   existing corpus does not silently reorder columns).
#' @examples
#' \dontrun{
#' r <- athletics_calendar_results(7214476)
#' map_calendar_results_to_championship_schema(r)
#' }
#' @export
map_calendar_results_to_championship_schema <- function(results) {
  if (!nrow(results)) {
    return(.empty_mapped_championship_dt())
  }
  # Relays are explicitly unmodelled elsewhere in this package (citius#1) --
  # excluded here too, not just left in with holes. A relay entry has no
  # individual competitor urlSlug (a national federation fields the team,
  # not one athlete), so athlete_id is NA for every relay row; when a race
  # fields more than one relay team (e.g. two age-group squads from the same
  # country), all of them collide on the same NA athlete_id within one
  # race_key. Found on real data (2026-08-30, competition 7214476: two
  # "Poland" youth relay entries in one race). Filtering on is_relay
  # resolves this at the source rather than downstream.
  results <- results[!(results$is_relay %in% TRUE)]
  if (!nrow(results)) return(.empty_mapped_championship_dt())
  dt <- data.table::copy(results)

  # Best-effort venue split: "<stadium>, <city> (<COUNTRY>)". Left NA on any
  # shape this doesn't match -- a missing value is honest, a wrong one is
  # not (see roxygen above).
  venue_m <- regmatches(dt$venue,
    regexec("^(.*?),\\s*([^,()]+?)\\s*\\(([A-Z]{2,4})\\)\\s*$", dt$venue))
  venue_ok <- vapply(venue_m, length, integer(1)) == 4L
  venue_stadium <- ifelse(venue_ok, vapply(venue_m, `[`, character(1), 2), NA_character_)
  venue_city    <- ifelse(venue_ok, vapply(venue_m, `[`, character(1), 3), NA_character_)
  venue_country <- ifelse(venue_ok, vapply(venue_m, `[`, character(1), 4), NA_character_)

  # Canonical citius event_id, via the SAME resolver every other harvest
  # adapter uses -- NA where the source's discipline text doesn't match the
  # registry, never guessed.
  event_id <- match_event(dt$event_name, dt$sex_code)

  # Registry properties (technical/orientation), looked up by the matched
  # event_id -- not per-result data, so a vectorised join, not a formula.
  reg <- citius_events()
  reg_idx <- match(event_id, reg$event_id)
  is_technical <- reg$technical[reg_idx]
  orientation  <- reg$orientation[reg_idx]

  mark_num <- parse_mark(dt$mark)
  perf <- ifelse(is.na(mark_num) | is.na(orientation), NA_real_,
                 to_perf(mark_num, orientation))

  date <- dt$race_date
  date[is.na(date)] <- dt$comp_start[is.na(date)]

  # race_number is trusted only where it actually distinguishes every race
  # within a (competition, wa event) group -- non-NA, positive, not
  # duplicated. Where it doesn't, a content hash of that race's athlete ids
  # stands in instead. Mirrors source_athletics.R's .race_discriminator()/
  # rn_usable pattern on a flat data.table rather than nested feed JSON --
  # see the roxygen above for why this exists (2026-07-27 Glasgow incident).
  # `round` (the response's race label, e.g. "Heat 1"/"Final") is REAL,
  # already-supplied data -- not fabricated -- and is exactly the field that
  # distinguishes two races when BOTH race_id and race_number collide
  # (found live via a synthetic test 2026-08-30: race_id and race_number
  # alone are not always sufficient, since race_id can repeat across an
  # event's races just as race_number can). Folded into the race grouping
  # unconditionally, not just as a fallback, since it costs nothing when
  # it's already distinct and fixes the case when it isn't.
  race_id_key <- paste(dt$race_id, dt$round, sep = "␞")
  grp <- interaction(dt$competition_id, dt$event_id, drop = TRUE)
  rn <- suppressWarnings(as.integer(dt$race_number))
  rn_usable <- unsplit(lapply(split(seq_along(rn), grp), function(idx) {
    n_races <- length(unique(race_id_key[idx]))
    ok <- n_races <= 1 || (!anyNA(rn[idx]) && all(rn[idx] > 0L) &&
                            !anyDuplicated(rn[idx][!duplicated(race_id_key[idx])]))
    rep(ok, length(idx))
  }), grp)
  race_of <- interaction(dt$competition_id, dt$event_id, race_id_key, drop = TRUE)
  content_hash <- unsplit(lapply(split(dt$athlete_id, race_of), function(a) {
    ids <- a[!is.na(a)]
    val <- if (!length(ids)) "NA" else paste0("f", sum(as.numeric(ids)) %% 1e9, "n", length(ids))
    rep(val, length(a))  # replicate to the GROUP's size, not the filtered ids' size
  }), race_of)
  discriminator <- ifelse(rn_usable & !is.na(rn) & rn > 0L, as.character(rn), content_hash)

  race_key <- paste(dt$competition_id, dt$event_id, dt$event_name,
                    dt$race_id, dt$round, discriminator, sep = "|")

  out <- data.table::data.table(
    competition_id       = dt$competition_id,
    event_id              = event_id,
    athlete_id            = dt$athlete_id,
    athlete_name          = dt$athlete_name,
    birthdate              = dt$birthdate,
    birthdate_year_only   = NA,
    date                   = date,
    sport                  = "Athletics",
    discipline             = dt$event_name,
    discipline_code       = NA_character_,
    event_name            = NA_character_,
    comp_day               = NA_integer_,
    sex_code               = dt$sex_code,
    race_key                = race_key,
    round                   = dt$round,
    value_raw               = NA_real_,
    mark_string            = dt$mark,
    is_technical           = is_technical,
    place                   = suppressWarnings(as.integer(sub("\\.$", "", dt$place))),
    wind                    = suppressWarnings(as.numeric(dt$wind)),
    indoor                  = NA,
    legal                   = NA,
    tier                    = NA_character_,
    venue_country          = venue_country,
    venue_city              = venue_city,
    venue_stadium          = venue_stadium,
    mark                    = mark_num,
    age                     = NA_real_,
    orientation             = orientation,
    perf                    = perf,
    comp_name               = dt$comp_name,
    comp_start             = dt$comp_start,
    comp_tier               = NA_character_
  )

  .drop_contradictory_track_rows(out)
}

#' Drop track-event rows where one athlete has disagreeing mark/place for
#' the same race
#'
#' Field events legitimately give one athlete several rows per `race_key`
#' (multiple attempts) -- established convention throughout this package,
#' kept here (`is_technical == TRUE` is never touched by this function).
#' Track events have no attempts, so >1 row for the same
#' (`race_key`, `athlete_id`) with a DIFFERENT `mark` or `place` is not
#' legitimate data, it is source ambiguity -- found on real data
#' (2026-08-30, 4 pairs, e.g. one athlete credited "10.23" place 3 AND
#' "10.23" place 1 in the same race). The first pass through this backfill
#' excluded those 4 pairs out of band, in an interactive session, not in
#' committed code -- so the exclusion never actually protected a future
#' re-run. This makes it a real, general rule instead: guess neither row is
#' right, drop the whole conflicting group, and say how many rows that cost.
#' @keywords internal
#' @noRd
.drop_contradictory_track_rows <- function(dt) {
  # NA athlete_id EXCLUDED from this check, not just from the grouping key.
  # `athlete_id` is NA whenever a competitor's urlSlug fails to parse -- not
  # rare -- and data.table groups every NA together, so two DIFFERENT real
  # athletes with unparseable slugs in the same race land in one
  # (race_key, NA) group. Their marks genuinely differ (different people), the
  # group reads as "disagreeing mark/place," and both athletes' real results
  # were dropped as if they were one contradictory athlete. Found by review
  # 2026-09-04 -- same bug class already fixed for competition_id elsewhere in
  # this file, reintroduced here. Rows with NA athlete_id pass through
  # untouched: their data isn't wrong, it just can't be compared against a
  # stranger's.
  track <- dt[is_technical %in% FALSE & !is.na(athlete_id)]
  if (!nrow(track)) return(dt)
  bad_keys <- track[, .(n_marks = data.table::uniqueN(mark_string),
                        n_places = data.table::uniqueN(place)),
                    by = .(race_key, athlete_id)][n_marks > 1 | n_places > 1]
  if (!nrow(bad_keys)) return(dt)
  bad <- dt[bad_keys, on = c("race_key", "athlete_id"), nomatch = NULL]
  cli::cli_warn(c(
    "{nrow(bad)} row{?s} across {nrow(bad_keys)} (race_key, athlete_id) pair{?s} had disagreeing mark/place for a track event -- dropped, not guessed at.",
    i = "Source data is ambiguous here, not this package's data. See {.fn .drop_contradictory_track_rows}."
  ))
  dt[!bad_keys, on = c("race_key", "athlete_id")]
}

.empty_mapped_championship_dt <- function() {
  data.table::data.table(
    competition_id = integer(), event_id = character(), athlete_id = integer(),
    athlete_name = character(), birthdate = as.Date(character()),
    birthdate_year_only = logical(), date = as.Date(character()),
    sport = character(), discipline = character(), discipline_code = character(),
    event_name = character(), comp_day = integer(), sex_code = character(),
    race_key = character(), round = character(), value_raw = numeric(),
    mark_string = character(), is_technical = logical(), place = integer(),
    wind = numeric(), indoor = logical(), legal = logical(), tier = character(),
    venue_country = character(), venue_city = character(), venue_stadium = character(),
    mark = numeric(), age = numeric(), orientation = numeric(), perf = numeric(),
    comp_name = character(), comp_start = as.Date(character()), comp_tier = character()
  )
}
