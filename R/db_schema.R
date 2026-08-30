# Expected column schemas for citius's DuckDB-backed stores.
#
# citius's equivalent of torp's column_schema.R: every store table declares
# its expected columns here, so `.citius_store_merge()` has something to check
# new data against without querying an empty/not-yet-created table. Column
# lists and types below were read directly off the current production RDS
# files (citiusdata/data/*.rds) on 2026-08-30 -- not guessed.
#
# When a column is added or removed upstream, update the relevant entry here
# in the SAME commit as the writer change, or the schema guard in
# duckdb_store.R will reject the next write for the wrong reason.
#
# `championship_results`'s comp_name/comp_start/comp_tier were briefly absent
# from the source file on 2026-08-29 (a data-recovery mistake, since fixed)
# and so were briefly absent here too. They are legitimate columns --
# athletics_corpus's entry already carries comp_name -- restored once the
# recovery was corrected on 2026-08-30.

#' Expected columns per citius DuckDB store table
#'
#' @keywords internal
CITIUS_DB_SCHEMA <- list(
  championship_results = c(
    "competition_id", "event_id", "athlete_id", "athlete_name", "birthdate",
    "birthdate_year_only", "date", "sport", "discipline", "discipline_code",
    "event_name", "comp_day", "sex_code", "race_key", "round", "value_raw",
    "mark_string", "is_technical", "place", "wind", "indoor", "legal", "tier",
    "venue_country", "venue_city", "venue_stadium", "mark", "age",
    "orientation", "perf", "comp_name", "comp_start", "comp_tier"
  ),
  athletics_corpus = c(
    "source", "athlete_id", "event_id", "discipline", "date", "competition_id",
    "comp_name", "round", "tier", "race_key", "value_raw", "mark_string",
    "mark", "place", "is_technical", "wind", "indoor", "legal",
    "venue_country", "venue_city", "venue_stadium", "age", "sex",
    "orientation", "perf", "nomark_observable", "scoreable"
  ),
  athletics_history = c(
    "event_id", "athlete_id", "date", "sport", "discipline", "competition",
    "competition_id", "value_raw", "mark_string", "is_technical", "place",
    "round", "wind", "indoor", "legal", "tier", "venue_country", "venue_city",
    "venue_stadium", "result_score", "mark", "age", "sex", "orientation",
    "perf", "race_key", "implausible"
  )
)

#' The dedup key each store table merges on
#'
#' All three merge at competition granularity -- see duckdb_store.R for why
#' (a half-merged field corrupts the shared race effect). `athletics_corpus`
#' is rebuilt whole every run (`mode = "replace"`) so it has no merge key in
#' practice, but the entry is kept for symmetry and in case a future caller
#' merges into it directly.
#'
#' @keywords internal
CITIUS_DB_DEDUP_KEY <- list(
  championship_results = "competition_id",
  athletics_corpus = "competition_id",
  athletics_history = "competition_id"
)
