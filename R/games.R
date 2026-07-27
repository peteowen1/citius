#' Commonwealth Games Association codes
#'
#' The 74 CGAs eligible for the Commonwealth Games, as three-letter codes
#' matching World Athletics' `country` field.
#'
#' One mismatch matters and cannot be resolved from World Athletics data alone:
#' the Commonwealth Games splits the United Kingdom into England, Scotland,
#' Wales and Northern Ireland, whereas World Athletics records every British
#' athlete as `GBR`. `GBR` is therefore treated as eligible. This is correct for
#' *eligibility* — every GBR athlete competes for one of the home nations — but
#' it means a projected field cannot say which home nation an athlete will
#' represent, and home-nation medal splits cannot be derived from it.
#'
#' @return Character vector of country codes.
#' @export
commonwealth_nations <- function() {
  c(
    # Home nations, recorded as GBR by World Athletics
    "GBR", "ENG", "SCO", "WAL", "NIR",
    # Africa
    "RSA", "KEN", "NGR", "UGA", "GHA", "TAN", "ZAM", "ZIM", "BOT", "NAM",
    "MAW", "MOZ", "LES", "ESW", "SWZ", "RWA", "CMR", "GAM", "SLE", "SEY",
    "MRI", "NGA",
    # Americas
    "CAN", "JAM", "TTO", "BAH", "BAR", "GUY", "BIZ", "BER", "CAY", "IVB",
    "TCA", "MSR", "AIA", "ANT", "DMA", "GRN", "LCA", "SKN", "VIN", "FLK",
    # Asia
    "IND", "PAK", "BAN", "SRI", "MAS", "SGP", "BRU", "MDV",
    # Europe
    "CYP", "MLT", "GIB", "IOM", "JEY", "GGY", "SHN",
    # Oceania
    "AUS", "NZL", "FIJ", "PNG", "SAM", "TGA", "VAN", "SOL", "COK", "NRU",
    "KIR", "TUV", "NFI", "NIU"
  )
}


#' Build a projected field for an event
#'
#' Selects the entrants most likely to contest an event, given an ability table
#' and an eligibility rule. Used when official start lists are unavailable —
#' which is the normal situation more than a few days out from a Games, and was
#' the situation for Glasgow 2026 while it was under way.
#'
#' A projected field is **not** an entry list. Athletes qualify, withdraw, are
#' not selected, or are entered in a different event, and none of that is
#' visible here. Predictions built on one are a genuine forward test of the
#' *model*, but they are not a forecast of the actual race. Label them as such.
#'
#' @param ability A `data.table` from [estimate_ability()].
#' @param event Canonical `event_id` to build a field for.
#' @param nations Optional vector of eligible country codes; `NULL` for no
#'   eligibility filter. See [commonwealth_nations()].
#' @param athlete_countries Optional two-column table (`athlete_id`, `country`)
#'   supplying nationality, which result feeds do not carry.
#' @param size Number of entrants to select.
#' @param min_results Minimum results required for an athlete to be considered,
#'   guarding against a single fluke mark dominating a projected field.
#' @param as_of Reference date for staleness. Defaults to today.
#' @param max_stale_years Optional hard cutoff on how old an athlete's most
#'   recent result may be. Defaults to `Inf` — **off** — because
#'   [estimate_ability()] already shrinks on total weight, so a decade-old
#'   record carries almost no evidence and regresses to the event mean on its
#'   own. Prefer that to a cutoff: the decay is fitted from data by
#'   [fit_half_life()], whereas any cutoff here is a guess. Set a value only
#'   when modelling entry eligibility rather than ability.
#' @return A `data.table` subset of `ability`, best first.
#' @export
project_field <- function(ability, event, nations = NULL,
                          athlete_countries = NULL, size = 8L,
                          min_results = 3L, as_of = Sys.Date(),
                          max_stale_years = Inf) {
  ab <- data.table::as.data.table(ability)[event_id == event]
  if (!nrow(ab)) return(ab)

  if ("n" %in% names(ab)) ab <- ab[n >= min_results]
  if (is.finite(max_stale_years) && "last_date" %in% names(ab)) {
    ab <- ab[is.na(last_date) |
               as.numeric(as.Date(as_of) - last_date) / 365.25 <= max_stale_years]
  }
  if (!nrow(ab)) return(ab)

  if (!is.null(nations)) {
    if (is.null(athlete_countries)) {
      cli::cli_abort("{.arg athlete_countries} is required when {.arg nations} is supplied.")
    }
    ac <- data.table::as.data.table(athlete_countries)
    ac[, athlete_id := as.character(athlete_id)]
    ab[, athlete_id := as.character(athlete_id)]
    ab <- merge(ab, ac[, .(athlete_id, country)], by = "athlete_id", all.x = TRUE)
    ab <- ab[country %in% nations]
  }

  if (!nrow(ab)) return(ab)
  data.table::setorder(ab, -ability)
  utils::head(ab, size)[]
}
