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


#' Multi-Sport Regional and Global Games Taxonomy
#'
#' Returns a reference data.table listing major multi-sport competitions worldwide,
#' including geographic scope, scale, inaugural year, and dominant powerhouses.
#'
#' @return A `data.table` with columns `games`, `name`, `scope`, `region`, `scale`, `start_year`, `powerhouses`.
#' @export
multisport_games_taxonomy <- function() {
  data.table::data.table(
    games = c("olympics_summer", "olympics_winter", "commonwealth", "asian_games",
              "panam_games", "african_games", "european_games", "pacific_games"),
    name = c("Summer Olympic Games", "Winter Olympic Games", "Commonwealth Games",
             "Asian Games", "Pan American Games", "African Games",
             "European Games", "Pacific Games"),
    scope = c("Global", "Global", "Global Commonwealth", "Continental",
              "Continental", "Continental", "Continental", "Regional"),
    region = c("Worldwide", "Worldwide", "Commonwealth of Nations", "Asia",
               "Americas", "Africa", "Europe", "Oceania / South Pacific"),
    scale = c("206 NOCs (~11,000 athletes)", "90+ NOCs (~3,000 athletes)",
              "56 Nations / 72 CGAs (~5,000 athletes)", "45 NOCs (~12,000 athletes)",
              "41 NOCs (~6,800 athletes)", "54 NOCs (~5,000 athletes)",
              "48 NOCs (~7,000 athletes)", "24 Island Nations & Territories"),
    start_year = c(1896L, 1924L, 1930L, 1951L, 1951L, 1965L, 2015L, 1963L),
    powerhouses = c("USA, China, Japan, Australia, France, GBR",
                    "Norway, USA, Germany, Austria, Canada",
                    "Australia, England, Canada, India, New Zealand, Kenya",
                    "China, Japan, South Korea, India",
                    "USA, Canada, Brazil, Cuba, Mexico",
                    "Kenya, Ethiopia, South Africa, Nigeria, Egypt",
                    "Italy, Great Britain, Germany, France, Spain",
                    "Fiji, Papua New Guinea, New Caledonia, Tahiti")
  )
}


#' Get Historical Multi-Sport Games Medal Tables
#'
#' Extracts historical medal tables for major global and continental multi-sport games
#' (Olympics, Commonwealth Games, Asian Games, PanAm Games, African Games, European Games, Pacific Games).
#'
#' @param games Optional character vector filtering games (e.g. `"commonwealth"`, `"olympics_summer"`, `"asian_games"`).
#' @param year Optional integer vector filtering competition years (e.g. `2026`, `2024`).
#' @param nation Optional character string or vector filtering by nation name / NOC.
#' @return A `data.table` containing medal counts, rank, `gold_share`, `medal_share`, and host details.
#' @export
get_games_medals <- function(games = NULL, year = NULL, nation = NULL) {
  rds_path <- system.file("extdata", "multisport_medal_tables.rds", package = "citius")
  if (!file.exists(rds_path) || file.info(rds_path)$size == 0) {
    # Development fallback, configurable rather than pinned to one machine.
    # Same pattern as summary_games_economic_dominance(); both are patched
    # together because a path duplicated across callers is how the last
    # hardcoded-path fix got half-applied.
    local_path <- file.path(getOption("citius.data_dir",
                                      "C:/dev/citiusverse/citiusdata/data"),
                            "multisport_medal_tables.rds")
    if (file.exists(local_path)) {
      rds_path <- local_path
    } else {
      cli::cli_abort(c(
        "Multi-sport medal tables dataset not found.",
        "i" = "Run {.file citiusdata/scripts/harvest_multisport_medal_tables.R}, or set
               {.code options(citius.data_dir=)} to the directory holding it."))
    }
  }
  
  dt <- data.table::as.data.table(readRDS(rds_path))
  
  if (!is.null(games)) {
    g_vec <- games
    dt <- dt[games %in% g_vec]
  }
  if (!is.null(year)) {
    yr_vec <- as.integer(year)
    dt <- dt[year %in% yr_vec]
  }
  if (!is.null(nation)) {
    nat_vec <- nation
    dt <- dt[grepl(paste(nat_vec, collapse = "|"), nation, ignore.case = TRUE)]
  }
  
  data.table::setorder(dt, games, -year, -gold, -silver, -bronze)
  dt[]
}


#' Summarize Single-Nation Multi-Sport Gold Dominance (Master Wrapper)
#'
#' Unified master wrapper function to evaluate historical multi-sport gold dominance across
#' raw gold shares, field size (competing nations), economic adjustments (GDP / Population),
#' and logistic log-odds scale metrics.
#'
#' Supported `method` parameter options:
#'   - `"raw"` (default): Rank strictly by raw actual gold percentage won (`gold_pct`).
#'   - `"excess_nations"`: Rank by excess gold % over uniform nation share (Actual % - 100% / competing_nations).
#'   - `"multiplier_nations"`: Rank by gold multiplier over uniform nation share (Actual % / expected_pct).
#'   - `"logit_nations"`: Rank by logistic log-odds shift over uniform nation share.
#'   - `"excess_gdp"`: Rank by excess gold % over GDP share (World Bank data).
#'   - `"excess_pop"`: Rank by excess gold % over Population share (World Bank data).
#'   - `"multiplier_gdp"`: Rank by economic gold efficiency multiplier (Actual % / GDP Share %).
#'   - `"multiplier_pop"`: Rank by per-capita gold efficiency multiplier (Actual % / Population Share %).
#'   - `"logit_gdp"`: Rank by logistic log-odds shift over GDP share.
#'   - `"logit_pop"`: Rank by logistic log-odds shift over Population share.
#'
#' @param games Optional character vector filtering by games (e.g. `"commonwealth"`, `"olympics_summer"`).
#' @param top_n Number of top performances to return. Defaults to 20.
#' @param min_golds Minimum golds required to qualify. Defaults to 5.
#' @param method Ranking method to use. Defaults to `"raw"`.
#' @param gamma Elasticity exponent for sub-linear log/power scaling. Defaults to empirical baseline (0.35 for pop, 0.45 for GDP).
#' @return A `data.table` of dominance rankings.
#' @export
summary_games_dominance <- function(games = NULL, top_n = 20L, min_golds = 5L, method = "raw", gamma = NULL) {
  if (method == "raw") {
    dt <- get_games_medals(games = games)
    dt <- dt[gold >= min_golds]
    data.table::setorder(dt, -gold_share, -gold)
    
    if (!"total_golds_in_games" %in% names(dt)) {
      dt[, total_golds_in_games := sum(gold, na.rm = TRUE), by = .(games, year)]
    }
    
    res <- dt[, .(
      games,
      year,
      country = nation,
      host = data.table::fifelse(is.na(host) | host == "", "Unknown", host),
      competing_nations = data.table::fifelse(!is.na(competing_nations), competing_nations, data.table::uniqueN(nation)),
      medalling_nations = data.table::fifelse(!is.na(medalling_nations), medalling_nations, data.table::uniqueN(nation)),
      golds = gold,
      total_event_golds = total_golds_in_games,
      gold_pct = paste0(round(gold_share * 100, 1), "%"),
      total_medals = total
    )]
    return(utils::head(res, top_n))
  } else if (method == "excess_nations") {
    return(summary_games_excess_gold(games = games, top_n = top_n, min_golds = min_golds))
  } else if (method == "multiplier_nations") {
    dt <- summary_games_excess_gold(games = games, top_n = 1000L, min_golds = min_golds)
    dt[, mult_num := as.numeric(gsub("x", "", gold_multiplier))]
    data.table::setorder(dt, -mult_num, -golds)
    dt[, mult_num := NULL]
    return(utils::head(dt, top_n))
  } else if (method == "logit_nations") {
    return(summary_games_logit_dominance(games = games, top_n = top_n, min_golds = min_golds))
  } else if (method %in% c("excess_gdp", "excess_pop", "multiplier_gdp", "multiplier_pop", "logit_gdp", "logit_pop",
                          "excess_log_pop", "excess_log_gdp", "multiplier_log_pop", "multiplier_log_gdp",
                          "logit_log_pop", "logit_log_gdp")) {
    return(summary_games_economic_dominance(games = games, top_n = top_n, min_golds = min_golds, rank_by = method, gamma = gamma))
  } else {
    cli::cli_abort("Unknown method '{method}'. Choose from: 'raw', 'excess_nations', 'multiplier_nations', 'logit_nations', 'excess_gdp', 'excess_pop', 'multiplier_gdp', 'multiplier_pop', 'logit_gdp', 'logit_pop', 'excess_log_pop', 'excess_log_gdp', 'multiplier_log_pop', 'multiplier_log_gdp', 'logit_log_pop', or 'logit_log_gdp'.")
  }
}


#' Summarize Excess Gold Percentage and Multiplier Over Expected Uniform Share
#'
#' Evaluates gold dominance adjusted for competing nation count, calculating:
#'   1) `expected_gold_pct` = `100% / competing_nations`
#'   2) `excess_gold_pct`   = `actual_gold_pct - expected_gold_pct`
#'   3) `gold_multiplier`   = `actual_gold_pct / expected_gold_pct`
#'
#' @param games Optional character vector filtering by games.
#' @param top_n Number of top performances to return. Defaults to 20.
#' @param min_golds Minimum golds required to qualify. Defaults to 5.
#' @return A `data.table` sorted by `excess_gold_pct` descending.
#' @export
summary_games_excess_gold <- function(games = NULL, top_n = 20L, min_golds = 5L) {
  dt <- get_games_medals(games = games)
  dt <- dt[gold >= min_golds]
  
  dt[, comp_nations := data.table::fifelse(!is.na(competing_nations) & competing_nations > 0, competing_nations, data.table::uniqueN(nation))]
  dt[, exp_gold_pct := 100.0 / comp_nations]
  dt[, act_gold_pct := (as.numeric(gold) / total_golds_in_games) * 100.0]
  dt[, excess_pct := act_gold_pct - exp_gold_pct]
  dt[, multiplier := act_gold_pct / exp_gold_pct]
  
  data.table::setorder(dt, -excess_pct, -gold)
  
  res <- dt[, .(
    games,
    year,
    country = nation,
    host = data.table::fifelse(is.na(host) | host == "", "Unknown", host),
    competing_nations = comp_nations,
    golds = gold,
    total_event_golds = total_golds_in_games,
    actual_gold_pct = paste0(round(act_gold_pct, 1), "%"),
    expected_gold_pct = paste0(round(exp_gold_pct, 2), "%"),
    excess_gold_pct = paste0(round(excess_pct, 1), "%"),
    gold_multiplier = paste0(round(multiplier, 1), "x")
  )]
  
  utils::head(res, top_n)
}


#' Summarize Single-Nation Gold Dominance by Logistic Coefficient (Logit Shift & Odds Ratio)
#'
#' Evaluates gold dominance on the logistic log-odds scale:
#'   1) `p_actual` = `golds / total_event_golds`
#'   2) `p_expected` = `1 / competing_nations`
#'   3) `logit(p)` = `log(p / (1 - p))`
#'   4) `logit_shift` = `logit(p_actual) - logit(p_expected)`
#'   5) `odds_ratio` = `exp(logit_shift)`
#'
#' @param games Optional character vector filtering by games.
#' @param top_n Number of top performances to return. Defaults to 20.
#' @param min_golds Minimum golds required to qualify. Defaults to 5.
#' @return A `data.table` sorted by `logit_shift` descending.
#' @export
summary_games_logit_dominance <- function(games = NULL, top_n = 20L, min_golds = 5L) {
  dt <- get_games_medals(games = games)
  dt <- dt[gold >= min_golds]
  
  dt[, comp_nations := data.table::fifelse(!is.na(competing_nations) & competing_nations > 0, competing_nations, data.table::uniqueN(nation))]
  
  dt[, p_act := as.numeric(gold) / total_golds_in_games]
  dt[p_act >= 0.999, p_act := 0.999]
  dt[, p_exp := 1.0 / comp_nations]
  
  logit_fun <- function(p) log(p / (1.0 - p))
  
  dt[, l_act := logit_fun(p_act)]
  dt[, l_exp := logit_fun(p_exp)]
  dt[, l_shift := l_act - l_exp]
  dt[, odds_rat := exp(l_shift)]
  
  data.table::setorder(dt, -l_shift, -gold)
  
  res <- dt[, .(
    games,
    year,
    country = nation,
    host = data.table::fifelse(is.na(host) | host == "", "Unknown", host),
    competing_nations = comp_nations,
    golds = gold,
    total_event_golds = total_golds_in_games,
    actual_gold_pct = paste0(round(p_act * 100.0, 1), "%"),
    expected_gold_pct = paste0(round(p_exp * 100.0, 2), "%"),
    logit_actual = round(l_act, 2),
    logit_expected = round(l_exp, 2),
    logit_shift = round(l_shift, 2),
    odds_ratio = paste0(round(odds_rat, 1), "x")
  )]
  
  utils::head(res, top_n)
}

