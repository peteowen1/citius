#' Summarize Single-Nation Gold Dominance Adjusted for GDP and Population
#'
#' Evaluates historical multi-sport gold dominance relative to a nation's share of
#' global / competing population and nominal GDP (from World Bank Open Data & Historical Statistics).
#' Supports both linear scale and sub-linear power-law (log/sqrt) scale demographic/economic shares.
#' Dynamically positions relevant ranking metrics at the front of the output table.
#'
#' Empirical Default Elasticities (from econometric regressions across 149 Games):
#'   - Population Elasticity: `gamma = 0.35`
#'   - GDP Elasticity: `gamma = 0.45`
#'
#' @param games Optional character vector filtering by games.
#' @param top_n Number of top performances to return. Defaults to 20.
#' @param min_golds Minimum golds required to qualify. Defaults to 5.
#' @param rank_by Metric to rank by: `"excess_gdp"`, `"excess_pop"`, `"multiplier_gdp"`, `"multiplier_pop"`,
#'   `"logit_gdp"`, `"logit_pop"`, `"excess_log_pop"`, `"excess_log_gdp"`, `"multiplier_log_pop"`, `"multiplier_log_gdp"`,
#'   `"logit_log_pop"`, or `"logit_log_gdp"`.
#' @param gamma Optional elasticity exponent. Defaults to `0.35` for population and `0.45` for GDP.
#' @return A `data.table` of economic-adjusted gold dominance rankings with method-specific column ordering.
#' @export
summary_games_economic_dominance <- function(games = NULL, top_n = 20L, min_golds = 5L, rank_by = "excess_gdp", gamma = NULL) {
  if (is.null(gamma)) {
    if (grepl("gdp", rank_by)) {
      gamma <- 0.45
    } else {
      gamma <- 0.35
    }
  }
  
  medals_dt <- get_games_medals(games = games)
  medals_dt <- medals_dt[gold >= min_golds]
  
  econ_path <- system.file("extdata", "country_economic_history.rds", package = "citius")
  if (!file.exists(econ_path) || file.info(econ_path)$size == 0) {
    # Development fallback, configurable rather than hardcoded to one machine.
    # The packaged copy under inst/extdata is the normal path and is now
    # committed, so this should only fire in a working tree that has not run
    # the harvest yet.
    local_path <- getOption("citius.data_dir",
                            "C:/dev/citiusverse/citiusdata/data")
    local_path <- file.path(local_path, "country_economic_history.rds")
    if (file.exists(local_path)) {
      econ_path <- local_path
    } else {
      cli::cli_abort(c(
        "Country economic history dataset not found.",
        "i" = "Run {.file citiusdata/scripts/harvest_gdp_population.R}, or set
               {.code options(citius.data_dir=)} to the directory holding it."))
    }
  }
  
  econ_dt <- data.table::as.data.table(readRDS(econ_path))
  
  # Resolve nations through the explicit crosswalk. The code this replaces fell
  # back to `substr(toupper(name), 1, 3)` for anything unmapped, which handed 37
  # ISO3 codes to more than one nation -- Austria and Australasia both landed on
  # "AUS" and were scored against Australia's GDP; Chile and Chinese Taipei both
  # on "CHI"; seven nations shared "NOR". It also mapped all four UK home
  # nations to GBR, so a Commonwealth denominator counted the UK four times.
  medals_dt[, clean_country := canonical_nation(nation)]
  medals_dt[, iso3 := nation_iso3(clean_country)]

  # One more chance for anything the map is silent on: an exact match against
  # the World Bank's own country label. Teams that are not economies keep NA.
  lookup <- unique(econ_dt[, .(country_name, iso3)])
  fillable <- is.na(medals_dt$iso3) &
    !(medals_dt$clean_country %in% non_economy_teams())
  medals_dt[fillable,
            iso3 := lookup$iso3[match(clean_country, lookup$country_name)]]

  unresolved <- medals_dt[is.na(iso3) & !(clean_country %in% non_economy_teams()),
                          sort(unique(clean_country))]
  if (length(unresolved)) {
    cli::cli_warn(c(
      "{length(unresolved)} nation{?s} have no economic match and are dropped from shares:",
      "*" = "{.val {utils::head(unresolved, 12)}}"
    ))
  }

  # World Bank data starts in 1960, so earlier editions are compared against
  # 1960 figures. That understates how much poorer the world was before then --
  # treat any pre-1960 economic ranking as indicative only.
  medals_dt[, econ_year := data.table::fifelse(year < 1960, 1960L, year)]

  merged <- merge(medals_dt,
                  econ_dt[, .(iso3, econ_year = year, population, gdp_usd)],
                  by = c("iso3", "econ_year"), all.x = TRUE)

  # Sum the denominator over DISTINCT economies, not over rows.
  denom <- unique(merged[!is.na(iso3), .(games, year, iso3, population, gdp_usd)])
  denom <- denom[, .(tot_comp_pop = sum(as.numeric(population), na.rm = TRUE),
                     tot_comp_gdp = sum(as.numeric(gdp_usd), na.rm = TRUE)),
                 by = .(games, year)]
  merged <- merge(merged, denom, by = c("games", "year"), all.x = TRUE)
  
  merged[, act_gold_pct := (as.numeric(gold) / total_golds_in_games) * 100.0]
  merged[, pop_share_pct := (as.numeric(population) / tot_comp_pop) * 100.0]
  merged[, gdp_share_pct := (as.numeric(gdp_usd) / tot_comp_gdp) * 100.0]
  
  merged[, excess_gold_vs_pop := act_gold_pct - pop_share_pct]
  merged[, excess_gold_vs_gdp := act_gold_pct - gdp_share_pct]
  merged[, pop_multiplier := act_gold_pct / pop_share_pct]
  merged[, gdp_multiplier := act_gold_pct / gdp_share_pct]
  
  # Sub-linear power-law elasticity scaling. The denominator is summed over
  # DISTINCT economies for the same reason as the linear one above: summing
  # over rows counts the UK once per home nation.
  merged[, pop_pow := (as.numeric(population))^gamma]
  merged[, gdp_pow := (as.numeric(gdp_usd))^gamma]
  denom_pow <- unique(merged[!is.na(iso3), .(games, year, iso3, pop_pow, gdp_pow)])
  denom_pow <- denom_pow[, .(tot_comp_pop_pow = sum(pop_pow, na.rm = TRUE),
                             tot_comp_gdp_pow = sum(gdp_pow, na.rm = TRUE)),
                         by = .(games, year)]
  merged <- merge(merged, denom_pow, by = c("games", "year"), all.x = TRUE)
  
  merged[, log_pop_share_pct := (pop_pow / tot_comp_pop_pow) * 100.0]
  merged[, log_gdp_share_pct := (gdp_pow / tot_comp_gdp_pow) * 100.0]
  
  merged[, excess_log_pop := act_gold_pct - log_pop_share_pct]
  merged[, excess_log_gdp := act_gold_pct - log_gdp_share_pct]
  merged[, mult_log_pop := act_gold_pct / log_pop_share_pct]
  merged[, mult_log_gdp := act_gold_pct / log_gdp_share_pct]
  
  # Logistic Logit Shifts
  merged[, p_act := as.numeric(gold) / total_golds_in_games]
  merged[p_act >= 0.999, p_act := 0.999]
  merged[, p_pop := pop_share_pct / 100.0]
  merged[, p_gdp := gdp_share_pct / 100.0]
  merged[, p_log_pop := log_pop_share_pct / 100.0]
  merged[, p_log_gdp := log_gdp_share_pct / 100.0]
  
  merged[p_pop <= 0.00001, p_pop := 0.00001]
  merged[p_pop >= 0.99999, p_pop := 0.99999]
  merged[p_gdp <= 0.00001, p_gdp := 0.00001]
  merged[p_gdp >= 0.99999, p_gdp := 0.99999]
  merged[p_log_pop <= 0.00001, p_log_pop := 0.00001]
  merged[p_log_pop >= 0.99999, p_log_pop := 0.99999]
  merged[p_log_gdp <= 0.00001, p_log_gdp := 0.00001]
  merged[p_log_gdp >= 0.99999, p_log_gdp := 0.99999]
  
  logit_fun <- function(p) log(p / (1.0 - p))
  
  merged[, logit_act := logit_fun(p_act)]
  merged[, logit_pop := logit_fun(p_pop)]
  merged[, logit_gdp := logit_fun(p_gdp)]
  merged[, logit_lpop := logit_fun(p_log_pop)]
  merged[, logit_lgdp := logit_fun(p_log_gdp)]
  
  merged[, logit_shift_pop := logit_act - logit_pop]
  merged[, logit_shift_gdp := logit_act - logit_gdp]
  merged[, logit_shift_log_pop := logit_act - logit_lpop]
  merged[, logit_shift_log_gdp := logit_act - logit_lgdp]
  
  merged[, odds_ratio_pop := exp(logit_shift_pop)]
  merged[, odds_ratio_gdp := exp(logit_shift_gdp)]
  merged[, odds_ratio_log_pop := exp(logit_shift_log_pop)]
  merged[, odds_ratio_log_gdp := exp(logit_shift_log_gdp)]
  
  if (rank_by == "excess_gdp") {
    data.table::setorder(merged, -excess_gold_vs_gdp, -gold, na.last = TRUE)
  } else if (rank_by == "excess_pop") {
    data.table::setorder(merged, -excess_gold_vs_pop, -gold, na.last = TRUE)
  } else if (rank_by == "multiplier_gdp") {
    data.table::setorder(merged, -gdp_multiplier, -gold, na.last = TRUE)
  } else if (rank_by == "multiplier_pop") {
    data.table::setorder(merged, -pop_multiplier, -gold, na.last = TRUE)
  } else if (rank_by == "logit_gdp") {
    data.table::setorder(merged, -logit_shift_gdp, -gold, na.last = TRUE)
  } else if (rank_by == "logit_pop") {
    data.table::setorder(merged, -logit_shift_pop, -gold, na.last = TRUE)
  } else if (rank_by == "excess_log_pop") {
    data.table::setorder(merged, -excess_log_pop, -gold, na.last = TRUE)
  } else if (rank_by == "excess_log_gdp") {
    data.table::setorder(merged, -excess_log_gdp, -gold, na.last = TRUE)
  } else if (rank_by == "multiplier_log_pop") {
    data.table::setorder(merged, -mult_log_pop, -gold, na.last = TRUE)
  } else if (rank_by == "multiplier_log_gdp") {
    data.table::setorder(merged, -mult_log_gdp, -gold, na.last = TRUE)
  } else if (rank_by == "logit_log_pop") {
    data.table::setorder(merged, -logit_shift_log_pop, -gold, na.last = TRUE)
  } else if (rank_by == "logit_log_gdp") {
    data.table::setorder(merged, -logit_shift_log_gdp, -gold, na.last = TRUE)
  } else {
    cli::cli_abort("Unknown rank_by option. Choose from: 'excess_gdp', 'excess_pop', 'multiplier_gdp', 'multiplier_pop', 'logit_gdp', 'logit_pop', 'excess_log_pop', 'excess_log_gdp', 'multiplier_log_pop', 'multiplier_log_gdp', 'logit_log_pop', or 'logit_log_gdp'.")
  }
  
  # Format numeric fields into display strings
  merged[, golds := gold]
  merged[, total_event_golds := total_golds_in_games]
  merged[, fmt_actual_gold_pct := paste0(round(act_gold_pct, 1), "%")]
  merged[, fmt_pop_share := data.table::fifelse(!is.na(pop_share_pct), paste0(round(pop_share_pct, 1), "%"), "N/A")]
  merged[, fmt_gdp_share := data.table::fifelse(!is.na(gdp_share_pct), paste0(round(gdp_share_pct, 1), "%"), "N/A")]
  merged[, fmt_exp_log_pop_share := data.table::fifelse(!is.na(log_pop_share_pct), paste0(round(log_pop_share_pct, 1), "%"), "N/A")]
  merged[, fmt_exp_log_gdp_share := data.table::fifelse(!is.na(log_gdp_share_pct), paste0(round(log_gdp_share_pct, 1), "%"), "N/A")]
  merged[, fmt_excess_vs_pop := data.table::fifelse(!is.na(excess_gold_vs_pop), paste0(round(excess_gold_vs_pop, 1), "%"), "N/A")]
  merged[, fmt_excess_vs_gdp := data.table::fifelse(!is.na(excess_gold_vs_gdp), paste0(round(excess_gold_vs_gdp, 1), "%"), "N/A")]
  merged[, fmt_excess_vs_log_pop := data.table::fifelse(!is.na(excess_log_pop), paste0(round(excess_log_pop, 1), "%"), "N/A")]
  merged[, fmt_excess_vs_log_gdp := data.table::fifelse(!is.na(excess_log_gdp), paste0(round(excess_log_gdp, 1), "%"), "N/A")]
  merged[, fmt_pop_multiplier := data.table::fifelse(!is.na(pop_multiplier), paste0(round(pop_multiplier, 1), "x"), "N/A")]
  merged[, fmt_gdp_multiplier := data.table::fifelse(!is.na(gdp_multiplier), paste0(round(gdp_multiplier, 1), "x"), "N/A")]
  merged[, fmt_mult_log_pop := data.table::fifelse(!is.na(mult_log_pop), paste0(round(mult_log_pop, 1), "x"), "N/A")]
  merged[, fmt_mult_log_gdp := data.table::fifelse(!is.na(mult_log_gdp), paste0(round(mult_log_gdp, 1), "x"), "N/A")]
  merged[, fmt_odds_ratio_pop := data.table::fifelse(!is.na(odds_ratio_pop), paste0(round(odds_ratio_pop, 1), "x"), "N/A")]
  merged[, fmt_odds_ratio_gdp := data.table::fifelse(!is.na(odds_ratio_gdp), paste0(round(odds_ratio_gdp, 1), "x"), "N/A")]
  merged[, fmt_odds_ratio_log_pop := data.table::fifelse(!is.na(odds_ratio_log_pop), paste0(round(odds_ratio_log_pop, 1), "x"), "N/A")]
  merged[, fmt_odds_ratio_log_gdp := data.table::fifelse(!is.na(odds_ratio_log_gdp), paste0(round(odds_ratio_log_gdp, 1), "x"), "N/A")]
  
  # Select base front columns
  base_cols <- c("games", "year", "country", "host", "competing_nations", "golds", "total_event_golds", "actual_gold_pct")
  merged[, actual_gold_pct := fmt_actual_gold_pct]
  merged[, country := nation]
  merged[, host := data.table::fifelse(is.na(host) | host == "", "Unknown", host)]
  
  # Method-specific front columns
  if (rank_by == "excess_pop") {
    method_cols <- c("pop_share", "excess_vs_pop", "pop_multiplier")
    merged[, pop_share := fmt_pop_share]; merged[, excess_vs_pop := fmt_excess_vs_pop]; merged[, pop_multiplier := fmt_pop_multiplier]
  } else if (rank_by == "excess_gdp") {
    method_cols <- c("gdp_share", "excess_vs_gdp", "gdp_multiplier")
    merged[, gdp_share := fmt_gdp_share]; merged[, excess_vs_gdp := fmt_excess_vs_gdp]; merged[, gdp_multiplier := fmt_gdp_multiplier]
  } else if (rank_by == "multiplier_pop") {
    method_cols <- c("pop_share", "pop_multiplier", "excess_vs_pop")
    merged[, pop_share := fmt_pop_share]; merged[, pop_multiplier := fmt_pop_multiplier]; merged[, excess_vs_pop := fmt_excess_vs_pop]
  } else if (rank_by == "multiplier_gdp") {
    method_cols <- c("gdp_share", "gdp_multiplier", "excess_vs_gdp")
    merged[, gdp_share := fmt_gdp_share]; merged[, gdp_multiplier := fmt_gdp_multiplier]; merged[, excess_vs_gdp := fmt_excess_vs_gdp]
  } else if (rank_by == "logit_pop") {
    method_cols <- c("pop_share", "logit_shift_pop", "odds_ratio_pop")
    merged[, pop_share := fmt_pop_share]; merged[, odds_ratio_pop := fmt_odds_ratio_pop]; merged[, logit_shift_pop := round(logit_shift_pop, 2)]
  } else if (rank_by == "logit_gdp") {
    method_cols <- c("gdp_share", "logit_shift_gdp", "odds_ratio_gdp")
    merged[, gdp_share := fmt_gdp_share]; merged[, odds_ratio_gdp := fmt_odds_ratio_gdp]; merged[, logit_shift_gdp := round(logit_shift_gdp, 2)]
  } else if (rank_by == "excess_log_pop") {
    method_cols <- c("pop_share", "exp_log_pop_share", "excess_vs_log_pop", "mult_log_pop")
    merged[, pop_share := fmt_pop_share]; merged[, exp_log_pop_share := fmt_exp_log_pop_share]; merged[, excess_vs_log_pop := fmt_excess_vs_log_pop]; merged[, mult_log_pop := fmt_mult_log_pop]
  } else if (rank_by == "excess_log_gdp") {
    method_cols <- c("gdp_share", "exp_log_gdp_share", "excess_vs_log_gdp", "mult_log_gdp")
    merged[, gdp_share := fmt_gdp_share]; merged[, exp_log_gdp_share := fmt_exp_log_gdp_share]; merged[, excess_vs_log_gdp := fmt_excess_vs_log_gdp]; merged[, mult_log_gdp := fmt_mult_log_gdp]
  } else if (rank_by == "multiplier_log_pop") {
    method_cols <- c("pop_share", "exp_log_pop_share", "mult_log_pop", "excess_vs_log_pop")
    merged[, pop_share := fmt_pop_share]; merged[, exp_log_pop_share := fmt_exp_log_pop_share]; merged[, mult_log_pop := fmt_mult_log_pop]; merged[, excess_vs_log_pop := fmt_excess_vs_log_pop]
  } else if (rank_by == "multiplier_log_gdp") {
    method_cols <- c("gdp_share", "exp_log_gdp_share", "mult_log_gdp", "excess_vs_log_gdp")
    merged[, gdp_share := fmt_gdp_share]; merged[, exp_log_gdp_share := fmt_exp_log_gdp_share]; merged[, mult_log_gdp := fmt_mult_log_gdp]; merged[, excess_vs_log_gdp := fmt_excess_vs_log_gdp]
  } else if (rank_by == "logit_log_pop") {
    method_cols <- c("pop_share", "exp_log_pop_share", "logit_shift_log_pop", "odds_ratio_log_pop")
    merged[, pop_share := fmt_pop_share]; merged[, exp_log_pop_share := fmt_exp_log_pop_share]; merged[, odds_ratio_log_pop := fmt_odds_ratio_log_pop]; merged[, logit_shift_log_pop := round(logit_shift_log_pop, 2)]
  } else if (rank_by == "logit_log_gdp") {
    method_cols <- c("gdp_share", "exp_log_gdp_share", "logit_shift_log_gdp", "odds_ratio_log_gdp")
    merged[, gdp_share := fmt_gdp_share]; merged[, exp_log_gdp_share := fmt_exp_log_gdp_share]; merged[, odds_ratio_log_gdp := fmt_odds_ratio_log_gdp]; merged[, logit_shift_log_gdp := round(logit_shift_log_gdp, 2)]
  }
  
  all_cols <- c(base_cols, method_cols)
  res <- merged[, ..all_cols]
  res <- utils::head(res, top_n)

  # Carry the exclusions on the returned object, not only as a console warning.
  # The denominator is built from resolved nations alone, so a caller that
  # suppresses warnings -- a Quarto render, a cached report, a Shiny app --
  # would otherwise get shares silently shifted by dropped competitors with no
  # trace in the value it holds.
  data.table::setattr(res, "unresolved_nations", unresolved)
  data.table::setattr(res, "n_unresolved", length(unresolved))
  res
}
