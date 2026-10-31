#' Summarize Single-Nation Gold Dominance Adjusted for GDP and Population
#'
#' Evaluates historical multi-sport gold dominance relative to a nation's share of
#' global / competing population and nominal GDP (from World Bank Open Data & Historical Statistics).
#' Supports both linear scale and sub-linear power-law (log/sqrt) scale demographic/economic shares.
#' Dynamically positions relevant ranking metrics at the front of the output table.
#'
#' Default elasticities are HAND-SET, not measured -- nothing in this package or
#' citiusdata fits them. They damp a nation's share sub-linearly so a giant is
#' not ranked purely on size:
#'   - Population: `gamma = 0.35`
#'   - GDP: `gamma = 0.45`
#'
#' An exception to the package's no-hand-tuned-constants rule, accepted because
#' this ranking is descriptive and outside the forecast model. Pass `gamma` to
#' override; fit them from the medal tables before this ranking is published.
#'
#' @param games Optional character vector filtering by games.
#' @param top_n Number of top performances to return. Defaults to 20.
#' @param min_golds Minimum golds required to qualify. Defaults to 5.
#' @param rank_by Metric to rank by: `"excess_gdp"`, `"excess_pop"`, `"multiplier_gdp"`, `"multiplier_pop"`,
#'   `"logit_gdp"`, `"logit_pop"`, `"excess_log_pop"`, `"excess_log_gdp"`, `"multiplier_log_pop"`, `"multiplier_log_gdp"`,
#'   `"logit_log_pop"`, or `"logit_log_gdp"`.
#' @param gamma Optional elasticity exponent. Defaults to the hand-set `0.35` for population and `0.45` for GDP.
#' @return A `data.table` of economic-adjusted gold dominance rankings with method-specific column ordering.
#' @export
summary_games_economic_dominance <- function(games = NULL, top_n = 20L, min_golds = 5L, rank_by = "excess_gdp", gamma = NULL) {
  if (!(length(rank_by) == 1L && rank_by %in% .economic_rank_by)) {
    choices <- .economic_choices_text(.economic_rank_by)
    cli::cli_abort("Unknown rank_by option. Choose from: {choices}.")
  }
  parts <- regmatches(rank_by, regexec("^(excess|multiplier|logit)_((log_)?(pop|gdp))$", rank_by))[[1]]
  metric <- parts[2]
  is_log <- nzchar(parts[4])
  var_col <- if (parts[5] == "pop") "population" else "gdp_usd"
  tot_col <- if (parts[5] == "pop") "tot_comp_pop" else "tot_comp_gdp"

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
    # Already overridable via options(), so lower priority than
    # duckdb_store.R's get_citius_db_path() -- but the same fragility: if
    # data/'s internal layout is ever restructured, this default (and the
    # matching one below for multisport_medal_tables.rds) needs the same
    # subpath-as-parameter treatment. Noted, not fixed, 2026-08-30.
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

  # Linear share of the chosen economic variable. It is always shown, and for
  # the log_ bases it sits beside the power-law share that is ranked on.
  merged[, lin_share_pct := (as.numeric(merged[[var_col]]) / merged[[tot_col]]) * 100.0]

  if (is_log) {
    # Sub-linear power-law elasticity scaling. The denominator is summed over
    # DISTINCT economies for the same reason as the linear one above: summing
    # over rows counts the UK once per home nation.
    merged[, var_pow := (as.numeric(merged[[var_col]]))^gamma]
    denom_pow <- unique(merged[!is.na(iso3), .(games, year, iso3, var_pow)])
    denom_pow <- denom_pow[, .(tot_comp_pow = sum(var_pow, na.rm = TRUE)),
                           by = .(games, year)]
    merged <- merge(merged, denom_pow, by = c("games", "year"), all.x = TRUE)
    merged[, exp_share_pct := (var_pow / tot_comp_pow) * 100.0]
    share_col <- "exp_share_pct"
  } else {
    share_col <- "lin_share_pct"
  }

  if (metric == "logit") {
    # Logistic logit shift: actual share clipped above, expected share clamped
    # into (0, 1) on both sides.
    merged[, p_act := as.numeric(gold) / total_golds_in_games]
    merged[p_act >= .citius_logit_clip, p_act := .citius_logit_clip]
    merged[, p_exp := merged[[share_col]] / 100.0]
    merged[p_exp <= 0.00001, p_exp := 0.00001]
    merged[p_exp >= 0.99999, p_exp := 0.99999]
    logit_fun <- function(p) log(p / (1.0 - p))
    merged[, logit_shift := logit_fun(p_act) - logit_fun(p_exp)]
    merged[, odds_ratio := exp(logit_shift)]
    sort_col <- "logit_shift"
  } else {
    merged[, excess := act_gold_pct - merged[[share_col]]]
    merged[, multiplier := act_gold_pct / merged[[share_col]]]
    sort_col <- metric
  }

  data.table::setorderv(merged, c(sort_col, "gold"), order = c(-1L, -1L),
                        na.last = TRUE)

  # Format numeric fields into display strings
  fmt <- function(x, suffix) {
    data.table::fifelse(!is.na(x), paste0(round(x, 1), suffix), "N/A")
  }

  merged[, golds := gold]
  merged[, total_event_golds := total_golds_in_games]

  # Select base front columns
  base_cols <- c("games", "year", "country", "host", "competing_nations", "golds", "total_event_golds", "actual_gold_pct")
  merged[, actual_gold_pct := paste0(round(act_gold_pct, 1), "%")]
  merged[, country := nation]
  merged[, host := data.table::fifelse(is.na(host) | host == "", "Unknown", host)]

  # Method-specific front columns, named and ordered as the lookup table says.
  out <- .economic_outputs[[rank_by]]
  method_values <- list(
    share      = fmt(merged$lin_share_pct, "%"),
    exp_share  = if (is_log) fmt(merged$exp_share_pct, "%"),
    excess     = if (metric != "logit") fmt(merged$excess, "%"),
    multiplier = if (metric != "logit") fmt(merged$multiplier, "x"),
    logit      = if (metric == "logit") round(merged$logit_shift, 2),
    odds       = if (metric == "logit") fmt(merged$odds_ratio, "x")
  )
  method_cols <- out$name
  for (i in seq_along(method_cols)) {
    data.table::set(merged, j = method_cols[i], value = method_values[[out$role[i]]])
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


# Output columns per rank_by method, one entry per method, in the order error
# messages list them: `name` is the column in the returned table, `role` the
# value it holds (see summary_games_economic_dominance()). The names are
# irregular (pop_multiplier but mult_log_pop) and are kept as they were, so
# this is the one place they are spelled out.
.economic_outputs <- list(
  excess_gdp = list(
    name = c("gdp_share", "excess_vs_gdp", "gdp_multiplier"),
    role = c("share", "excess", "multiplier")),
  excess_pop = list(
    name = c("pop_share", "excess_vs_pop", "pop_multiplier"),
    role = c("share", "excess", "multiplier")),
  multiplier_gdp = list(
    name = c("gdp_share", "gdp_multiplier", "excess_vs_gdp"),
    role = c("share", "multiplier", "excess")),
  multiplier_pop = list(
    name = c("pop_share", "pop_multiplier", "excess_vs_pop"),
    role = c("share", "multiplier", "excess")),
  logit_gdp = list(
    name = c("gdp_share", "logit_shift_gdp", "odds_ratio_gdp"),
    role = c("share", "logit", "odds")),
  logit_pop = list(
    name = c("pop_share", "logit_shift_pop", "odds_ratio_pop"),
    role = c("share", "logit", "odds")),
  excess_log_pop = list(
    name = c("pop_share", "exp_log_pop_share", "excess_vs_log_pop", "mult_log_pop"),
    role = c("share", "exp_share", "excess", "multiplier")),
  excess_log_gdp = list(
    name = c("gdp_share", "exp_log_gdp_share", "excess_vs_log_gdp", "mult_log_gdp"),
    role = c("share", "exp_share", "excess", "multiplier")),
  multiplier_log_pop = list(
    name = c("pop_share", "exp_log_pop_share", "mult_log_pop", "excess_vs_log_pop"),
    role = c("share", "exp_share", "multiplier", "excess")),
  multiplier_log_gdp = list(
    name = c("gdp_share", "exp_log_gdp_share", "mult_log_gdp", "excess_vs_log_gdp"),
    role = c("share", "exp_share", "multiplier", "excess")),
  logit_log_pop = list(
    name = c("pop_share", "exp_log_pop_share", "logit_shift_log_pop", "odds_ratio_log_pop"),
    role = c("share", "exp_share", "logit", "odds")),
  logit_log_gdp = list(
    name = c("gdp_share", "exp_log_gdp_share", "logit_shift_log_gdp", "odds_ratio_log_gdp"),
    role = c("share", "exp_share", "logit", "odds"))
)

# The twelve rank_by methods. summary_games_dominance() dispatches on this same
# vector, so the two functions cannot disagree about which methods exist.
.economic_rank_by <- names(.economic_outputs)

# "'a', 'b', or 'c'" for an error message.
.economic_choices_text <- function(x) {
  q <- paste0("'", x, "'")
  paste0(paste(q[-length(q)], collapse = ", "), ", or ", q[length(q)])
}
