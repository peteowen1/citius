# summary_games_economic_dominance() builds its output from one lookup table
# (.economic_outputs) instead of two hand-written 12-arm ladders. These pin the
# per-method column order and ranking the ladders produced, so a table edit
# that renames, reorders or mis-sorts a method fails here.

expected_method_cols <- list(
  excess_gdp         = c("gdp_share", "excess_vs_gdp", "gdp_multiplier"),
  excess_pop         = c("pop_share", "excess_vs_pop", "pop_multiplier"),
  multiplier_gdp     = c("gdp_share", "gdp_multiplier", "excess_vs_gdp"),
  multiplier_pop     = c("pop_share", "pop_multiplier", "excess_vs_pop"),
  logit_gdp          = c("gdp_share", "logit_shift_gdp", "odds_ratio_gdp"),
  logit_pop          = c("pop_share", "logit_shift_pop", "odds_ratio_pop"),
  excess_log_pop     = c("pop_share", "exp_log_pop_share", "excess_vs_log_pop", "mult_log_pop"),
  excess_log_gdp     = c("gdp_share", "exp_log_gdp_share", "excess_vs_log_gdp", "mult_log_gdp"),
  multiplier_log_pop = c("pop_share", "exp_log_pop_share", "mult_log_pop", "excess_vs_log_pop"),
  multiplier_log_gdp = c("gdp_share", "exp_log_gdp_share", "mult_log_gdp", "excess_vs_log_gdp"),
  logit_log_pop      = c("pop_share", "exp_log_pop_share", "logit_shift_log_pop", "odds_ratio_log_pop"),
  logit_log_gdp      = c("gdp_share", "exp_log_gdp_share", "logit_shift_log_gdp", "odds_ratio_log_gdp")
)
base_cols <- c("games", "year", "country", "host", "competing_nations",
               "golds", "total_event_golds", "actual_gold_pct")

skip_if_no_econ_data <- function() {
  skip_if(system.file("extdata", "country_economic_history.rds", package = "citius") == "",
          "packaged economic history not available")
}

# The ranked value, read back from the column that displays it.
ranked_value <- function(res, rank_by) {
  col <- expected_method_cols[[rank_by]][if (grepl("_log_", rank_by)) 3 else 2]
  x <- res[[col]]
  if (is.character(x)) x <- suppressWarnings(as.numeric(sub("[%x]$", "", x)))
  x
}

test_that("every rank_by method is listed once, with its own output columns", {
  expect_setequal(citius:::.economic_rank_by, names(expected_method_cols))
  expect_false(anyDuplicated(citius:::.economic_rank_by) > 0)
})

test_that("each rank_by returns its documented columns, sorted by its metric", {
  skip_if_no_econ_data()
  for (rb in names(expected_method_cols)) {
    res <- suppressWarnings(summary_games_economic_dominance(rank_by = rb, top_n = 50L))
    expect_identical(names(res), c(base_cols, expected_method_cols[[rb]]), info = rb)
    expect_gt(nrow(res), 0)
    v <- ranked_value(res, rb)
    v <- v[!is.na(v)]
    expect_true(all(diff(v) <= 0), info = rb)
    expect_false(is.null(attr(res, "n_unresolved")), info = rb)
  }
})

test_that("summary_games_dominance() dispatches economic methods unchanged", {
  skip_if_no_econ_data()
  direct <- suppressWarnings(summary_games_economic_dominance(rank_by = "logit_log_gdp", top_n = 5L))
  via <- suppressWarnings(summary_games_dominance(method = "logit_log_gdp", top_n = 5L))
  expect_identical(via, direct)
})

test_that("an unknown rank_by or method aborts and lists every choice", {
  expect_error(summary_games_economic_dominance(rank_by = "bogus"),
               "Choose from: 'excess_gdp'.*or 'logit_log_gdp'")
  expect_error(summary_games_dominance(method = "bogus"),
               "Choose from: 'raw'.*'logit_nations', 'excess_gdp'.*or 'logit_log_gdp'")
})
