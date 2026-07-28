test_that("decompose_races reports how far it got", {
  set.seed(1)
  n_ath <- 30; n_race <- 40
  d <- data.table::CJ(a = seq_len(n_ath), r = seq_len(n_race))[
    sample(.N, 600)]
  d[, `:=`(athlete_id = paste0("A", a), race_key = paste0("R", r),
           event_id = "E1", perf = stats::rnorm(.N))]
  dec <- decompose_races(d[, .(athlete_id, race_key, event_id, perf)])
  # The flag existed from the start and no caller read it, so it silently
  # reported FALSE on real data while the numbers were quoted as measured.
  expect_true(all(c("converged", "delta", "sweeps") %in% names(dec)))
  expect_true(is.numeric(dec$delta))
  expect_true(dec$sweeps >= 1L)
})

test_that("a non-converged decomposition warns rather than passing silently", {
  set.seed(2)
  d <- data.table::data.table(
    athlete_id = paste0("A", rep(1:60, each = 3)),
    race_key = paste0("R", sample(1:80, 180, replace = TRUE)),
    event_id = "E1", perf = stats::rnorm(180))
  # One sweep cannot converge, so the warning must fire.
  expect_warning(decompose_races(d, max_iter = 1L), "did not converge")
})
