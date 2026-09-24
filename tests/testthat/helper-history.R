# Shared by test-ability.R, test-sigma-k.R and test-altitude-band.R.
synthetic_history <- function(n_athletes = 12, n_each = 20, sigma = 0.01,
                              event_id = "AT-100Metres-M", seed = 99) {
  set.seed(seed)
  true_ability <- to_perf(seq(9.80, 10.30, length.out = n_athletes), -1L)
  data.table::rbindlist(lapply(seq_len(n_athletes), function(i) {
    data.table::data.table(
      athlete_id = as.character(i),
      event_id = event_id,
      date = Sys.Date() - sample(1:900, n_each, replace = TRUE),
      perf = true_ability[i] + stats::rnorm(n_each, 0, sigma),
      race_code = "OW",
      round = "F"
    )
  }))
}
