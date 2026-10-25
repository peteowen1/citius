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

test_that("the GForce sweep reproduces the original expression-in-mean sweep", {
  # decompose_races() once took `mean(perf - c_r)` by group, which data.table
  # cannot optimise (one R call per group: ~43 min per base calibrate on the
  # corpus). The sweep now means a scratch column instead. This pins the
  # rewrite to the original arithmetic, including races below min_race_size,
  # two events per athlete and a run that stops short of convergence.
  old_sweep <- function(d, max_iter, tol, min_race_size, centre) {
    dt <- data.table::copy(d)
    dt[, n_in_race := .N, by = race_key]
    dt[, shared := n_in_race >= min_race_size]
    dt[, c_r := 0]
    converged <- FALSE; i <- 0L
    for (i in seq_len(max_iter)) {
      dt[, a_i := mean(perf - c_r), by = .(athlete_id, event_id)]
      new_c <- dt[shared == TRUE, .(c_new = mean(perf - a_i)), by = race_key]
      if (!nrow(new_c)) { converged <- TRUE; break }
      if (identical(centre, "always") || all(dt$shared)) {
        new_c[, c_new := c_new - mean(c_new)]
      }
      dt[, c_new := 0]
      dt[new_c, on = "race_key", c_new := i.c_new]
      delta <- max(abs(dt$c_new - dt$c_r), na.rm = TRUE)
      dt[, c_r := c_new]
      if (is.finite(delta) && delta < tol) { converged <- TRUE; break }
    }
    dt[, resid := perf - a_i - c_r]
    list(data = dt, converged = converged, sweeps = i)
  }
  set.seed(7)
  n <- 3000
  d <- data.table::data.table(
    athlete_id = paste0("A", sample(1:400, n, replace = TRUE)),
    race_key = paste0("R", sample(1:700, n, replace = TRUE)),
    event_id = sample(c("E1", "E2"), n, replace = TRUE))
  d[, perf := stats::rnorm(.N) + (event_id == "E2")]
  for (centre in c("always", "auto")) {
    for (mi in c(3L, 400L)) {
      ref <- old_sweep(d, mi, 1e-8, 2L, centre)
      new <- suppressWarnings(
        decompose_races(d, max_iter = mi, tol = 1e-8, centre = centre))
      expect_identical(new$converged, ref$converged)
      expect_identical(new$sweeps, ref$sweeps)
      # Same row order: both keep the input order of the filtered rows.
      expect_equal(new$data$a_i, ref$data$a_i, tolerance = 1e-12)
      expect_equal(new$data$c_r, ref$data$c_r, tolerance = 1e-12)
      expect_equal(new$data$resid, ref$data$resid, tolerance = 1e-12)
      expect_false(any(c("d_", "c_new", "ae_id", "rk_id") %in% names(new$data)))
    }
  }
})
