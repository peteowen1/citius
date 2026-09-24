# sigma_k=: the per-event sigma scale a caller computes once and passes back.
#
# The claim is that passing k back with only= reproduces the unhoisted
# per-event run exactly. It was false for THIN events (fewer than 20
# well-observed athletes) until 2026-09-24: the hoist drops the population the
# pooled k is computed from, so a thin event the caller's table omitted fell
# back to k = 1 -- sigma ~15-25% too small, silently.

k_history <- function() {
  thick <- synthetic_history(n_athletes = 25, n_each = 15, sigma = 0.02,
                             event_id = "AT-100Metres-M", seed = 1)
  thin  <- synthetic_history(n_athletes = 6, n_each = 12, sigma = 0.02,
                             event_id = "AT-200Metres-M", seed = 2)
  thin[, athlete_id := paste0("t", athlete_id)]
  rbind(thick, thin)
}

by_event <- function(expr) {
  old <- Sys.getenv("CITIUS_SIGMA_K_BY_EVENT", NA_character_)
  Sys.setenv(CITIUS_SIGMA_K_BY_EVENT = "1")
  on.exit(if (is.na(old)) Sys.unsetenv("CITIUS_SIGMA_K_BY_EVENT")
          else Sys.setenv(CITIUS_SIGMA_K_BY_EVENT = old))
  expr
}

# What backtest_athletics.R builds: each event's own k where it has >= 20
# well-observed athletes, the population's pooled k for the rest.
caller_k_table <- function(ab) {
  ref <- ab[n >= 10L & is.finite(sigma_rob) & sigma_rob > 0 &
              is.finite(sigma_raw) & sigma_raw > 0]
  k_pool <- if (nrow(ref) >= 20L) stats::median(ref$sigma_raw / ref$sigma_rob) else 1
  kt <- ref[, .(k_ev = stats::median(sigma_raw / sigma_rob), n_ref = .N), by = event_id]
  kt[n_ref < 20L, k_ev := k_pool]
  missing <- setdiff(unique(ab$event_id), kt$event_id)
  rbind(kt[, .(event_id, k_ev)], data.table::data.table(event_id = missing, k_ev = rep(k_pool, length(missing))))
}

test_that("passing k back with only= reproduces the per-event run, thin events included", {
  h <- k_history()
  full <- by_event(estimate_ability(h, adjust_context = FALSE))
  kt <- caller_k_table(full)
  # the setup must actually exercise both branches
  ref <- full[n >= 10L & is.finite(sigma_rob) & sigma_rob > 0 & is.finite(sigma_raw) & sigma_raw > 0]
  expect_gte(ref[event_id == "AT-100Metres-M", .N], 20L)
  expect_lt(ref[event_id == "AT-200Metres-M", .N], 20L)
  # ...and k must be far enough from 1 that the old k = 1 fallback would show
  expect_gt(abs(kt[event_id == "AT-200Metres-M"]$k_ev - 1), 0.01)

  ids <- c("3", "t2", "t5")
  hoisted <- estimate_ability(h, adjust_context = FALSE, only = ids, sigma_k = kt)
  want <- full[athlete_id %in% ids]
  data.table::setkey(hoisted, athlete_id, event_id); data.table::setkey(want, athlete_id, event_id)
  expect_equal(hoisted$sigma, want$sigma, tolerance = 1e-12)
  expect_true(all(is.finite(hoisted$sigma)))
})

test_that("a k table with a gap is an error on the only= path, not a silent k = 1", {
  h <- k_history()
  full <- by_event(estimate_ability(h, adjust_context = FALSE))
  kt <- caller_k_table(full)[event_id != "AT-200Metres-M"]
  expect_error(estimate_ability(h, adjust_context = FALSE, only = c("3", "t2"), sigma_k = kt),
               "AT-200Metres-M")
  # without only= the population is present, so the pooled fallback is the real one
  expect_no_error(estimate_ability(h, adjust_context = FALSE, sigma_k = kt))
})
