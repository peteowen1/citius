# Regression tests for defects found in the pre-merge package review, 2026-08-03.
# Each of these shipped, ran without erroring, and produced a wrong number. Each
# existing test suite passed throughout — the notes below say why, because that
# is the part worth not repeating.

test_that(".crs_sex assigns by position, not by dropping non-matches", {
  # `regmatches(x, regexpr(...))` returns ONLY the matching elements, so the
  # result was shorter than the input and `rep_len` recycled it from the start.
  # Routes M, W, <none>, W, M came back M, W, W, M, M — three of five wrong,
  # and every one of them a plausible value rather than an NA.
  routes <- c("/SW/M/100FR", "/SW/W/200FR", "/SW/NOSEX/PARA",
              "/SW/W/50BK", "/SW/M/400FR")
  expect_equal(citius:::.crs_sex(routes), c("M", "W", NA, "W", "M"))

  # The all-matching case must be unchanged — that is why this went unnoticed.
  expect_equal(citius:::.crs_sex(routes[-3]), c("M", "W", "W", "M"))

  # Mixed relays carry no single sex.
  expect_equal(citius:::.crs_sex("/SW/X/4x100MED"), NA_character_)
  expect_equal(citius:::.crs_sex(character(0)), character(0))
})

test_that("fit_wind_effect recovers the slope when athletes differ in mean wind", {
  # The old code centred only `perf` and regressed on RAW wind. That is not the
  # within estimator: it recovers b * Var(w - wbar_i) / Var(w), attenuated by
  # between-athlete variation in mean wind exposure. The existing fixture drew
  # wind from one distribution for every athlete, so Var(wbar_i) was ~0 and the
  # bug was invisible. Here each athlete has a distinct mean wind — which is the
  # real situation, since athletes cluster by circuit and venues differ.
  set.seed(11)
  beta_true <- -0.004
  ath <- paste0("a", 1:12)
  centres <- seq(-2.5, 2.5, length.out = 12)   # systematic between-athlete shift
  d <- data.table::rbindlist(lapply(seq_along(ath), function(i) {
    w <- centres[i] + stats::rnorm(25, 0, 0.6)
    data.table::data.table(
      athlete_id = ath[i], event_id = "AT-100Metres-M", wind = w,
      perf = 2.3 + stats::rnorm(1, 0, 0.05) + beta_true * w +
             stats::rnorm(25, 0, 0.004))
  }))
  got <- fit_wind_effect(d, min_n = 30)
  expect_equal(nrow(got), 1L)
  # Within a factor of ~1.5 of truth. The pre-fix estimator attenuated this by
  # roughly an order of magnitude on the same data.
  expect_true(abs(got$beta[1] - beta_true) < 0.4 * abs(beta_true))
})

test_that("an unmatched event yields NA perf rather than a wrong-signed one", {
  # to_perf() is safe on an NA orientation — it produces NA. The adapters were
  # substituting -1L (time-event) first, so an unmatched FIELD event got a
  # perf computed with the wrong sign instead of being marked unknown.
  expect_true(is.na(to_perf(7.2, NA_integer_)))
  expect_equal(to_perf(7.2, 1L), log(7.2))
  expect_equal(to_perf(9.58, -1L), -log(9.58))
  # Vectorised, mixed.
  got <- to_perf(c(9.58, 7.2, 8.0), c(-1L, 1L, NA_integer_))
  expect_equal(got[1], -log(9.58))
  expect_equal(got[2], log(7.2))
  expect_true(is.na(got[3]))
})

test_that("condition_prior with no field is a true no-op", {
  # The default treated "every athlete in `ability`" as the field. That is only
  # a no-op when `ability` spans the population prior_mu came from — and
  # estimate_ability(only = entrants) deliberately returns entrants while
  # keeping the POPULATION prior_mu, so the default silently applied exactly the
  # field conditioning the caller declined to ask for.
  ab <- data.table::data.table(
    athlete_id = c("a", "b", "c"),
    event_id = "AT-100Metres-M",
    ability_raw = c(-2.30, -2.32, -2.34),
    ability = c(-2.30, -2.32, -2.34),
    shrinkage = c(0.4, 0.4, 0.4),
    prior_mu = -2.20)                       # population mean, not these three
  out <- condition_prior(ab)
  expect_equal(out$ability, ab$ability)
  expect_equal(out$prior_mu, ab$prior_mu)

  # Passing a field explicitly must still shift, or the function does nothing.
  out2 <- condition_prior(ab, field = c("a", "b", "c"))
  expect_false(isTRUE(all.equal(out2$ability, ab$ability)))
})

test_that("pooled context offsets fall back when no reference round exists", {
  # `eff - eff[round_class == "final"][1]` on data with no final subtracts NA
  # from every row, and the guarded fallback then ran max() on an all-NA column.
  # Result: every pooled offset NA, which the caller reads as "no adjustment" —
  # the opposite of the intended reference-to-the-slowest-context behaviour.
  set.seed(3)
  d <- data.table::data.table(
    athlete_id = rep(paste0("a", 1:8), each = 4),
    event_id = "AT-100Metres-M",
    round = rep(c("Round 1 - Heat", "Semifinal - Heat"), 16),
    perf = -log(10 + stats::rnorm(32, 0, 0.05)),
    date = as.Date("2024-01-01"))
  ctx <- estimate_context_effects(d)
  # Returns a NAMED NUMERIC VECTOR per context, not a table.
  expect_true(is.numeric(ctx$round))
  expect_true(length(ctx$round) >= 2L)
  expect_false(all(is.na(ctx$round)))       # was all-NA before the fix
  expect_true(all(is.finite(ctx$round)))
  # Referenced to the fallback (the slowest context), so one entry is exactly 0.
  expect_true(any(abs(ctx$round) < 1e-12))
})
