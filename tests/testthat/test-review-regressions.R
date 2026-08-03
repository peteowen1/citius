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


test_that("calibrate() attaches the indoor and season offsets it fits", {
  # Both `fit_indoor_effect()` and `fit_season_effect()` were built, tested and
  # (for season) validated out of sample, and then never attached to a
  # calibration. `estimate_ability()` read `calibration$indoor` and
  # `calibration$season` the whole time, so with nothing setting them both
  # adjustment blocks were dead in every deployed run — the same shape as the
  # fitted wind coefficient that once sat unused. Fitting is covered in
  # test-context.R; what is asserted here is the WIRING.
  set.seed(11)
  # `fit_season_effect()` needs min_n = 1000 marks per family-hemisphere-month
  # cell by default, so the synthetic set has to clear that in every month.
  n_ath <- 200L
  months <- rep(c(5L, 6L, 9L, 10L), each = 6L)
  d <- data.table::CJ(athlete_id = paste0("a", seq_len(n_ath)), k = seq_along(months))
  d[, month := months[k]]
  d[, event_id := "AT-100Metres-M"]
  # Day-of-month must stay INSIDE the month: `fit_season_effect()` recomputes the
  # month from the date, so a "+ k" offset would silently relabel September marks
  # as November and the planted phase would be fitted against the wrong cells.
  d[, date := as.Date(sprintf("2021-%02d-%02d", month, 2L + ((k - 1L) %% 6L) * 4L))]
  d[, venue_country := "GBR"]
  d[, indoor := FALSE]
  d[, round := "Final"][, tier := "OW"]
  d[, race_key := paste(event_id, date)]
  # A real seasonal phase: sharp in May/June, flat in Sep/Oct.
  d[, seas := ifelse(month %in% c(5L, 6L), 0.004, -0.004)]
  d[, ability := stats::rnorm(.N, 0, 0.02), by = athlete_id]
  d[, perf := -log(10) + ability + seas + stats::rnorm(.N, 0, 0.004)]

  # Synthetic data has no missing marks, so the no-mark-rate warning is expected
  # and unrelated to what this test asserts.
  # Opted in explicitly. Both default to FALSE because the pair was measured and
  # rejected on 2026-08-04 (gold Brier +0.74% across 948 finals), but the WIRING
  # must still work -- the original defect was code reading a calibration element
  # nothing could set, and a default-off flag must not quietly restore that.
  cal <- suppressWarnings(calibrate(d, min_races = 1L, min_race_size = 1L,
                                    context_season = TRUE, context_indoor = TRUE))

  # 1. The elements exist rather than being silently NULL.
  expect_false(is.null(cal$season))
  expect_true(nrow(cal$season) > 0L)
  # `indoor` is all FALSE here, so there is no contrast to fit; the element must
  # still be attached (possibly zero-offset) rather than absent.
  expect_false(is.null(cal$indoor))

  # 2. The planted phase is recovered with the right sign.
  s <- data.table::as.data.table(cal$season)
  may <- s[month == 5L, offset][1]
  sep <- s[month == 9L, offset][1]
  expect_true(is.finite(may) && is.finite(sep))
  expect_gt(may, sep)

  # 3. And it REACHES estimate_ability() — the step that was missing. Ability
  #    estimated with the season offsets must differ from ability estimated
  #    without them, or the block is still dead.
  ab_on  <- estimate_ability(d, calibration = cal, adjust_context = TRUE)
  cal_off <- cal
  cal_off$season <- NULL
  ab_off <- estimate_ability(d, calibration = cal_off, adjust_context = TRUE)
  m <- merge(ab_on[, .(athlete_id, event_id, on = ability)],
             ab_off[, .(athlete_id, event_id, off = ability)],
             by = c("athlete_id", "event_id"))
  expect_gt(nrow(m), 0L)
  expect_false(isTRUE(all.equal(m$on, m$off)))
})

test_that("the season offset is a phase, not an intercept shift", {
  # `fit_season_effect()` centres within family-hemisphere precisely so that
  # applying it removes WHEN an athlete raced without moving the family's
  # overall level. If the offsets failed to centre, every mark in the family
  # would shift and the correction would masquerade as an ability change.
  set.seed(12)
  months <- rep(c(4L, 5L, 6L, 7L, 8L, 9L), each = 4L)
  d <- data.table::CJ(athlete_id = paste0("b", 1:50), k = seq_along(months))
  d[, month := months[k]]
  d[, event_id := "AT-100Metres-M"]
  d[, date := as.Date(sprintf("2021-%02d-10", month)) + k]
  d[, venue_country := "GBR"][, indoor := FALSE]
  d[, round := "Final"][, tier := "OW"]
  d[, race_key := paste(event_id, date)]
  d[, perf := -log(10) + stats::rnorm(.N, 0, 0.01) + 0.003 * sin(month)]

  s <- fit_season_effect(d, min_n = 20L)
  skip_if(nrow(s) == 0L, "no season cells fitted")
  # Weighted mean of the offsets within family-hemisphere is zero by construction.
  chk <- s[, .(wm = stats::weighted.mean(offset, n)), by = .(family, hemi)]
  expect_true(all(abs(chk$wm) < 1e-12))
})


test_that("season and indoor stay OFF unless explicitly asked for", {
  # The A/B on 2026-08-04 rejected the pair: gold Brier +0.74% across 948 scored
  # finals (p = 0.00019), +2.02% on majors. The wiring is kept because the
  # original defect was unreachable code, but a default calibration must not
  # apply a rejected adjustment -- otherwise the next rebaseline adopts it
  # silently and the regression arrives with no commit that caused it.
  set.seed(21)
  months <- rep(c(5L, 6L, 9L, 10L), each = 6L)
  d <- data.table::CJ(athlete_id = paste0("c", 1:200), k = seq_along(months))
  d[, month := months[k]]
  d[, event_id := "AT-100Metres-M"]
  d[, date := as.Date(sprintf("2021-%02d-%02d", month, 2L + ((k - 1L) %% 6L) * 4L))]
  d[, venue_country := "GBR"][, indoor := FALSE]
  d[, round := "Final"][, tier := "OW"]
  d[, race_key := paste(event_id, date)]
  d[, perf := -log(10) + stats::rnorm(.N, 0, 0.01)]

  cal <- suppressWarnings(calibrate(d, min_races = 1L, min_race_size = 1L))
  expect_null(cal$season)
  expect_null(cal$indoor)
})
