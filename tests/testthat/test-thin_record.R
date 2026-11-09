test_that("own-record prior keeps the shrinkage weight and swaps the target", {
  ab <- data.table::data.table(athlete_id = c("1", "2", "3"), event_id = "E",
    ability_raw = c(-5.47, -5.50, -5.60), shrinkage = c(0.27, 0.05, 0.5),
    prior_mu = -5.63)
  ab[, ability := (1 - shrinkage) * ability_raw + shrinkage * prior_mu]
  lg <- data.table::data.table(athlete_id = c("1", "2"), event_id = "E",
                               ability_raw = c(-5.44, -5.49))
  out <- suppressMessages(apply_own_record_prior(ab, lg))
  expect_equal(out$ability[1], (1 - 0.27) * -5.47 + 0.27 * -5.44)
  expect_equal(out$ability[2], (1 - 0.05) * -5.50 + 0.05 * -5.49)
  # no long-horizon row: unchanged, and said so
  expect_equal(out$ability[3], ab$ability[3])
  expect_false(out$own_prior_applied[3])
  expect_equal(out$ability_event_prior, ab$ability)
  # input not modified by reference
  expect_false("own_prior" %in% names(ab))
})

test_that("min_shrinkage limits the swap to large pulls", {
  ab <- data.table::data.table(athlete_id = c("1", "2"), event_id = "E",
    ability_raw = c(-5.47, -5.50), shrinkage = c(0.27, 0.05), prior_mu = -5.63)
  ab[, ability := (1 - shrinkage) * ability_raw + shrinkage * prior_mu]
  lg <- data.table::data.table(athlete_id = c("1", "2"), event_id = "E", ability_raw = c(-5.44, -5.49))
  out <- suppressMessages(apply_own_record_prior(ab, lg, min_shrinkage = 0.2))
  expect_equal(out$own_prior_applied, c(TRUE, FALSE))
  expect_equal(out$ability[2], ab$ability[2])
})

test_that("gap offsets are relative to recent racers and applied by bucket", {
  scored <- data.table::data.table(
    perf = c(-5.01, -5.01, -5.02, -5.03), ability = -5.00,
    gap_days = c(10, 30, 90, 400))
  off <- fit_gap_offsets(scored, min_n = 1)
  expect_equal(off[gap == "<60d", offset], 0)
  expect_equal(off[gap == "60-120d", offset], -0.01, tolerance = 1e-12)
  expect_equal(off[gap == ">365d", offset], -0.02, tolerance = 1e-12)
  ab <- data.table::data.table(ability = c(-5, -5, -5),
    last_date = as.Date(c("2025-12-20", "2025-10-01", "2024-01-01")))
  out <- suppressMessages(apply_gap_offsets(ab, as.Date("2026-01-01"), off))
  expect_equal(out$gap, c("<60d", "60-120d", ">365d"))
  expect_equal(out$ability, c(-5, -5.01, -5.02), tolerance = 1e-12)
  # a bucket with no offset row gets 0, not NA
  out2 <- suppressMessages(apply_gap_offsets(ab, as.Date("2026-01-01"), off[gap != ">365d"]))
  expect_equal(out2$ability[3], -5)
})

test_that("thin-record helpers refuse malformed input", {
  expect_error(apply_own_record_prior(data.table::data.table(x = 1), data.table::data.table()), "lacks")
  expect_error(fit_gap_offsets(data.table::data.table(perf = 1, ability = 1, gap_days = 100)), "anchor")
  expect_error(apply_gap_offsets(data.table::data.table(ability = 1, last_date = Sys.Date()),
                                 Sys.Date(), data.table::data.table(gap = "nope", offset = 0)), "Unknown")
})

test_that("apply_record_blend computes a*R + (1-a)*L and zeroes shrinkage where applied", {
  ab <- data.table::data.table(athlete_id = c("1", "2", "3"), event_id = "E",
                               ability = c(-5.6, -5.5, -5.4), shrinkage = c(0.9, 0.1, 0.5))
  rec <- data.table::data.table(athlete_id = c("1", "2"), event_id = "E", ability_raw = c(-5.45, -5.50),
                                w_total = c(0.2, 2.0), shrinkage = c(0.5, 0.05))
  lng <- data.table::data.table(athlete_id = c("1", "2"), event_id = "E", ability_raw = c(-5.47, -5.52))
  out <- suppressMessages(apply_record_blend(ab, rec, lng, kappa_mult = 0.5))
  kR1 <- 0.5 * 0.2 / 0.5; a1 <- 0.2 / (0.2 + 0.5 * kR1)
  expect_equal(out$ability[1], a1 * -5.45 + (1 - a1) * -5.47)
  expect_equal(out$blend_a[1], a1)
  expect_equal(out$shrinkage[1:2], c(0, 0))
  # athlete 3 has no refit rows: untouched, flagged
  expect_false(out$blend_applied[3]); expect_equal(out$ability[3], -5.4); expect_equal(out$shrinkage[3], 0.5)
  expect_equal(out$athlete_id, ab$athlete_id)            # same rows, same order
  expect_equal(ab$ability, c(-5.6, -5.5, -5.4))          # input not modified
})

test_that("apply_record_blend falls back on non-finite inputs and validates arguments", {
  ab <- data.table::data.table(athlete_id = "1", event_id = "E", ability = -5.6, shrinkage = 0.9)
  rec <- data.table::data.table(athlete_id = "1", event_id = "E", ability_raw = -5.45, w_total = 0, shrinkage = 1)
  lng <- data.table::data.table(athlete_id = "1", event_id = "E", ability_raw = -5.47)
  out <- suppressMessages(apply_record_blend(ab, rec, lng))
  expect_false(out$blend_applied); expect_equal(out$ability, -5.6); expect_equal(out$shrinkage, 0.9)
  expect_error(apply_record_blend(ab, rec, lng, kappa_mult = 0), "positive")
  expect_error(apply_record_blend(ab[, !"shrinkage"], rec, lng), "lacks")
})

test_that("apply_record_blend keeps integer athlete ids integer", {
  ab <- data.table::data.table(athlete_id = 1:2, event_id = "E", ability = c(-5.6, -5.5), shrinkage = c(0.5, 0.5))
  rec <- data.table::data.table(athlete_id = c("1", "2"), event_id = "E", ability_raw = c(-5.5, -5.5),
                                w_total = c(1, 1), shrinkage = c(0.2, 0.2))
  lng <- data.table::data.table(athlete_id = c("1", "2"), event_id = "E", ability_raw = c(-5.5, -5.5))
  out <- suppressMessages(apply_record_blend(ab, rec, lng))
  expect_type(out$athlete_id, "integer"); expect_true(all(out$blend_applied))
})
