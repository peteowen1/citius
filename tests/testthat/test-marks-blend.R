# The marks-only recency blend, and the guarantee that makes it safe to ship.
#
# `estimate_ability()` emits `recent_mean`, the mean of an athlete's last five
# RAW marks. `simulate_event()` can blend the centre of the MARK distribution
# toward it by `CITIUS_MARKS_BLEND`, and leaves the ranking on plain `ability`.
#
# THE BLEND IS OFF BY DEFAULT and is a diagnostic lever, not a model component.
# It was deployed at 0.5 on 2026-09-07 and withdrawn the same day: blending a
# prediction toward the baseline it is scored against is not a way to beat that
# baseline. The machinery is tested because it still runs when switched on for a
# measurement, and because the OFF default is itself worth pinning.
#
# That claim is the thing worth testing. Everything else here is scaffolding.

test_that("the blend moves the mark centre and leaves every placing identical", {
  ab <- data.table::data.table(
    athlete_id = c("a", "b", "c", "d"),
    event_id   = "AT-100Metres-M",
    ability     = -log(c(9.85, 9.92, 10.01, 10.12)),
    # Recent form deliberately REORDERS the field: d has been the sharpest.
    recent_mean = -log(c(10.20, 10.05, 10.02, 9.95)),
    sigma       = c(0.006, 0.007, 0.008, 0.009),
    sigma_marks = c(0.006, 0.007, 0.008, 0.009)
  )
  ab_no <- data.table::copy(ab)[, recent_mean := NULL]

  withr::with_envvar(c(CITIUS_MARKS_BLEND = "0.6"), {
    set.seed(99); s_yes <- simulate_event(ab, n_sims = 4000L)
    set.seed(99); s_no  <- simulate_event(ab_no, n_sims = 4000L)

    # Placings: bit-identical. Same seed, same draws, and the blend appears in
    # neither `perf` nor `rank`. A reordering recent_mean is the strong form of
    # this test -- if any of it leaked into the ranking, d would move up.
    expect_identical(s_yes$rank, s_no$rank)
    expect_identical(s_yes$perf, s_no$perf)
    expect_identical(medal_probs(s_yes)$p_gold,  medal_probs(s_no)$p_gold)
    expect_identical(medal_probs(s_yes)$p_medal, medal_probs(s_no)$p_medal)

    # Marks: they DO move, or the column does nothing and the test is vacuous.
    m_yes <- medal_probs(s_yes)[order(athlete_id)]
    m_no  <- medal_probs(s_no)[order(athlete_id)]
    expect_false(isTRUE(all.equal(m_yes$median_mark, m_no$median_mark)))
    expect_equal(m_yes$median_mark,
                 exp(-(0.4 * ab$ability + 0.6 * ab$recent_mean)), tolerance = 0.01)
  })
})

test_that("blend 0 is the identity on marks", {
  ab <- data.table::data.table(
    athlete_id = c("a", "b", "c"), event_id = "AT-100Metres-M",
    ability     = -log(c(9.9, 10.0, 10.1)),
    recent_mean = -log(c(10.5, 10.6, 10.7)),
    sigma = 0.007, sigma_marks = 0.007
  )
  withr::with_envvar(c(CITIUS_MARKS_BLEND = "0"), {
    set.seed(3); s0 <- simulate_event(ab, n_sims = 3000L)
  })
  ab_no <- data.table::copy(ab)[, recent_mean := NULL]
  set.seed(3); s_no <- simulate_event(ab_no, n_sims = 3000L)
  expect_identical(medal_probs(s0)$median_mark, medal_probs(s_no)$median_mark)
})

test_that("the blend tracks a LATER adjustment to ability", {
  # This is why estimate_ability() emits recent_mean rather than a pre-blended
  # centre. backtest_athletics.R ages and momentum-adjusts `ability` after the
  # estimate returns; a stored blended column would stop tracking those, and an
  # aged athlete's ranking would move while their predicted mark stayed put.
  ab <- data.table::data.table(
    athlete_id = c("a", "b"), event_id = "AT-100Metres-M",
    ability     = -log(c(9.90, 10.00)),
    recent_mean = -log(c(10.30, 10.40)),
    sigma = 0.007, sigma_marks = 0.007
  )
  aged <- data.table::copy(ab)[, ability := ability - 0.01]  # a tenth of a second slower
  withr::with_envvar(c(CITIUS_MARKS_BLEND = "0.6"), {
    set.seed(7); m1 <- medal_probs(simulate_event(ab,   n_sims = 4000L))[order(athlete_id)]
    set.seed(7); m2 <- medal_probs(simulate_event(aged, n_sims = 4000L))[order(athlete_id)]
  })
  # The mark must move by (1 - blend) of the ability shift, not by zero.
  expect_equal(log(m2$median_mark) - log(m1$median_mark), rep(0.4 * 0.01, 2),
               tolerance = 1e-3)
})

test_that("estimate_ability emits recent_mean from the last five RAW marks", {
  # Six marks; the earliest is much the fastest, so a mean of the last FIVE
  # must be clearly slower than a decay-weighted ability over all six.
  hist <- data.table::rbindlist(lapply(c("a", "b"), function(id) {
    data.table::data.table(
      athlete_id = id, event_id = "AT-100Metres-M",
      date = as.Date("2025-01-01") + c(0, 30, 60, 90, 120, 150),
      mark = c(9.80, 10.05, 10.06, 10.07, 10.08, 10.09),
      orientation = -1
    )
  }))
  hist[, perf := orientation * log(mark)]
  a <- estimate_ability(hist, adjust_context = FALSE, as_of = as.Date("2025-07-01"))

  expect_true("recent_mean" %in% names(a))
  # the last five marks, exactly -- raw, unweighted, undecayed
  expect_equal(unique(a$recent_mean), mean(-log(c(10.05, 10.06, 10.07, 10.08, 10.09))))
  # `ability` must not depend on the blend setting at all
  withr::with_envvar(c(CITIUS_MARKS_BLEND = "0"), {
    a0 <- estimate_ability(hist, adjust_context = FALSE, as_of = as.Date("2025-07-01"))
  })
  expect_equal(a0$ability, a$ability)
})

test_that("an athlete with fewer than three marks gets NA and keeps ability", {
  hist <- data.table::data.table(
    athlete_id = c("a", "a", "b", "b", "b", "b"),
    event_id = "AT-100Metres-M",
    date = as.Date("2025-01-01") + c(0, 30, 0, 30, 60, 90),
    mark = c(9.9, 10.4, 9.9, 10.4, 10.4, 10.4),
    orientation = -1
  )
  hist[, perf := orientation * log(mark)]
  a <- estimate_ability(hist, min_results = 2L, adjust_context = FALSE,
                        as_of = as.Date("2025-07-01"))
  expect_true(is.na(a[athlete_id == "a"]$recent_mean))
  expect_true(is.finite(a[athlete_id == "b"]$recent_mean))

  # and such an athlete's mark is the unblended ability
  ab <- data.table::data.table(
    athlete_id = c("a", "b"), event_id = "AT-100Metres-M",
    ability = -log(c(9.9, 10.0)), recent_mean = c(NA_real_, -log(10.4)),
    sigma = 0.007, sigma_marks = 0.007)
  withr::with_envvar(c(CITIUS_MARKS_BLEND = "0.6"), {
    set.seed(5); m <- medal_probs(simulate_event(ab, n_sims = 4000L))[order(athlete_id)]
  })
  expect_equal(m$median_mark[1], 9.9, tolerance = 0.01)
})

test_that("an out-of-range CITIUS_MARKS_BLEND warns and falls back", {
  withr::with_envvar(c(CITIUS_MARKS_BLEND = "1.4"), {
    expect_warning(v <- citius:::.marks_blend(), "not a number in")
    expect_equal(v, 0)
  })
  withr::with_envvar(c(CITIUS_MARKS_BLEND = "nonsense"), {
    expect_warning(v <- citius:::.marks_blend(), "not a number in")
    expect_equal(v, 0)
  })
  withr::with_envvar(c(CITIUS_MARKS_BLEND = "0.35"), {
    expect_equal(citius:::.marks_blend(), 0.35)
  })
  # OFF by default, and pinned so it stays off. Blending predictions toward the
  # very baseline they are scored against is not a way to beat that baseline;
  # the term survives only as a diagnostic lever. A silent drift back above 0
  # would quietly restore it and flatter every comparison in the lab.
  withr::with_envvar(c(CITIUS_MARKS_BLEND = ""), {
    expect_equal(citius:::.marks_blend(), 0)
  })
})

test_that("the empty ability table carries every column the populated one does", {
  # This drifted once already: sigma_marks was added to the populated return on
  # 2026-09-06 and not to the empty one, which is how a caller that rbinds the
  # two ends up with silent NAs in a column it thinks is populated.
  hist <- data.table::data.table(
    athlete_id = rep(c("a", "b"), each = 4),
    event_id = "AT-100Metres-M",
    date = as.Date("2025-01-01") + rep(c(0, 30, 60, 90), 2),
    mark = c(9.9, 9.95, 10.0, 10.05, 10.1, 10.15, 10.2, 10.25),
    orientation = -1
  )
  hist[, perf := orientation * log(mark)]
  full <- estimate_ability(hist, adjust_context = FALSE, as_of = as.Date("2025-07-01"))
  expect_true(all(names(full) %in% names(citius:::.empty_ability())),
              info = paste("missing from .empty_ability():",
                           paste(setdiff(names(full), names(citius:::.empty_ability())),
                                 collapse = ", ")))
})
