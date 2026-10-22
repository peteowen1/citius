rating_field <- function(n = 14, event_id = "AT-100Metres-M") {
  data.table::data.table(
    athlete_id = paste0("a", seq_len(n)),
    event_id = event_id,
    ability = -log(seq(9.80, 10.30, length.out = n)),
    sigma = 0.008,
    ability_se = 0.003)
}

test_that("one probability per row, in row order, better ability medals more", {
  ab <- rating_field()
  p <- rating_medal_chance(ab, "AT-100Metres-M", n_sims = 3000L, seed = 1L)
  expect_length(p, nrow(ab))
  expect_true(all(p >= 0 & p <= 1))
  # Rows are ordered best to worst, so chances must fall (up to Monte Carlo noise).
  expect_gt(p[1], p[8])
  expect_gt(p[8], p[14])
  expect_lt(cor(p, seq_along(p), method = "spearman"), -0.95)
})

test_that("top-8 members agree with the simulator's own medal probabilities", {
  # The members' number is P(rank <= 3) in the reference field, so it must match
  # what simulate_event()/medal_probs() say about that same 8-athlete field.
  ab <- rating_field()
  p <- rating_medal_chance(ab, "AT-100Metres-M", n_sims = 20000L, seed = 2L)
  mp <- medal_probs(simulate_event(ab[1:8], n_sims = 20000L, seed = 3L))
  ref <- mp$p_medal[match(ab$athlete_id[1:8], mp$athlete_id)]
  expect_equal(p[1:8], ref, tolerance = 0.03)
  # The reference field medals three athletes per race.
  expect_equal(sum(p[1:8]), 3, tolerance = 0.05)
})

test_that("a non-member's chance is what they get joining the reference field", {
  # Beating the third-best of the other eight IS finishing top three of nine, so
  # the rating must match medal_probs() on the 9-athlete field.
  ab <- rating_field()
  joiner <- data.table::copy(ab[4])[, athlete_id := "new"]
  p <- rating_medal_chance(joiner, "AT-100Metres-M", reference = ab[1:8],
                           n_sims = 30000L, seed = 4L)
  mp <- medal_probs(simulate_event(data.table::rbindlist(list(ab[1:8], joiner)),
                                   n_sims = 30000L, seed = 5L))
  expect_equal(as.vector(p), mp$p_medal[mp$athlete_id == "new"], tolerance = 0.02)
})

test_that("a fixed reference gives the same rating whatever else is in the table", {
  ab <- rating_field()
  ref <- ab[1:8]
  all_p <- rating_medal_chance(ab, "AT-100Metres-M", reference = ref,
                               n_sims = 4000L, seed = 6L)
  sub_p <- rating_medal_chance(ab[10:14], "AT-100Metres-M", reference = ref,
                               n_sims = 4000L, seed = 6L)
  expect_equal(as.vector(all_p[10:14]), as.vector(sub_p), tolerance = 0.03)
  # ...and unlike "top8", it does not move when the table's best athlete does.
  stronger <- data.table::copy(ab)
  stronger[1, ability := ability + 0.05]
  p_fixed <- rating_medal_chance(stronger[10:14], "AT-100Metres-M", reference = ref,
                                 n_sims = 4000L, seed = 6L)
  expect_equal(as.vector(p_fixed), as.vector(sub_p))
  p_moving <- rating_medal_chance(stronger, "AT-100Metres-M", n_sims = 4000L, seed = 6L)
  p_still  <- rating_medal_chance(ab, "AT-100Metres-M", n_sims = 4000L, seed = 6L)
  expect_lt(p_moving[8], p_still[8])
})

test_that("the reference field is reported with the answer", {
  ab <- rating_field()
  p <- rating_medal_chance(ab, "AT-100Metres-M", n_sims = 500L, seed = 1L)
  expect_equal(nrow(attr(p, "reference")), 8L)
  expect_match(attr(p, "reference_label"), "top 8")
  p2 <- rating_medal_chance(ab, "AT-100Metres-M", reference = "top5", n_sims = 500L, seed = 1L)
  expect_equal(nrow(attr(p2, "reference")), 5L)
  p3 <- rating_medal_chance(ab, "AT-100Metres-M", reference = ab[1:6], n_sims = 500L, seed = 1L)
  expect_match(attr(p3, "reference_label"), "fixed")
})

test_that("seeded runs are reproducible and leave the RNG stream alone", {
  ab <- rating_field()
  set.seed(10); a <- rating_medal_chance(ab, "AT-100Metres-M", n_sims = 500L, seed = 9L)
  x <- runif(1)
  set.seed(10); b <- rating_medal_chance(ab, "AT-100Metres-M", n_sims = 500L, seed = 9L)
  y <- runif(1)
  expect_equal(as.vector(a), as.vector(b))
  expect_equal(x, y)
})

test_that("results do not depend on the block size", {
  ab <- rating_field(40)
  one <- rating_medal_chance(ab[1:8], "AT-100Metres-M", n_sims = 2000L, seed = 1L)
  expect_length(one, 8L)
  # 40 athletes in one block vs. split into two calls: same fixed reference.
  ref <- ab[1:8]
  p_all <- rating_medal_chance(ab, "AT-100Metres-M", reference = ref, n_sims = 4000L, seed = 2L)
  p_part <- c(rating_medal_chance(ab[9:20], "AT-100Metres-M", reference = ref,
                                  n_sims = 4000L, seed = 2L),
              rating_medal_chance(ab[21:40], "AT-100Metres-M", reference = ref,
                                  n_sims = 4000L, seed = 2L))
  expect_equal(as.vector(p_all[9:40]), as.vector(p_part), tolerance = 0.03)
})

test_that("missing estimates give NA, and bad inputs fail loudly", {
  ab <- rating_field()
  ab[3, ability := NA_real_]
  p <- rating_medal_chance(ab, "AT-100Metres-M", n_sims = 500L, seed = 1L)
  expect_true(is.na(p[3]))
  expect_false(anyNA(p[-3]))

  expect_error(rating_medal_chance(rating_field(), "AT-200Metres-M"), "must be event")
  expect_error(suppressWarnings(rating_medal_chance(rating_field(3), "AT-100Metres-M")),
               "at least 4")
  expect_error(rating_medal_chance(rating_field(), "AT-100Metres-M", reference = "best"),
               "must be a string")
  expect_error(rating_medal_chance(rating_field(), "AT-100Metres-M", reference = ab[, -"sigma"]),
               "missing required")
  expect_error(rating_medal_chance(rating_field()[, -"sigma"], "AT-100Metres-M"),
               "missing required")
})

test_that("calibrated sensitivity is normalised to the reference, not the block", {
  ab <- rating_field(20)
  cal <- structure(list(athlete = data.table::data.table(
    athlete_id = ab$athlete_id,
    sensitivity = seq(0.4, 1.6, length.out = 20))), class = "citius_calibration")
  ref <- ab[1:8]
  all_p <- rating_medal_chance(ab, "AT-100Metres-M", cal, reference = ref,
                               n_sims = 4000L, seed = 3L)
  sub_p <- rating_medal_chance(ab[18:20], "AT-100Metres-M", cal, reference = ref,
                               n_sims = 4000L, seed = 3L)
  expect_equal(as.vector(all_p[18:20]), as.vector(sub_p), tolerance = 0.03)
})

test_that("condition_sensitivity() is unchanged by the .sensitivity_raw() split", {
  ab <- rating_field(6)
  cal <- list(athlete = data.table::data.table(
    athlete_id = ab$athlete_id[1:5], sensitivity = c(0.5, 1, 1.5, 2, 1)))
  s <- condition_sensitivity(ab, "AT-100Metres-M", cal)
  raw <- c(0.5, 1, 1.5, 2, 1, 1)
  expect_equal(s, raw / mean(raw))
  expect_equal(condition_sensitivity(ab, "AT-100Metres-M"), rep(1, 6))
  # sensitivity_scale = field mean reproduces the default exactly.
  a <- simulate_event(ab, n_sims = 300L, seed = 1L, calibration = cal)
  b <- simulate_event(ab, n_sims = 300L, seed = 1L, calibration = cal,
                      sensitivity_scale = mean(raw))
  expect_equal(a$perf, b$perf)
})
