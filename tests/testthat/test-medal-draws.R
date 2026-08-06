test_that("medal_draws is off by default and changes nothing when off", {
  ab <- data.table::data.table(
    athlete_id = as.character(1:12),
    event_id = "AT-100Metres-M",
    ability = seq(-2.30, -2.25, length.out = 12),
    sigma = rep(0.01, 12),
    ability_se = rep(0.004, 12))
  a <- simulate_rounds(ab, structure = list(list(races = 2, advance = 3,
                                                 fastest_losers = 2),
                                            list(races = 1)),
                       n_sims = 2000L, seed = 1L)
  b <- simulate_rounds(ab, structure = list(list(races = 2, advance = 3,
                                                 fastest_losers = 2),
                                            list(races = 1)),
                       n_sims = 2000L, seed = 1L, medal_draws = TRUE)
  expect_null(attr(a, "medal_draws"))
  expect_false(is.null(attr(b, "medal_draws")))
  # Same seed must give identical probabilities: asking for the draws must not
  # perturb the random stream, or every published number would shift the day
  # this flag is switched on.
  expect_equal(a$p_gold, b$p_gold)
  expect_equal(a$p_medal, b$p_medal)
})

test_that("the draws are a real podium: three per simulation, places 1-3", {
  ab <- data.table::data.table(
    athlete_id = as.character(1:10),
    event_id = "AT-800Metres-W",
    ability = seq(-4.78, -4.74, length.out = 10),
    sigma = rep(0.012, 10),
    ability_se = rep(0.004, 10))
  n <- 1500L
  r <- simulate_rounds(ab, structure = list(list(races = 1)),
                       n_sims = n, seed = 7L, medal_draws = TRUE)
  d <- attr(r, "medal_draws")

  expect_equal(data.table::uniqueN(d$sim), n)
  expect_equal(nrow(d), 3L * n)
  expect_setequal(unique(d$place), 1:3)
  # Exactly one of each place per simulation — a podium, not a ranking.
  per <- d[, .N, by = .(sim, place)]
  expect_true(all(per$N == 1L))
  # Nobody may appear twice on one podium.
  expect_equal(nrow(d[, .N, by = .(sim, athlete_id)][N > 1L]), 0L)
})

test_that("draws reproduce the marginals they were collapsed from", {
  # The whole point is that the joint draws and the reported marginals are the
  # same object viewed two ways. If they disagree, one of them is a lie.
  ab <- data.table::data.table(
    athlete_id = as.character(1:9),
    event_id = "AT-LongJump-M",
    ability = seq(2.05, 2.11, length.out = 9),
    sigma = rep(0.02, 9),
    ability_se = rep(0.006, 9))
  n <- 4000L
  r <- simulate_rounds(ab, structure = list(list(races = 1)),
                       n_sims = n, seed = 11L, medal_draws = TRUE)
  d <- attr(r, "medal_draws")

  gold <- d[place == 1L, .(p = .N / n), by = athlete_id]
  med  <- d[, .(p = .N / n), by = athlete_id]
  chk  <- merge(r[, .(athlete_id, p_gold, p_medal)], gold, by = "athlete_id",
                all.x = TRUE)
  chk[is.na(p), p := 0]
  expect_equal(chk$p_gold, chk$p, tolerance = 1e-9)

  chk2 <- merge(r[, .(athlete_id, p_medal)], med, by = "athlete_id", all.x = TRUE)
  chk2[is.na(p), p := 0]
  expect_equal(chk2$p_medal, chk2$p, tolerance = 1e-9)
})

test_that("multi-entrant nations are NEGATIVELY dependent, which is the point", {
  # Two athletes of equal ability contest the same three medals. Summing their
  # marginal medal probabilities as independent Bernoullis would overstate the
  # variance of their combined count; the joint draws must not.
  ab <- data.table::data.table(
    athlete_id = as.character(1:8),
    event_id = "AT-100Metres-M",
    ability = rep(-2.28, 8),
    sigma = rep(0.01, 8),
    ability_se = rep(0.003, 8))
  n <- 6000L
  r <- simulate_rounds(ab, structure = list(list(races = 1)),
                       n_sims = n, seed = 3L, medal_draws = TRUE)
  d <- attr(r, "medal_draws")
  pair <- c("1", "2")
  cnt <- d[athlete_id %in% pair, .(k = .N), by = sim]
  full <- merge(data.table::data.table(sim = seq_len(n)), cnt, by = "sim",
                all.x = TRUE)
  full[is.na(k), k := 0L]

  p1 <- r[athlete_id == "1"]$p_medal
  p2 <- r[athlete_id == "2"]$p_medal
  var_joint <- stats::var(full$k)
  var_indep <- p1 * (1 - p1) + p2 * (1 - p2)
  expect_lt(var_joint, var_indep)
  # Means must still agree — dependence moves the spread, not the expectation.
  expect_equal(mean(full$k), p1 + p2, tolerance = 0.02)
})
