plant_venue <- function(effects = c(fast = 0.010, normal = 0, slow = -0.010),
                        n_ath = 50, n_each = 30, sigma = 0.008, seed = 8) {
  set.seed(seed)
  ability <- stats::rnorm(n_ath, to_perf(10, -1L), 0.02)
  venues <- names(effects)
  data.table::rbindlist(lapply(seq_len(n_ath), function(i) {
    v <- sample(venues, n_each, replace = TRUE)
    data.table::data.table(
      athlete_id = as.character(i), event_id = "AT-100Metres-M",
      comp_name = v, date = Sys.Date() - seq_len(n_each),
      round = "F", tier = "OW",
      perf = ability[i] + unname(effects[v]) + stats::rnorm(n_each, 0, sigma))
  }))
}

test_that("planted venue effects are recovered in the right order", {
  d <- plant_venue()
  v <- fit_venue_effect(d)
  expect_equal(nrow(v), 3)
  data.table::setorderv(v, "effect", -1)
  expect_equal(v$venue, c("fast", "normal", "slow"))
})

test_that("the spread between fast and slow venues is about right", {
  d <- plant_venue(effects = c(fast = 0.010, normal = 0, slow = -0.010))
  v <- fit_venue_effect(d)
  spread <- max(v$effect) - min(v$effect)
  expect_lt(abs(spread - 0.020), 0.004)
})

test_that("effects are centred so they add no intercept", {
  v <- fit_venue_effect(plant_venue())
  expect_lt(abs(stats::weighted.mean(v$effect, v$n)), 1e-8)
})

test_that("ability does not leak into venue effects", {
  # Strong athletes deliberately race more at the fast venue. A naive mean would
  # credit the venue with their ability; a within-athlete fit must not.
  set.seed(3)
  n <- 60
  ability <- stats::rnorm(n, to_perf(10, -1L), 0.03)
  strong <- order(ability, decreasing = TRUE)[1:20]
  rows <- data.table::rbindlist(lapply(seq_len(n), function(i) {
    p_fast <- if (i %in% strong) 0.8 else 0.2
    v <- sample(c("fast", "slow"), 40, replace = TRUE, prob = c(p_fast, 1 - p_fast))
    data.table::data.table(
      athlete_id = as.character(i), event_id = "AT-100Metres-M",
      comp_name = v, date = Sys.Date() - seq_len(40), round = "F", tier = "OW",
      perf = ability[i] + ifelse(v == "fast", 0.004, -0.004) +
        stats::rnorm(40, 0, 0.008))
  }))
  v <- fit_venue_effect(rows)
  spread <- v[venue == "fast"]$effect - v[venue == "slow"]$effect
  expect_lt(abs(spread - 0.008), 0.004)   # recovers 0.008, not the ability gap
})

test_that("rare venues are excluded", {
  d <- plant_venue()
  d <- rbind(d, data.table::data.table(
    athlete_id = "1", event_id = "AT-100Metres-M", comp_name = "oneoff",
    date = Sys.Date(), round = "F", tier = "OW", perf = to_perf(9, -1L)))
  expect_false("oneoff" %in% fit_venue_effect(d)$venue)
})

test_that("adjustment removes the venue signal", {
  d <- plant_venue()
  adj <- adjust_venue(d, fit_venue_effect(d))
  adj[, dev := perf - mean(perf), by = athlete_id]
  resid <- adj[, .(m = mean(dev)), by = comp_name]
  expect_lt(max(abs(resid$m)), 0.003)
})

test_that("adjustment shrinks within-athlete spread", {
  d <- plant_venue(effects = c(fast = 0.02, normal = 0, slow = -0.02), sigma = 0.006)
  before <- d[, .(s = stats::sd(perf)), by = athlete_id][, mean(s)]
  adj <- adjust_venue(d, fit_venue_effect(d))
  after <- adj[, .(s = stats::sd(perf)), by = athlete_id][, mean(s)]
  expect_lt(after, before)
})

test_that("unknown venues pass through untouched", {
  d <- plant_venue()
  v <- fit_venue_effect(d)
  d2 <- data.table::copy(d)
  d2[1:5, comp_name := "never-seen"]
  adj <- adjust_venue(d2, v)
  expect_equal(adj$perf[1:5], d2$perf[1:5])
  expect_equal(adj$venue_adj[1:5], rep(0, 5))
})

test_that("an empty fit leaves marks unchanged", {
  d <- plant_venue()
  adj <- adjust_venue(d, fit_venue_effect(d[0]))
  expect_equal(adj$perf, d$perf)
})
