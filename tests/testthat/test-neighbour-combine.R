# Cross-event ability combination.
#
# The properties pinned here are the ones a plausible wrong implementation
# would break, and three of them are mistakes this code actually made before it
# shipped (2026-09-14):
#
#   * weighting by 1/ability_se^2 instead of w_total/sigma^2, which handed a
#     four-year-stale 800m rating 29% of an athlete's 5000m weight because
#     ability_se is bounded by each event's own prior spread and is overwritten
#     for unevidenced athletes;
#   * a neighbour with no evidence producing Inf/Inf = NaN rather than zero
#     weight, which silently dropped the athlete's OWN rating out of their own
#     combination;
#   * taking only the strongest edge, which is what transfer_neighbour_ability()
#     does and is not what this function is for -- every neighbour contributes.
#
# The uncorrelated-neighbour test is the load-bearing one. It is why no
# hand-maintained list of which events may inform which is needed: a pair that
# carries no signal has tau2 approaching the target's whole spread and falls out
# on its own arithmetic.

ab_fix <- function(...) {
  d <- data.table::data.table(...)
  if (is.null(d$shrinkage)) d[, shrinkage := 0]
  if (is.null(d$prior_mu)) d[, prior_mu := 0]
  d[, ability := (1 - shrinkage) * ability_raw + shrinkage * prior_mu][]
}

two_event <- function(own_w = 1, nb_w = 1, own_sig = 0.1, nb_sig = 0.1) {
  ab_fix(athlete_id = c("a1", "a1"),
         event_id   = c("EV-T", "EV-N"),
         ability_raw = c(0, 1),
         sigma = c(own_sig, nb_sig),
         w_total = c(own_w, nb_w))
}
link <- function(delta = 0, tau2 = 0) {
  data.table::data.table(event_id = "EV-T", neighbour_event_id = "EV-N",
                         delta = delta, tau2 = tau2)
}

test_that("NULL links is a true no-op", {
  ab <- two_event()
  expect_equal(combine_neighbour_ability(ab, NULL), ab)
  expect_equal(combine_neighbour_ability(ab, link()[0]), ab)
})

test_that("confidence and spread columns are never touched", {
  out <- combine_neighbour_ability(two_event(), link())
  ab <- two_event()
  # The function adds evidence about WHERE ability sits; it does not re-argue
  # how confident the estimator should have been.
  expect_equal(out$w_total, ab$w_total)
  expect_equal(out$sigma, ab$sigma)
  expect_equal(out$shrinkage, ab$shrinkage)
  expect_equal(nrow(out), nrow(ab))
})

test_that("the offset maps the neighbour onto the target's scale", {
  # delta = -1 puts the neighbour's 1 exactly on the target's 0, so a perfectly
  # agreeing neighbour must move nothing. Without the offset it would drag the
  # athlete halfway to 1.
  out <- combine_neighbour_ability(two_event(), link(delta = -1, tau2 = 0))
  expect_equal(out[event_id == "EV-T"]$ability_raw, 0)

  # delta = 0 with equal precision is a straight average.
  out0 <- combine_neighbour_ability(two_event(), link(delta = 0, tau2 = 0))
  expect_equal(out0[event_id == "EV-T"]$ability_raw, 0.5)
})

test_that("confidence is evidence, not ability_se: more races pull harder", {
  thin <- combine_neighbour_ability(two_event(nb_w = 0.25), link())
  fat  <- combine_neighbour_ability(two_event(nb_w = 4), link())
  t_val <- thin[event_id == "EV-T"]$ability_raw
  f_val <- fat[event_id == "EV-T"]$ability_raw
  expect_lt(t_val, f_val)          # both toward 1, the better-evidenced further
  expect_equal(t_val, 0.2)         # w 1 vs 0.25 -> 1/(1+0.25) share
  expect_equal(f_val, 0.8)
})

test_that("a neighbour with no evidence earns zero weight, not NaN", {
  out <- combine_neighbour_ability(two_event(nb_w = 0), link())
  v <- out[event_id == "EV-T"]$ability_raw
  expect_false(is.na(v))
  expect_equal(v, 0)
})

test_that("an athlete with no evidence of their own takes the neighbour", {
  out <- combine_neighbour_ability(two_event(own_w = 0), link())
  expect_equal(out[event_id == "EV-T"]$ability_raw, 1)
})

test_that("no evidence on EITHER side leaves the rating alone", {
  # The real 0/0 path: own weight 0 and neighbour weight 0 divide to NaN unless
  # the sum is guarded. The two single-sided cases above both have one finite
  # weight and so cannot reach it.
  ab <- two_event(own_w = 0, nb_w = 0)
  out <- combine_neighbour_ability(ab, link())
  v <- out[event_id == "EV-T"]$ability_raw
  expect_false(is.na(v))
  expect_equal(v, 0)
  expect_false(any(is.na(out$ability)))
})

test_that("an uncorrelated neighbour falls out on its own arithmetic", {
  # tau2 large relative to the neighbour's evidence variance is what r -> 0
  # produces, and it must leave the athlete essentially where they were.
  out <- combine_neighbour_ability(two_event(), link(tau2 = 1e6))
  expect_lt(abs(out[event_id == "EV-T"]$ability_raw), 1e-5)
})

test_that("every neighbour contributes, not only the strongest", {
  ab <- ab_fix(athlete_id = rep("a1", 3),
               event_id = c("EV-T", "EV-N", "EV-M"),
               ability_raw = c(0, 1, 1), sigma = rep(0.1, 3), w_total = rep(1, 3))
  lk <- data.table::data.table(
    event_id = c("EV-T", "EV-T"), neighbour_event_id = c("EV-N", "EV-M"),
    delta = c(0, 0), tau2 = c(0, 0))
  one <- combine_neighbour_ability(ab, lk[1])
  two <- combine_neighbour_ability(ab, lk)
  expect_equal(one[event_id == "EV-T"]$ability_raw, 0.5)   # 1 own + 1 neighbour
  expect_equal(two[event_id == "EV-T"]$ability_raw, 2/3)   # 1 own + 2 neighbours
})

test_that("ability is rebuilt through the athlete's existing shrinkage", {
  ab <- two_event()
  ab[, `:=`(shrinkage = 0.5, prior_mu = -2)]
  out <- combine_neighbour_ability(ab, link())
  r <- out[event_id == "EV-T"]
  expect_equal(r$ability_raw, 0.5)
  expect_equal(r$ability, 0.5 * 0.5 + 0.5 * -2)
})

test_that("a table not from estimate_ability() is refused by name", {
  ab <- two_event()[, .(athlete_id, event_id, ability_raw)]
  expect_error(combine_neighbour_ability(ab, link()), "sigma")
})

test_that("fit_neighbour_links clamps a negative correlation to zero", {
  # A negatively correlated pair is noise, not a backwards transfer. Clamping
  # sends tau2 to the target's whole spread, which is the no-weight case above.
  set.seed(1)
  n <- 200
  y <- rnorm(n)
  d <- data.table::rbindlist(list(
    data.table::data.table(athlete_id = paste0("a", 1:n), event_id = "EV-T",
                           ability_raw = y),
    data.table::data.table(athlete_id = paste0("a", 1:n), event_id = "EV-N",
                           ability_raw = -y + rnorm(n, sd = 0.01))))
  lk <- fit_neighbour_links(d, target_events = "EV-T", elite_quantile = 0,
                            min_n = 10L)
  expect_equal(lk$r, 0)
  expect_equal(lk$tau2, lk$sigma2_target)
})

test_that("fit_neighbour_links drops a pair too thin to fit", {
  d <- data.table::rbindlist(list(
    data.table::data.table(athlete_id = paste0("a", 1:100), event_id = "EV-T",
                           ability_raw = rnorm(100)),
    data.table::data.table(athlete_id = paste0("a", 1:3), event_id = "EV-N",
                           ability_raw = rnorm(3))))
  lk <- fit_neighbour_links(d, target_events = "EV-T", elite_quantile = 0,
                            min_n = 30L)
  expect_equal(nrow(lk), 0)
})
