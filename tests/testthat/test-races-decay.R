# Race-count decay: discount a result by how many of the athlete's OWN races
# have happened since, on top of the calendar decay.
#
# The default is Inf (off), so the first thing to pin is that existing callers
# are bit-identical. After that, the properties worth asserting are the ones a
# plausible-looking wrong implementation would break: that it counts RACES and
# not days, that it is per athlete-event rather than global, and that it does
# not quietly reorder the returned table.

test_that("Inf is off and reproduces the previous behaviour exactly", {
  hist <- data.table::data.table(
    athlete_id = rep(c("a", "b"), each = 6),
    event_id = "AT-100Metres-M",
    date = as.Date("2024-01-01") + rep(c(0, 60, 120, 200, 300, 400), 2),
    mark = c(10.2, 10.1, 10.0, 9.95, 9.99, 9.98,
             10.5, 10.4, 10.3, 10.35, 10.28, 10.31),
    orientation = -1
  )
  hist[, perf := orientation * log(mark)]
  a_off <- estimate_ability(hist, adjust_context = FALSE, as_of = as.Date("2025-06-01"))
  a_inf <- estimate_ability(hist, adjust_context = FALSE, as_of = as.Date("2025-06-01"),
                            races_half_life = Inf)
  expect_equal(a_off$ability, a_inf$ability)
  expect_equal(a_off$w_total, a_inf$w_total)
})

test_that("it counts RACES, not days", {
  # Both athletes open with the same slow mark on day 0 and finish with the same
  # fast mark on day 100, so the CALENDAR span of their history is identical.
  # `busy` simply raced three more times in between, at that same fast mark.
  #
  # Comparing the two directly would be confounded -- busy's extra fast marks
  # pull their estimate faster under any weighting. So the test measures how
  # much each athlete MOVES when races-since decay is switched on. Only the rank
  # term produces that movement, and it must move `busy` more, because busy has
  # four races since their slow opener where `quiet` has one.
  mk <- function(id, days, marks) data.table::data.table(
    athlete_id = id, event_id = "AT-100Metres-M",
    date = as.Date("2024-01-01") + days, mark = marks, orientation = -1)
  hist <- data.table::rbindlist(list(
    mk("quiet", c(0, 100),             c(10.60, 10.00)),
    mk("busy",  c(0, 25, 50, 75, 100), c(10.60, 10.00, 10.00, 10.00, 10.00))))
  hist[, perf := orientation * log(mark)]

  f <- function(r) {
    a <- estimate_ability(hist, adjust_context = FALSE, as_of = as.Date("2024-06-01"),
                          races_half_life = r)
    stats::setNames(a$ability, a$athlete_id)
  }
  off <- f(Inf); on <- f(2)
  # oriented scale: higher is better, and discounting the slow opener raises both
  expect_gt(on[["quiet"]], off[["quiet"]])
  expect_gt(on[["busy"]],  off[["busy"]])
  # but it must raise `busy` MORE -- that is the whole claim
  expect_gt(on[["busy"]] - off[["busy"]], on[["quiet"]] - off[["quiet"]])
})

test_that("a smaller races half-life weights the most recent marks harder", {
  hist <- data.table::data.table(
    athlete_id = "a", event_id = "AT-100Metres-M",
    date = as.Date("2024-01-01") + c(0, 10, 20, 30, 40, 50),
    # steadily improving: heavier recency weighting must give a FASTER estimate
    mark = c(10.5, 10.4, 10.3, 10.2, 10.1, 10.0),
    orientation = -1
  )
  hist[, perf := orientation * log(mark)]
  f <- function(r) estimate_ability(hist, adjust_context = FALSE,
                                    as_of = as.Date("2024-06-01"),
                                    races_half_life = r)$ability
  # oriented scale: higher is better, so a sharper decay must raise ability
  expect_gt(f(1), f(3))
  expect_gt(f(3), f(10))
  expect_gt(f(10), f(Inf))
})

test_that("the race count is per athlete-event, not global", {
  # If the counter were global, `b`'s marks would be ranked after `a`'s and
  # crushed to near-zero weight, dragging b's estimate toward the prior.
  hist <- data.table::rbindlist(lapply(c("a", "b"), function(id)
    data.table::data.table(
      athlete_id = id, event_id = "AT-100Metres-M",
      date = as.Date("2024-01-01") + c(0, 10, 20, 30),
      mark = c(10.4, 10.3, 10.2, 10.1), orientation = -1)))
  hist[, perf := orientation * log(mark)]
  a <- estimate_ability(hist, adjust_context = FALSE, as_of = as.Date("2024-06-01"),
                        races_half_life = 1)
  expect_equal(a[athlete_id == "a"]$ability, a[athlete_id == "b"]$ability)
  expect_equal(a[athlete_id == "a"]$w_total, a[athlete_id == "b"]$w_total)
})

test_that("two events for one athlete are counted separately", {
  # An athlete's 100m form should not be discounted because they have run a lot
  # of 200m races since.
  hist <- data.table::rbindlist(list(
    data.table::data.table(athlete_id = "a", event_id = "AT-100Metres-M",
      date = as.Date("2024-01-01") + c(0, 10, 20), mark = c(10.3, 10.2, 10.1)),
    data.table::data.table(athlete_id = "a", event_id = "AT-200Metres-M",
      date = as.Date("2024-01-01") + c(5, 15, 25, 35, 45, 55, 65),
      mark = c(20.9, 20.8, 20.7, 20.6, 20.5, 20.4, 20.3)),
    data.table::data.table(athlete_id = "b", event_id = "AT-100Metres-M",
      date = as.Date("2024-01-01") + c(0, 10, 20), mark = c(10.3, 10.2, 10.1))))
  hist[, `:=`(orientation = -1)]
  hist[, perf := orientation * log(mark)]
  a <- estimate_ability(hist, adjust_context = FALSE, as_of = as.Date("2024-06-01"),
                        races_half_life = 2)
  # a's 100m and b's 100m are the same three marks; a's 200m racing is irrelevant
  expect_equal(a[athlete_id == "a" & event_id == "AT-100Metres-M"]$ability,
               a[athlete_id == "b" & event_id == "AT-100Metres-M"]$ability)
})

test_that("turning it on does not change the returned columns or drop athletes", {
  hist <- data.table::rbindlist(lapply(c("a", "b", "c"), function(id)
    data.table::data.table(
      athlete_id = id, event_id = "AT-100Metres-M",
      date = as.Date("2024-01-01") + c(0, 40, 80, 120),
      mark = c(10.4, 10.3, 10.2, 10.1), orientation = -1)))
  hist[, perf := orientation * log(mark)]
  a0 <- estimate_ability(hist, adjust_context = FALSE, as_of = as.Date("2024-06-01"))
  a1 <- estimate_ability(hist, adjust_context = FALSE, as_of = as.Date("2024-06-01"),
                         races_half_life = 4)
  expect_identical(names(a0), names(a1))
  expect_identical(sort(a0$athlete_id), sort(a1$athlete_id))
  expect_true(all(is.finite(a1$ability)))
  # and it must actually have done something, or the test above is vacuous
  expect_false(isTRUE(all.equal(a0$ability, a1$ability)))
})
