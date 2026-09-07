# Per-event parameters: races_half_life, trim_tactical and context_scale all
# accept either a scalar or a table keyed on event_id and/or family.
#
# The properties worth pinning are the ones a plausible wrong implementation
# would break: that the scalar path is unchanged, that an event the table does
# not name falls back to the DOCUMENTED DEFAULT rather than to a median of other
# events' values, that family is used only when the event is absent, and that
# the value travels with its row rather than being applied after a sort.

hist2 <- function(ids = c("AT-100Metres-M", "AT-ShotPut-M")) {
  h <- data.table::rbindlist(lapply(ids, function(ev) {
    ori <- if (grepl("Metres", ev)) -1 else 1
    base <- if (ori < 0) 10.2 else 20.5
    data.table::data.table(
      athlete_id = paste0("a_", ev), event_id = ev,
      date = as.Date("2024-01-01") + c(0, 30, 60, 90, 120, 150),
      mark = base + ori * c(0, 0.05, 0.10, 0.15, 0.20, 0.25) * -1,
      orientation = ori)
  }))
  h[, perf := orientation * log(mark)][]
}

test_that(".event_param falls back to the DEFAULT, never a median", {
  ids <- c("AT-100Metres-M", "AT-ShotPut-M", "AT-Marathon-W")
  tb <- data.frame(event_id = c("AT-100Metres-M", "AT-ShotPut-M"),
                   races_half_life = c(3, 9))
  v <- citius:::.event_param(ids, tb, "races_half_life", Inf)
  expect_equal(v[1:2], c(3, 9))
  # the unnamed event gets Inf, NOT median(c(3, 9)) == 6
  expect_true(is.infinite(v[3]))
})

test_that("family is used only where the event is absent", {
  ids <- c("AT-100Metres-M", "AT-ShotPut-M")
  tb <- data.frame(event_id = c("AT-100Metres-M", NA), family = c(NA, "throw"),
                   trim_tactical = c(0.05, 0.40))
  v <- citius:::.event_param(ids, tb, "trim_tactical", 0.25)
  expect_equal(v[1], 0.05)   # its own row wins
  expect_equal(v[2], 0.40)   # falls through to its family
})

test_that("a scalar is unchanged and a table naming every event matches it", {
  h <- hist2()
  a_scalar <- estimate_ability(h, adjust_context = FALSE, as_of = as.Date("2024-09-01"),
                               races_half_life = 4)
  tb <- data.frame(event_id = unique(h$event_id), races_half_life = 4)
  a_table <- estimate_ability(h, adjust_context = FALSE, as_of = as.Date("2024-09-01"),
                              races_half_life = tb)
  expect_equal(a_scalar$ability, a_table$ability)
  expect_equal(a_scalar$w_total, a_table$w_total)
})

test_that("a per-event table gives each event its own decay", {
  h <- hist2()
  same <- estimate_ability(h, adjust_context = FALSE, as_of = as.Date("2024-09-01"),
                           races_half_life = 2)
  # sharp decay for the 100m only; the shot put is left off
  tb <- data.frame(event_id = "AT-100Metres-M", races_half_life = 2)
  mixed <- estimate_ability(h, adjust_context = FALSE, as_of = as.Date("2024-09-01"),
                            races_half_life = tb)
  m <- merge(same[, .(event_id, same = ability)], mixed[, .(event_id, mixed = ability)],
             by = "event_id")
  expect_equal(m[event_id == "AT-100Metres-M"]$same, m[event_id == "AT-100Metres-M"]$mixed)
  # the shot put, absent from the table, must differ -- it fell back to Inf
  expect_false(isTRUE(all.equal(m[event_id == "AT-ShotPut-M"]$same,
                                m[event_id == "AT-ShotPut-M"]$mixed)))
})

test_that("the per-event value travels with its row, not with the sort order", {
  # races_half_life reorders the table by (athlete, event, -date). If the values
  # were resolved into a bare vector before that sort, they would be applied to
  # the wrong rows -- silently, with no error. Two events whose rows interleave
  # by date make that visible: scoring them together must equal scoring each
  # alone.
  h <- hist2()
  tb <- data.frame(event_id = c("AT-100Metres-M", "AT-ShotPut-M"),
                   races_half_life = c(1, 50))
  together <- estimate_ability(h, adjust_context = FALSE, as_of = as.Date("2024-09-01"),
                               races_half_life = tb)
  apart <- data.table::rbindlist(lapply(unique(h$event_id), function(ev)
    estimate_ability(h[event_id == ev], adjust_context = FALSE,
                     as_of = as.Date("2024-09-01"),
                     races_half_life = tb[tb$event_id == ev, ])))
  m <- merge(together[, .(event_id, tog = ability)], apart[, .(event_id, apt = ability)],
             by = "event_id")
  expect_equal(m$tog, m$apt)
})

test_that("context_scale 1 is the default and 0 leaves the raw mark", {
  h <- hist2("AT-100Metres-M")
  h[, `:=`(round = "final", tier = "GW")]
  a_default <- estimate_ability(h, as_of = as.Date("2024-09-01"))
  a_one     <- estimate_ability(h, as_of = as.Date("2024-09-01"), context_scale = 1)
  expect_equal(a_default$ability, a_one$ability)

  a_zero <- estimate_ability(h, as_of = as.Date("2024-09-01"), context_scale = 0)
  a_raw  <- estimate_ability(h, as_of = as.Date("2024-09-01"), adjust_context = FALSE)
  # scale 0 discards the whole correction, so it must match the unadjusted path
  expect_equal(a_zero$ability, a_raw$ability)
})

test_that("trim_tactical accepts a table and 0 disables it per event", {
  # A tactical event with a clear worst mark: trimming must raise the estimate.
  h <- data.table::data.table(
    athlete_id = "a", event_id = "AT-1500Metres-M",
    date = as.Date("2024-01-01") + c(0, 20, 40, 60, 80),
    mark = c(230, 218, 217, 216, 215), orientation = -1)
  h[, perf := orientation * log(mark)]
  trimmed <- estimate_ability(h, adjust_context = FALSE, as_of = as.Date("2024-09-01"),
                              trim_tactical = 0.25)
  none <- estimate_ability(h, adjust_context = FALSE, as_of = as.Date("2024-09-01"),
                           trim_tactical = 0)
  tb <- data.frame(event_id = "AT-1500Metres-M", trim_tactical = 0)
  none_tb <- estimate_ability(h, adjust_context = FALSE, as_of = as.Date("2024-09-01"),
                              trim_tactical = tb)
  expect_equal(none$ability, none_tb$ability)
  expect_gt(trimmed$ability, none$ability)   # oriented: dropping the worst raises it
})

test_that("the tactical override only fires in families where tactics exist", {
  # `tactical_index` is the skew of an event's fitted race effects, so it fires
  # whenever some races come out much slower than typical. In a 1500m that is
  # sit-and-kick; in a shot put it is weather. Only the first should trim.
  expect_true(all(c("middle", "distance") %in% citius:::.CITIUS_TACTICAL_FAMILIES))
  expect_false(any(c("sprint", "throw", "jump", "hurdles") %in%
                     citius:::.CITIUS_TACTICAL_FAMILIES))

  reg <- data.table::as.data.table(citius_events())
  mk <- function(ev, marks) {
    ori <- reg$orientation[match(ev, reg$event_id)]
    h <- data.table::data.table(
      athlete_id = "a", event_id = ev,
      date = as.Date("2024-01-01") + seq(0, by = 20, length.out = length(marks)),
      mark = marks, orientation = ori)
    h[, perf := orientation * log(mark)][]
  }
  # The pair has to be chosen carefully. The 1500m is ALREADY tactical in the
  # registry, so the override adds nothing there and the test would pass
  # vacuously. The marathon is NOT registry-tactical and IS in a tactical
  # family, so it is one of the events the override genuinely adds; the shot put
  # is one it should stop adding.
  expect_false(reg$tactical[match("AT-Marathon-M", reg$event_id)])
  expect_false(reg$tactical[match("AT-ShotPut-M", reg$event_id)])

  # a calibration claiming BOTH events are strongly negatively skewed
  cal <- list(events = data.frame(
    event_id = c("AT-Marathon-M", "AT-ShotPut-M"),
    tactical_index = c(-2, -2), calibrated = c(TRUE, TRUE)))

  # each has one clearly worst mark, so trimming it must raise the estimate
  h1 <- mk("AT-Marathon-M", c(9000, 7810, 7800, 7790, 7780))
  h2 <- mk("AT-ShotPut-M",  c(17.0, 20.4, 20.5, 20.6, 20.7))
  f <- function(h, calib) estimate_ability(h, adjust_context = FALSE,
                                           as_of = as.Date("2024-09-01"),
                                           calibration = calib,
                                           trim_tactical = 0.25)$ability

  # the marathon is in a tactical family, so the override fires and lifts it
  expect_gt(f(h1, cal), f(h1, NULL))
  # the shot put is not, so the override is ignored and nothing moves
  expect_equal(f(h2, cal), f(h2, NULL))
})

test_that("precision_scale exponentiates the context weights and leaves recency alone", {
  cal <- list(round = data.frame(round_class = c("final", "heat"),
                                 precision = c(0.8, 1.6)),
              tier  = data.frame(tier_class = c("low", "top"),
                                 precision = c(1.1, 0.9)))
  d <- as.Date("2025-01-01")
  w1 <- result_weight(d, tier = "F", round = "heat", as_of = d,
                      half_life = Inf, calibration = cal)
  w0 <- result_weight(d, tier = "F", round = "heat", as_of = d,
                      half_life = Inf, calibration = cal, precision_scale = 0)
  wh <- result_weight(d, tier = "F", round = "heat", as_of = d,
                      half_life = Inf, calibration = cal, precision_scale = 0.5)
  # scale 0 flattens the context weight to exactly 1
  expect_equal(w0, 1)
  # and 0.5 is the square root of the full weight
  expect_equal(wh, sqrt(w1))

  # RECENCY MUST BE UNTOUCHED. Exponentiating the whole weight instead of only
  # the precision would silently rescale the half-life, which is a different
  # parameter with its own fitted value.
  old <- as.Date("2024-01-01")
  r1 <- result_weight(old, tier = "F", round = "heat", as_of = d,
                      half_life = 365, calibration = cal)
  r0 <- result_weight(old, tier = "F", round = "heat", as_of = d,
                      half_life = 365, calibration = cal, precision_scale = 0)
  expect_equal(r0, 0.5^(as.numeric(d - old) / 365))
  expect_equal(r1 / r0, w1)          # the ratio is the context weight alone
})

test_that("precision_scale reaches estimate_ability and takes a per-event table", {
  h <- data.table::rbindlist(lapply(c("AT-100Metres-M", "AT-ShotPut-M"), function(ev) {
    ori <- if (ev == "AT-ShotPut-M") 1 else -1
    data.table::data.table(
      athlete_id = paste0("a_", ev), event_id = ev,
      date = as.Date("2024-01-01") + c(0, 40, 80, 120),
      mark = if (ori < 0) c(10.4, 10.3, 10.2, 10.1) else c(20.2, 20.3, 20.4, 20.5),
      orientation = ori,
      round = c("heat", "final", "heat", "final"),
      tier = c("F", "OW", "F", "OW"))
  }))
  h[, perf := orientation * log(mark)]
  cal <- list(round = data.frame(round_class = c("final", "heat"),
                                 precision = c(0.8, 1.6)),
              tier  = data.frame(tier_class = c("low", "top"),
                                 precision = c(1.1, 0.9)))
  a1 <- estimate_ability(h, adjust_context = FALSE, as_of = as.Date("2024-09-01"),
                         calibration = cal)
  a0 <- estimate_ability(h, adjust_context = FALSE, as_of = as.Date("2024-09-01"),
                         calibration = cal, precision_scale = 0)
  expect_false(isTRUE(all.equal(a1$ability, a0$ability)))

  # per-event: only the 100m is flattened, so the shot put must match a1
  tb <- data.frame(event_id = "AT-100Metres-M", precision_scale = 0)
  am <- estimate_ability(h, adjust_context = FALSE, as_of = as.Date("2024-09-01"),
                         calibration = cal, precision_scale = tb)
  m <- merge(a1[, .(event_id, full = ability)],
             merge(a0[, .(event_id, flat = ability)],
                   am[, .(event_id, mixed = ability)], by = "event_id"), by = "event_id")
  expect_equal(m[event_id == "AT-ShotPut-M"]$mixed, m[event_id == "AT-ShotPut-M"]$full)
  expect_equal(m[event_id == "AT-100Metres-M"]$mixed, m[event_id == "AT-100Metres-M"]$flat)
})
