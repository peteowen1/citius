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
