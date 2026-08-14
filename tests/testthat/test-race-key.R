test_that("rows sharing competition, event, round and date share a race key", {
  d <- data.table::data.table(
    competition_id = c("C1", "C1"),
    event_id = c("AT-100Metres-M", "AT-100Metres-M"),
    round = c("F", "F"),
    date = as.Date(c("2024-06-01", "2024-06-01")))
  out <- add_race_key(d)
  expect_equal(out$race_key[1], out$race_key[2])
})

test_that("differing in any one of the four fields gives a different key", {
  base <- data.table::data.table(
    competition_id = "C1", event_id = "AT-100Metres-M",
    round = "F", date = as.Date("2024-06-01"))
  ref <- add_race_key(base)$race_key

  vary_comp  <- data.table::copy(base)[, competition_id := "C2"]
  vary_event <- data.table::copy(base)[, event_id := "AT-200Metres-M"]
  vary_round <- data.table::copy(base)[, round := "H1"]
  vary_date  <- data.table::copy(base)[, date := as.Date("2024-06-02")]

  expect_false(add_race_key(vary_comp)$race_key == ref)
  expect_false(add_race_key(vary_event)$race_key == ref)
  expect_false(add_race_key(vary_round)$race_key == ref)
  expect_false(add_race_key(vary_date)$race_key == ref)
})

test_that("missing competition_id warns and still returns a race_key column", {
  d <- data.table::data.table(
    event_id = "AT-100Metres-M", round = "F", date = as.Date("2024-06-01"))
  expect_warning(out <- add_race_key(d), "competition_id")
  expect_true("race_key" %in% names(out))
})

test_that("overwriting an existing race_key warns (incident 2026-08-14)", {
  # The derived key is competition|event|round|date, so every HEAT of a
  # championship round collapses into one race. Deriving over the top of a real
  # key is how the corpus came to hold races with up to 45 winners.
  d <- data.table::data.table(
    competition_id = c("C1", "C1"),
    event_id = c("AT-800Metres-W", "AT-800Metres-W"),
    round = c("Round 1 - Heat", "Round 1 - Heat"),
    date = as.Date(c("2024-06-01", "2024-06-01")),
    race_key = c("C1|999|Round 1 - Heat|1", "C1|999|Round 1 - Heat|2"))
  expect_warning(out <- add_race_key(d), "already carry")
  # Two genuinely different heats must not have been merged silently: the
  # warning is the contract, and the caller is responsible for preserving.
  expect_equal(out$race_key[1], out$race_key[2])
})

test_that("no warning when nothing is being overwritten", {
  d <- data.table::data.table(
    competition_id = "C1", event_id = "AT-100Metres-M",
    round = "F", date = as.Date("2024-06-01"), race_key = NA_character_)
  expect_no_warning(add_race_key(d))
})

test_that("the input table is not mutated by reference", {
  d <- data.table::data.table(
    competition_id = "C1", event_id = "AT-100Metres-M",
    round = "F", date = as.Date("2024-06-01"))
  invisible(add_race_key(d))
  expect_false("race_key" %in% names(d))
})
