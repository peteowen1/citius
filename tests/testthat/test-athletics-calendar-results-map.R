.fake_track_result <- function(competition_id, athlete_id, athlete_name, race_id,
                               race_number, round, mark, place, wind = 0) {
  data.table::data.table(
    competition_id = competition_id, event_id = "e1", event_name = "100 Metres",
    sex_code = "M", athlete_id = athlete_id, athlete_name = athlete_name,
    birthdate = as.Date(NA), race_date = as.Date(NA), comp_start = as.Date("2026-01-01"),
    race_id = race_id, race_number = race_number, round = round,
    mark = mark, place = place, wind = wind, is_relay = FALSE,
    venue = "Test Stadium, Testville (GBR)", comp_name = "Test Meet"
  )
}

test_that("race_key does not collapse two races sharing race_id/race_number when round differs", {
  # The 2026-07-27 Glasgow incident: raceNumber sentinel repeated across an
  # event's races silently pooled distinct heats into one race_key. round
  # (the response's own race label) is real supplied data that survives
  # even when race_number doesn't discriminate.
  fake <- rbind(
    .fake_track_result(999L, 1:3, paste0("A", 1:3), 10L, 1L, "Heat 1",
                       c("10.20","10.30","10.40"), c("1.","2.","3."), 0.5),
    .fake_track_result(999L, 4:6, paste0("A", 4:6), 10L, 1L, "Heat 2",
                       c("10.50","10.60","10.70"), c("1.","2.","3."), -0.2)
  )
  mapped <- map_calendar_results_to_championship_schema(fake)
  expect_equal(data.table::uniqueN(mapped$race_key), 2L)
  expect_false(anyNA(mapped$race_key))
})

test_that("race_key falls back to a content hash when race_id AND race_number both collide", {
  fake <- rbind(
    .fake_track_result(999L, 1:3, paste0("A", 1:3), 10L, 1L, "Heat",
                       c("10.20","10.30","10.40"), c("1.","2.","3."), 0.5),
    .fake_track_result(999L, 4:6, paste0("A", 4:6), 10L, 1L, "Heat 2",
                       c("10.50","10.60","10.70"), c("1.","2.","3."), -0.2)
  )
  mapped <- map_calendar_results_to_championship_schema(fake)
  expect_equal(data.table::uniqueN(mapped$race_key), 2L)
})

test_that("the residual worst case (race_id, race_number, AND round all identical) is a documented, not silently-wrong, limit", {
  # Not a pass/fail assertion on correctness -- this documents the known,
  # honestly-unfixable gap in the roxygen: when every available
  # discriminator collides, the function cannot separate two real races,
  # because the flattened input has already lost the information that
  # would. This test exists so a future change that accidentally makes it
  # WORSE (e.g. silently drops rows, or errors) is caught, not to assert the
  # collapse is fine.
  fake <- rbind(
    .fake_track_result(997L, 1:3, paste0("A", 1:3), 10L, 1L, "Heat",
                       c("10.20","10.30","10.40"), c("1.","2.","3."), 0.5),
    .fake_track_result(997L, 4:6, paste0("A", 4:6), 10L, 1L, "Heat",
                       c("10.50","10.60","10.70"), c("1.","2.","3."), -0.2)
  )
  mapped <- map_calendar_results_to_championship_schema(fake)
  expect_equal(nrow(mapped), 6L)  # no rows silently dropped
  expect_equal(data.table::uniqueN(mapped$race_key), 1L)  # the documented collapse
})

test_that("contradictory track-event rows (same athlete, same race, disagreeing mark/place) are dropped, not guessed at", {
  fake <- rbind(
    .fake_track_result(998L, c(1L, 1L), c("Dup", "Dup"), 20L, 1L, "Final",
                       c("20.10", "20.10"), c("1.", "3."), 0.1),
    .fake_track_result(998L, 2L, "Other", 20L, 1L, "Final", "20.50", "2.", 0.1)
  )
  expect_warning(
    mapped <- map_calendar_results_to_championship_schema(fake),
    "disagreeing mark/place"
  )
  expect_equal(nrow(mapped), 1L)
  expect_equal(mapped$athlete_id, 2L)
})

test_that("field-event multi-attempt rows for one athlete are kept, not treated as contradictory", {
  fake <- data.table::data.table(
    competition_id = 996L, event_id = "e2", event_name = "Long Jump", sex_code = "M",
    athlete_id = c(1L, 1L, 1L), athlete_name = "Jumper",
    birthdate = as.Date(NA), race_date = as.Date(NA), comp_start = as.Date("2026-01-01"),
    race_id = 30L, race_number = 1L, round = "Final",
    mark = c("7.50", "7.60", "7.55"), place = c("1.", "1.", "1."), wind = 0,
    is_relay = FALSE, venue = "Test Stadium, Testville (GBR)", comp_name = "Test Meet"
  )
  mapped <- map_calendar_results_to_championship_schema(fake)
  expect_equal(nrow(mapped), 3L)  # all three attempts kept
  expect_equal(data.table::uniqueN(mapped$race_key), 1L)  # same race
})

test_that(".drop_contradictory_track_rows() does not collapse two different athletes who both have NA athlete_id", {
  # Regression for the 2026-09-04 review finding: the contradiction check
  # grouped by (race_key, athlete_id), and athlete_id is NA whenever a
  # competitor's urlSlug fails to parse -- not rare. Two DIFFERENT real
  # athletes with unparseable slugs in the same race used to land in one
  # (race_key, NA) group; their marks genuinely differ (different people), so
  # the group read as "disagreeing mark/place" and BOTH athletes' real
  # results were silently dropped.
  fake <- .fake_track_result(999L, athlete_id = c(NA_integer_, NA_integer_),
                             athlete_name = c("Unparseable One", "Unparseable Two"),
                             race_id = 10L, race_number = 1L, round = "Heat 1",
                             mark = c("10.20", "10.55"), place = c("1.", "2."))
  mapped <- map_calendar_results_to_championship_schema(fake)
  expect_equal(nrow(mapped), 2L)  # neither real athlete's result was dropped
  expect_true(all(is.na(mapped$athlete_id)))

  # Control: the ORIGINAL bug this function exists to catch must still work --
  # a single real athlete_id with genuinely disagreeing mark/place in the same
  # race is still dropped.
  fake_real <- .fake_track_result(999L, athlete_id = c(7L, 7L),
                                  athlete_name = c("Real Athlete", "Real Athlete"),
                                  race_id = 11L, race_number = 1L, round = "Heat 1",
                                  mark = c("10.23", "10.99"), place = c("1.", "3."))
  mapped_real <- map_calendar_results_to_championship_schema(fake_real)
  expect_equal(nrow(mapped_real), 0L)  # the genuine contradiction is still caught
})

test_that("map_calendar_results_to_championship_schema() works on real, current data", {
  skip_if_offline("worldathletics.org")
  r <- athletics_calendar_results(7214476)
  mapped <- map_calendar_results_to_championship_schema(r)
  expect_true(nrow(mapped) > 0)
  expect_false(anyNA(mapped$race_key))
  expect_true(all(c("competition_id", "athlete_id", "mark", "race_key") %in% names(mapped)))
})
