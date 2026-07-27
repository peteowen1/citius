make_export <- function(path) {
  obj <- list(
    p = list(
      list("athletic-result/SWM/ST/W/100MBR------------/FNL-/000100--",
           "26 Jul 2026, 19:20  |  Finished  •  Tollcross Swimming Centre",
           "WOMEN'S 100M BREASTSTROKE FINAL"),
      list("athletic-result/SWM/ST/M/400MFR------------/HEAT/--------",
           "25 Jul 2026, 11:49  |  Finished  •  Tollcross Swimming Centre",
           "MEN'S 400M FREESTYLE - HEATS"),
      list("athletic-result/SWM/RE/M/4X100MFR----------/FNL-/000100--",
           "24 Jul 2026, 21:21  |  Finished  •  Tollcross Swimming Centre",
           "MEN'S 4 X 100M FREESTYLE RELAY FINAL")
    ),
    r = list(
      list(0, "Final", "1", "4", "SCO", "Angharad EVANS", "0.73", "1:06.07"),
      list(0, "Final", "2", "5", "RSA", "Aimee CANNY", "0.74", "1:06.19"),
      list(0, "Final", "DSQ", "8", "JAM", "Sabrina LYN", "0.71", ""),
      list(1, "Heat 2", "1", "5", "SCO", "Evi MACKIE", "0.66", "3:52.10"),
      list(2, "Final", "1", "4", "AUS", "Australia", "", "3:10.55")
    )
  )
  jsonlite::write_json(obj, path, auto_unbox = TRUE)
  path
}

test_that("a CRS export parses into the canonical schema", {
  p <- withr::local_tempfile(fileext = ".json")
  make_export(p)
  res <- parse_crs_export(p)

  expect_s3_class(res, "data.table")
  expect_equal(nrow(res), 5)
  expect_true(all(c("event_id", "mark", "perf", "place", "round",
                    "race_key", "reaction_time") %in% names(res)))
})

test_that("marks, sexes and events resolve correctly", {
  p <- withr::local_tempfile(fileext = ".json")
  make_export(p)
  res <- parse_crs_export(p)

  ev <- res[res$athlete_name == "Angharad EVANS", ]
  expect_equal(ev$mark, 66.07)
  expect_equal(ev$sex, "W")
  expect_equal(ev$event_id, "SW-100mBreaststroke-W")
  expect_equal(ev$place, 1L)
  expect_equal(ev$reaction_time, 0.73)

  mk <- res[res$athlete_name == "Evi MACKIE", ]
  expect_equal(mk$sex, "M")
  expect_equal(mk$event_id, "SW-400mFreestyle-M")
  expect_equal(mk$mark, 232.10)
})

test_that("the round label is preserved per heat", {
  p <- withr::local_tempfile(fileext = ".json")
  make_export(p)
  res <- parse_crs_export(p)
  expect_equal(res[res$athlete_name == "Evi MACKIE", ]$round, "Heat 2")
  expect_equal(res[res$athlete_name == "Aimee CANNY", ]$round, "Final")
})

test_that("a disqualification keeps the row but drops the mark", {
  # DSQ is a missing performance, not a slow one.
  p <- withr::local_tempfile(fileext = ".json")
  make_export(p)
  res <- parse_crs_export(p)
  dsq <- res[res$athlete_name == "Sabrina LYN", ]
  expect_equal(nrow(dsq), 1)
  expect_true(is.na(dsq$mark))
  expect_true(is.na(dsq$perf))
  expect_true(is.na(dsq$place))
})

test_that("relays are left unmatched rather than forced onto an event", {
  p <- withr::local_tempfile(fileext = ".json")
  make_export(p)
  res <- parse_crs_export(p)
  relay <- res[res$athlete_name == "Australia", ]
  expect_true(is.na(relay$event_id))
  expect_equal(nrow(relay), 1)   # retained, not dropped
})

test_that("race_key groups a heat together and separates heats", {
  p <- withr::local_tempfile(fileext = ".json")
  make_export(p)
  res <- parse_crs_export(p)
  fin <- res[res$round == "Final" & res$sex == "W", ]$race_key
  expect_equal(length(unique(fin)), 1)
  expect_false(unique(fin) == unique(res[res$round == "Heat 2", ]$race_key))
})

test_that("dates parse from the Games header line", {
  p <- withr::local_tempfile(fileext = ".json")
  make_export(p)
  res <- parse_crs_export(p)
  expect_equal(res[res$athlete_name == "Angharad EVANS", ]$date,
               as.Date("2026-07-26"))
})

test_that("50m stroke events exist for Commonwealth and World programmes", {
  # Not on the Olympic programme, so an Olympic-only registry misses them.
  ev <- citius_events()
  for (d in c("50m Backstroke", "50m Breaststroke", "50m Butterfly")) {
    expect_true(d %in% ev$discipline)
  }
  expect_equal(match_event("50M BUTTERFLY", "W"), "SW-50mButterfly-W")
})
