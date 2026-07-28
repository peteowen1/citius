test_that("SwimCloud event names translate to registry disciplines", {
  e <- swimcloud_event(c("50 Breast Women", "200 IM Men", "1500 Free Men",
                         "100 Fly Women", "200 Back Women"))
  expect_equal(e$discipline, c("50m Breaststroke", "200m Individual Medley",
                               "1500m Freestyle", "100m Butterfly",
                               "200m Backstroke"))
  expect_equal(e$sex, c("F", "M", "M", "F", "F"))
})

test_that("relays are NOT translated into individual events", {
  # "400 Medley Relay" contains "Medley" and mapped onto "400m Individual
  # Medley", which would have entered relay times as individual swims. The
  # mixed relay escaped only because its sex is neither M nor F; the men's
  # relay would have matched cleanly and corrupted the event silently.
  e <- swimcloud_event(c("400 Medley Relay Mixed", "400 Medley Relay Men",
                         "800 Free Relay Women", "200 Free Relay Men"))
  expect_true(all(is.na(e$discipline)))
  expect_true(all(is.na(e$sex)))
})

test_that("other aquatic sports on the same pages are not translated", {
  e <- swimcloud_event(c("3M Diving Men", "10M Platform Women",
                         "Open Water 5k Men"))
  expect_true(all(is.na(e$discipline)))
})

test_that("unrecognised events return NA rather than a guess", {
  # Same contract as match_event(): refusing to guess is what surfaced the
  # missing 50m Butterfly registry entries rather than silently mis-filing them.
  e <- swimcloud_event(c("500 Free Men", "50 IM Women", "Some Novelty Event"))
  expect_equal(e$discipline, c("500m Freestyle", "50m Individual Medley", NA))
  # 500 Free and 50 IM are real names but absent from the registry, so
  # match_event() is what rejects them -- not this translation layer.
  expect_true(is.na(match_event("500m Freestyle", "M")))
})
