test_that("the registry covers both sports for both sexes", {
  ev <- citius_events()
  expect_true(nrow(ev) > 60)
  expect_setequal(unique(ev$sport), c("Athletics", "Swimming"))
  expect_setequal(unique(ev$sex), c("M", "W"))
  expect_false(anyDuplicated(ev$event_id) > 0)
})

test_that("orientation is set correctly per event type", {
  ev <- citius_events()
  expect_true(all(ev[ev$unit == "seconds", ]$orientation == -1L))
  expect_true(all(ev[ev$unit %in% c("metres", "points"), ]$orientation == 1L))
})

test_that("tactical and technical flags mark the right events", {
  ev <- citius_events()
  get <- function(id) ev[ev$event_id == id, ]

  # Distance track races decouple time from placing; sprints and swims do not.
  expect_true(get("AT-1500Metres-M")$tactical)
  expect_true(get("AT-5000Metres-W")$tactical)
  expect_false(get("AT-100Metres-M")$tactical)
  expect_false(get("SW-1500mFreestyle-M")$tactical)

  # Field events have a discrete failure mode; track events do not.
  expect_true(get("AT-PoleVault-M")$technical)
  expect_true(get("AT-JavelinThrow-W")$technical)
  expect_false(get("AT-400Metres-M")$technical)
})

test_that("swimming carries lower variance priors than athletics", {
  ev <- citius_events()
  swim <- mean(ev[ev$sport == "Swimming", ]$cv_prior)
  track <- mean(ev[ev$sport == "Athletics", ]$cv_prior)
  expect_lt(swim, track)
})

test_that("match_event normalises the spellings each feed uses", {
  expect_equal(match_event("100 Metres", "M"), "AT-100Metres-M")
  expect_equal(match_event("Men's 100m Freestyle", "Men"), "SW-100mFreestyle-M")
  expect_equal(match_event("10,000 Metres", "W"), "AT-10000Metres-W")
  expect_equal(match_event("10000 Metres", "W"), "AT-10000Metres-W")
  expect_equal(match_event("Women's 200m Individual Medley", "W"), "SW-200mIndividualMedley-W")
})

test_that("sex codes are accepted in every form the feeds use", {
  expect_equal(match_event("100 Metres", "Women"), "AT-100Metres-W")
  expect_equal(match_event("100 Metres", "F"), "AT-100Metres-W")
  expect_equal(match_event("100 Metres", "W"), "AT-100Metres-W")
})

test_that("unmatched events return NA instead of a plausible guess", {
  # Silently snapping an unknown event onto a neighbour would corrupt histories.
  expect_true(is.na(match_event("Underwater Basket Weaving", "M")))
  expect_true(is.na(match_event("110 Metres Hurdles", "W")))  # not a women's event
})
