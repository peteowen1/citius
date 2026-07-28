test_that("name order does not matter", {
  # The bug this exists for: World Aquatics writes "SJOESTROEM Sarah", the
  # Commonwealth Games system writes "Hannah STERRY".
  expect_equal(athlete_key("Hannah STERRY"), athlete_key("STERRY Hannah"))
  expect_equal(athlete_key("Mohamed Aan HUSSAIN"), athlete_key("HUSSAIN Mohamed Aan"))
  expect_equal(athlete_key("Amna Thazkiyah MIRSAAD"),
               athlete_key("MIRSAAD Amna Thazkiyah"))
})

test_that("punctuation that varies between feeds is ignored", {
  expect_equal(athlete_key("O'BRIEN Sean"), athlete_key("OBRIEN Sean"))
  expect_equal(athlete_key("AL-SAID Ahmed"), athlete_key("AL SAID Ahmed"))
})

test_that("different athletes do not collide", {
  expect_false(athlete_key("SMITH John") == athlete_key("SMITH Jane"))
  expect_false(athlete_key("JACKSON Abeku") == athlete_key("JACKSON Abena"))
  # A missing middle name is a DIFFERENT key, deliberately. Guessing that
  # "HUSSAIN Mohamed" is "HUSSAIN Mohamed Aan" would silently merge two careers
  # into one ability estimate, which nothing downstream could detect.
  expect_false(athlete_key("HUSSAIN Mohamed") == athlete_key("HUSSAIN Mohamed Aan"))
})

test_that("empty and missing names give NA rather than a joinable key", {
  expect_true(is.na(athlete_key("")))
  expect_true(is.na(athlete_key("   ")))
  expect_true(is.na(athlete_key(NA_character_)))
  # An NA key must never join to another NA key.
  k <- athlete_key(c("", "  ", NA_character_))
  expect_true(all(is.na(k)))
})

test_that("it is vectorised and order-preserving", {
  x <- c("Hannah STERRY", "STERRY Hannah", "SMITH John")
  k <- athlete_key(x)
  expect_length(k, 3L)
  expect_equal(k[1], k[2])
  expect_false(k[1] == k[3])
})
