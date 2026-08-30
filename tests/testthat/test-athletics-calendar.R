# No existing convention in this package for live-network tests (no prior
# test file touches source_athletics.R at all) -- skip_if_offline() is the
# standard testthat approach, used here rather than inventing a new one.

test_that("athletics_calendar() returns real, mostly date-filtered results", {
  skip_if_offline("worldathletics.org")
  cal <- athletics_calendar(start_date = "2015-01-01", end_date = "2015-12-31")
  expect_true(nrow(cal) > 0)
  expect_equal(attr(cal, "hits"), nrow(cal))  # under 100 hits, single page
  # The endpoint's own date filter is NOT strictly enforced -- confirmed
  # 2026-08-30, a 2015-01-01/2015-12-31 query returned at least one 2007
  # result. Assert the filter does most of the work (the large majority in
  # range) rather than a guarantee the API itself doesn't make; a caller
  # needing a hard boundary must post-filter on start_date.
  in_range <- cal$start_date >= as.Date("2015-01-01") & cal$start_date <= as.Date("2015-12-31")
  expect_gt(mean(in_range), 0.9)
  expect_true(all(c("competition_id", "name", "start_date") %in% names(cal)))
})

test_that("athletics_calendar() with no matches returns a well-formed empty table", {
  skip_if_offline("worldathletics.org")
  cal <- athletics_calendar(query = "zzzznonexistentcompetitionxyz123")
  expect_equal(nrow(cal), 0L)
  expect_true(all(c("competition_id", "name", "start_date") %in% names(cal)))
  # A confirmed zero-hit query is fetch_ok = TRUE -- distinct from a blocked/
  # failed request, which also returns zero rows but fetch_ok = FALSE. Found
  # missing in review 2026-08-30: both used to be indistinguishable.
  expect_true(attr(cal, "fetch_ok"))
})

test_that("athletics_calendar() marks a real, successful fetch as fetch_ok = TRUE", {
  skip_if_offline("worldathletics.org")
  cal <- athletics_calendar(start_date = "2015-01-01", end_date = "2015-12-31")
  expect_true(attr(cal, "fetch_ok"))
})

test_that("athletics_calendar_all() pages correctly past the 100-row page size", {
  skip_if_offline("worldathletics.org")
  cal <- athletics_calendar_all(start_date = "2026-08-20", end_date = "2026-08-30", throttle = 0.5)
  expect_true(nrow(cal) > 100)
  expect_equal(nrow(cal), attr(cal, "hits"))
  expect_false(anyDuplicated(cal$competition_id) > 0)
  # A clean multi-page pull should report itself complete, with nothing
  # dropped and no failed pages -- the invariant the function used to only
  # document, not verify (review 2026-08-30).
  expect_true(attr(cal, "complete"))
  expect_length(attr(cal, "failed_offsets"), 0)
  expect_equal(attr(cal, "dropped_bad_id"), 0L)
})

test_that("athletics_athlete_official_profile() resolves a known athlete", {
  skip_if_offline("worldathletics.org")
  prof <- athletics_athlete_official_profile(14679502)  # Armand Duplantis
  expect_equal(nrow(prof), 1L)
  expect_equal(prof$family_name, "DUPLANTIS")
  expect_equal(prof$given_name, "Armand")
  expect_equal(prof$country_code, "SWE")
  expect_equal(prof$sex, "M")
  expect_equal(prof$birthdate, as.Date("1999-11-10"))
})

test_that("athletics_athlete_official_profile() marks a real, successful fetch as fetch_ok = TRUE", {
  skip_if_offline("worldathletics.org")
  prof <- athletics_athlete_official_profile(14679502)
  expect_true(attr(prof, "fetch_ok"))
})

test_that("athletics_athlete_official_profile() returns zero rows for an unresolvable id, confirmed not blocked", {
  skip_if_offline("worldathletics.org")
  prof <- athletics_athlete_official_profile(1)
  expect_equal(nrow(prof), 0L)
  expect_true(all(c("athlete_id", "family_name", "given_name") %in% names(prof)))
  # A confirmed "id does not exist" (the page rendered fine and said so) is
  # fetch_ok = TRUE -- distinct from a blocked/failed request, which also
  # returns zero rows but fetch_ok = FALSE. Found missing in review
  # 2026-08-30: a mid-run block would otherwise have silently read every
  # subsequent athlete in a batch as "does not exist."
  expect_true(attr(prof, "fetch_ok"))
})
