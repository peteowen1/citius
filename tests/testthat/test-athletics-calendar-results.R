# Same live-network convention as test-athletics-calendar.R: no mocking
# infrastructure exists in this package for these endpoints, skip_if_offline()
# is the standard testthat approach.

test_that("athletics_calendar_results() returns real results for a known competition", {
  skip_if_offline("worldathletics.org")
  r <- athletics_calendar_results(7214476)  # 7th Irena Szewinska Memorial
  expect_true(nrow(r) > 0)
  expect_true(attr(r, "fetch_ok"))
  expect_true(all(c("competition_id", "comp_name", "event_name", "athlete_id",
                     "athlete_name", "mark", "place") %in% names(r)))
  expect_equal(unique(r$competition_id), 7214476L)
  # At least one gold-medal-shaped row should have a real mark and a
  # resolvable athlete_id, not just NAs throughout.
  expect_true(any(!is.na(r$mark)))
  expect_true(any(!is.na(r$athlete_id)))
})

test_that("athletics_calendar_results() resolves a competition the community wrapper cannot", {
  skip_if_offline("worldathletics.org")
  # 7147603 is one of 11 competitions confirmed 2026-08-30 to consistently
  # fail with HTTP 500 from worldathletics.nimarion.de, verified NOT stale
  # (World Athletics' own site resolves it at this same id with
  # hasResults = TRUE). This endpoint is a different first-party surface
  # and resolves it cleanly -- the actual proof this function solves the
  # problem it was built for, not just a happy-path smoke test.
  r <- athletics_calendar_results(7147603)
  expect_true(nrow(r) > 0)
  expect_true(attr(r, "fetch_ok"))
})

test_that("athletics_calendar_results() aborts loudly, not silently, on a genuinely broken id", {
  skip_if_offline("worldathletics.org")
  # Unlike the calendar/profile endpoints (which return a definitive 404 for
  # an unresolvable id), this endpoint returns HTTP 500 for competition_id=1
  # -- verified 2026-08-30. citius_get_html() aborts loudly on any non-404
  # error status rather than swallowing it, which is the correct behavior
  # here: a real server error must not be silently reported as "confirmed,
  # no results." This is the same failure signature already found on 4 of
  # the 26 target competitions (7189491, 7189494, 7204909, 7213160) -- this
  # endpoint resolves most of the wrapper's stuck set but not all of it, and
  # says so loudly rather than hiding the remainder.
  expect_error(athletics_calendar_results(1), "500")
})
