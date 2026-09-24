# as_date_safe() sits under every source adapter's date columns. A feed that
# changes its date format must be heard, not turned into a column of NA.

test_that("as_date_safe parses ISO dates and datetimes, and leaves missing alone", {
  expect_equal(as_date_safe(c("2024-08-01", "2024-08-01T19:30:00Z")),
               as.Date(c("2024-08-01", "2024-08-01")))
  expect_no_warning(out <- as_date_safe(c(NA, "", "2024-08-01")))
  expect_equal(out, as.Date(c(NA, NA, "2024-08-01")))
})

test_that("as_date_safe warns with a count when present values fail to parse", {
  expect_warning(out <- as_date_safe(c("01/08/2024", "02/08/2024", "2024-08-03")),
                 "2 date values could not be parsed")
  expect_equal(out[3], as.Date("2024-08-03"))
  expect_true(all(is.na(out[1:2])))
})
