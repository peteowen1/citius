test_that("parse_mark handles every format the feeds emit", {
  expect_equal(parse_mark("9.58"), 9.58)
  expect_equal(parse_mark("3:37.84"), 217.84)
  expect_equal(parse_mark("2:03:21"), 7401)
  expect_equal(parse_mark("8.95"), 8.95)
  expect_equal(parse_mark("9058"), 9058)
})

test_that("annotation suffixes are stripped, not parsed as digits", {
  expect_equal(parse_mark("9.69w"), 9.69)   # wind-aided
  expect_equal(parse_mark("10.2h"), 10.2)   # hand-timed
  expect_equal(parse_mark("8.95i"), 8.95)   # indoor
})

test_that("non-marks return NA rather than a misleading number", {
  # A DNF is not a slow time. Coercing it to one would corrupt ability estimates.
  expect_true(all(is.na(parse_mark(c("DNF", "DQ", "NM", "DNS", "NH")))))
  expect_true(is.na(parse_mark("")))
})

test_that("format_mark round-trips parse_mark", {
  x <- c(9.58, 217.84, 7401)
  expect_equal(parse_mark(format_mark(x)), x)
})

test_that("to_perf orients both directions so higher is always better", {
  # Faster time -> higher performance
  p <- to_perf(c(9.58, 10.20), orientation = -1)
  expect_gt(p[1], p[2])

  # Longer throw -> higher performance
  p <- to_perf(c(74.35, 70.10), orientation = 1)
  expect_gt(p[1], p[2])
})

test_that("perf_to_mark inverts to_perf in both orientations", {
  expect_equal(perf_to_mark(to_perf(9.58, -1), -1), 9.58)
  expect_equal(perf_to_mark(to_perf(8.95, 1), 1), 8.95)
})

test_that("to_perf rejects an invalid orientation", {
  expect_error(to_perf(9.58, orientation = 0), "orientation")
})

test_that("non-positive marks yield NA rather than -Inf", {
  expect_true(is.na(to_perf(0, -1)))
  expect_true(is.na(to_perf(-5, -1)))
})

test_that("the display string wins over a corrupted performanceValue", {
  # Upstream drops trailing zeros: a 6.00m vault arrives as performanceValue 6,
  # which would convert to 6cm. The string is authoritative.
  expect_equal(citius:::.resolve_mark("6.00", 6, TRUE), 6.00)
  expect_equal(citius:::.resolve_mark("5.50", 550, TRUE), 5.50)
  expect_equal(citius:::.resolve_mark("9.58", 9580, FALSE), 9.58)
  expect_equal(citius:::.resolve_mark("3:37.84", 217840, FALSE), 217.84)
})

test_that("performanceValue is used only when the string is unusable", {
  expect_equal(citius:::.resolve_mark(NA_character_, 550, TRUE), 5.50)
  expect_equal(citius:::.resolve_mark("DNF", 0, FALSE), 0)
})
