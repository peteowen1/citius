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


test_that("predicted_mark round-trips the marks it was validated against", {
  # These are the reference values the blog export was checked against by hand
  # before it shipped: if a change to the formatter moves any of them, a mark
  # already published on the site has silently changed.
  rt <- function(mark, o) predicted_mark(to_perf(mark, o), o)$mark
  expect_equal(rt(9.84, -1), "9.84")
  expect_equal(rt(116.30, -1), "1:56.30")
  expect_equal(rt(7709, -1), "2:08:29")
  expect_equal(rt(6.03, 1), "6.03")
  expect_equal(rt(1.98, 1), "1.98")
  expect_equal(rt(8813, 1), "8,813")
})

test_that("predicted_mark picks the unit off the orientation, not the value", {
  expect_equal(predicted_mark(to_perf(9.84, -1), -1)$unit, "")
  expect_equal(predicted_mark(to_perf(6.03, 1), 1)$unit, "m")
  expect_equal(predicted_mark(to_perf(8813, 1), 1)$unit, "pts")
})

test_that("predicted_mark switches format at the minute and hour boundaries", {
  rt <- function(mark, o) predicted_mark(to_perf(mark, o), o)$mark
  expect_equal(rt(59.99, -1), "59.99")
  expect_equal(rt(60.01, -1), "1:00.01")
  expect_equal(rt(3599.99, -1), "59:59.99")
  # Deliberately not a x.5-second value: the log/exp round-trip lands a hair
  # either side of an exact half, so a half-second input tests floating point
  # rather than the formatting branch this case is about.
  expect_equal(rt(3661, -1), "1:01:01")
})

test_that("predicted_mark vectorises over mixed orientations", {
  # The exports call this on a whole table at once, where track and field rows
  # sit side by side. A scalar-only implementation would recycle the first
  # orientation across every row and silently format throws as times.
  out <- predicted_mark(c(to_perf(9.84, -1), to_perf(6.03, 1)), c(-1L, 1L))
  expect_equal(out$mark, c("9.84", "6.03"))
  expect_equal(out$unit, c("", "m"))
})

test_that("a non-finite ability yields NA, not a zero-second race", {
  # exp(-Inf) is 0, which is finite and would format as "0.00" — a nonsense
  # mark that reads exactly like a real one.
  expect_true(all(is.na(predicted_mark(c(NA_real_, Inf, -Inf), -1)$mark)))
})

test_that("a seconds field that rounds up to 60 carries instead of printing '60'", {
  # citius#9. The old implementation split the value into fields and rounded
  # each one independently, so a seconds component landing on exactly 60 had
  # nothing above it to carry into and was simply printed. Four marathon
  # predictions shipped to the live site as 2:05:60, 2:15:60, 2:28:60 and
  # 2:29:60 -- clock times that cannot exist.
  #
  # The existing boundary test above passes on the broken code: every value it
  # chose (59.99, 60.01, 3599.99, 3661) sits OUTSIDE the rounding band. These
  # sit inside it. Values are kept a few thousandths clear of the exact
  # half-way point on purpose -- the log/exp round-trip in to_perf() lands a
  # hair either side of an exact half, so a knife-edge input would be testing
  # floating point rather than the carry.
  rt <- function(mark, o) predicted_mark(to_perf(mark, o), o)$mark
  expect_equal(rt(59.999, -1), "1:00.00")    # was "60.00"
  expect_equal(rt(119.999, -1), "2:00.00")   # was "1:60.00"
  expect_equal(rt(7259.7, -1), "2:01:00")    # was "2:00:60"
  expect_equal(rt(3719.7, -1), "1:02:00")    # was "1:01:60"
  expect_equal(rt(7559.7, -1), "2:06:00")    # the live 2:05:60 case
})

test_that("no formatted time ever contains a field of 60 or more", {
  # A property, not a case list: the bug was only ever found because someone
  # happened to look at a marathon. Sweeping the whole plausible range makes
  # the next variant of it fail here instead of on the site. The old code
  # fails this within a few thousand draws.
  set.seed(20260809)
  v <- c(runif(20000, 9, 3600), runif(20000, 3600, 12000),
         # concentrate on the bands where a carry is actually due
         59.9 + runif(2000, 0, 0.1), 3599.9 + runif(2000, 0, 0.1),
         7259.5 + runif(2000, 0, 0.5))
  marks <- predicted_mark(to_perf(v, -1), -1)$mark
  # A BARE mark must be checked too, not skipped for having no colon. The
  # quieter half of this bug printed "60.00" with no colon at all: the branch
  # was chosen on `v` while the digits came from a rounded value, so 59.996 took
  # the under-a-minute path. An earlier version of this test returned FALSE for
  # any single-field mark, so it could not see that variant -- fed 116 genuinely
  # broken "60.00"s it reported zero. Every mark here is a time (orientation -1
  # throughout), so a bare value of 60 or more is always wrong.
  bad <- vapply(marks, function(m) {
    if (is.na(m)) return(FALSE)
    f <- strsplit(m, ":", fixed = TRUE)[[1L]]
    n <- suppressWarnings(as.numeric(if (length(f) == 1L) f else f[-1L]))
    any(!is.na(n) & n >= 60)
  }, logical(1))
  expect_equal(sum(bad), 0L, info = paste("examples:",
    paste(utils::head(marks[bad], 5), collapse = " ")))
})

test_that("an absurd but finite ability does not format as the string 'NA:NA:NA'", {
  # Guarding the INPUT for non-finiteness is not enough: a large finite value
  # used to overflow as.integer() and print "NA:NA:NA" -- a string, so
  # `is.na(mark)` reads FALSE and every downstream filter for bad predictions
  # passes it through. Whatever comes back here must be either a real NA or a
  # well-formed time, never the word NA embedded in one.
  # Swept far past the old int32 cliff AND past the point where double modulus
  # stops being exact. An earlier version of this test stopped at -30 to stay
  # inside the exact range, which meant it asserted the fix worked exactly where
  # the fix was never in doubt. At -94.75 the unclamped code produced a minutes
  # field of 1092, and at -97.25 a NEGATIVE one -- malformed, and containing no
  # "NA" for this test's first assertion to catch.
  m <- predicted_mark(c(-21, -22, -30, -34.5, -60, -94.75, -97.25, -400), -1)$mark
  expect_false(any(grepl("NA", m[!is.na(m)], fixed = TRUE)))
  ok <- is.na(m) | grepl("^[0-9]+:[0-5][0-9]:[0-5][0-9]$", m)
  expect_true(all(ok), info = paste("got:", paste(m, collapse = " ")))
})

test_that("the mark cap trims only garbage, never a real performance", {
  # A cap is only safe if it cannot reach anything genuine. The longest events
  # anyone actually contests sit six orders of magnitude below it.
  rt <- function(mark, o) predicted_mark(to_perf(mark, o), o)$mark
  expect_equal(rt(9.58, -1), "9.58")            # 100m world record
  expect_equal(rt(7235, -1), "2:00:35")         # marathon, ~2 hours
  expect_false(is.na(rt(518400, -1)))           # a six-day race, 518,400s
  expect_false(is.na(rt(9.9e6, -1)))            # 114 days, still formatted
  expect_true(is.na(rt(1.1e7, -1)))             # past the cap, NA not a string
})
