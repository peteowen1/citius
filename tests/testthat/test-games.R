make_ability <- function() {
  data.table::data.table(
    athlete_id = as.character(1:10),
    event_id = "AT-100Metres-M",
    ability = to_perf(seq(9.80, 10.25, length.out = 10), -1L),
    sigma = 0.01,
    n = c(rep(10L, 9), 1L)          # last athlete has a single result
  )
}

countries <- function() {
  data.table::data.table(
    athlete_id = as.character(1:10),
    country = c("JAM", "USA", "AUS", "KEN", "ETH", "GBR", "CAN", "NGR", "JPN", "RSA")
  )
}

test_that("the Commonwealth list covers the major athletics nations", {
  n <- commonwealth_nations()
  for (c_ in c("AUS", "JAM", "KEN", "NGR", "RSA", "CAN", "IND", "NZL", "GBR")) {
    expect_true(c_ %in% n)
  }
  expect_false("USA" %in% n)
  expect_false("ETH" %in% n)
  expect_false("JPN" %in% n)
})

test_that("GBR is eligible because World Athletics does not split home nations", {
  expect_true("GBR" %in% commonwealth_nations())
  # The home nation codes are also present for feeds that do split them
  for (c_ in c("ENG", "SCO", "WAL", "NIR")) expect_true(c_ %in% commonwealth_nations())
})

test_that("project_field selects the best eligible athletes", {
  f <- project_field(make_ability(), "AT-100Metres-M",
                     nations = commonwealth_nations(),
                     athlete_countries = countries(), size = 4)
  expect_equal(nrow(f), 4)
  expect_true(all(f$country %in% commonwealth_nations()))
  # Non-Commonwealth athletes excluded regardless of ability
  expect_false("2" %in% f$athlete_id)   # USA, second-fastest
  expect_false("5" %in% f$athlete_id)   # ETH
  # Best eligible first
  expect_equal(f$athlete_id[1], "1")    # JAM, fastest overall
})

test_that("field ordering is by ability, best first", {
  f <- project_field(make_ability(), "AT-100Metres-M", size = 5)
  expect_equal(f$ability, sort(f$ability, decreasing = TRUE))
})

test_that("athletes with too few results are excluded", {
  # A single fluke mark must not put someone in a projected final.
  f <- project_field(make_ability(), "AT-100Metres-M", size = 10, min_results = 3)
  expect_false("10" %in% f$athlete_id)
  expect_equal(nrow(f), 9)
})

test_that("nations filter requires nationality data", {
  expect_error(
    project_field(make_ability(), "AT-100Metres-M", nations = c("AUS")),
    "athlete_countries"
  )
})

test_that("an event with no eligible athletes returns zero rows, not an error", {
  f <- project_field(make_ability(), "AT-100Metres-M", nations = "NRU",
                     athlete_countries = countries())
  expect_equal(nrow(f), 0)
})

test_that("an unknown event returns zero rows", {
  expect_equal(nrow(project_field(make_ability(), "AT-Nonsense-M")), 0)
})

test_that("athletes with stale records are excluded from projected fields", {
  # An estimate built on a decade-old junior record describes someone who no
  # longer competes; projecting it forward compounds the error.
  today <- as.Date("2026-07-30")
  ab <- data.table::data.table(
    athlete_id = c("current", "stale"),
    event_id = "AT-100Metres-M",
    ability = to_perf(c(10.10, 9.85), -1L),   # stale athlete looks faster
    sigma = 0.01, n = 20L,
    last_date = c(today - 60, today - 12 * 365)
  )
  f <- project_field(ab, "AT-100Metres-M", size = 8, as_of = today,
                     max_stale_years = 2)
  expect_equal(f$athlete_id, "current")
})

test_that("staleness filtering can be disabled", {
  today <- as.Date("2026-07-30")
  ab <- data.table::data.table(
    athlete_id = c("a", "b"), event_id = "AT-100Metres-M",
    ability = to_perf(c(10.10, 9.85), -1L), sigma = 0.01, n = 20L,
    last_date = c(today - 60, today - 12 * 365))
  f <- project_field(ab, "AT-100Metres-M", as_of = today, max_stale_years = Inf)
  expect_equal(nrow(f), 2)
})
