test_that("footnote letters are stripped, spaced or glued", {
  expect_equal(canonical_nation("United States a"), "United States")
  expect_equal(canonical_nation("United Statesa"), "United States")
  expect_equal(canonical_nation("Cuba b"), "Cuba")
  expect_equal(canonical_nation("Canada c"), "Canada")
})

test_that("a real name ending in a short word survives stripping", {
  # The rule removes a trailing SINGLE letter, so multi-letter endings are safe.
  expect_equal(canonical_nation("Trinidad and Tobago"), "Trinidad and Tobago")
  expect_equal(canonical_nation("Papua New Guinea"), "Papua New Guinea")
  expect_equal(canonical_nation("Isle of Man"), "Isle of Man")
})

test_that("leaked citation CSS is removed", {
  ugly <- paste0("Greece.mw-parser-output .citation{word-wrap:break-word}",
                 ".mw-parser-output .citation:target{background-color:rgba}")
  expect_equal(canonical_nation(ugly), "Greece")
})

test_that("renames of one entity merge but successor states do not", {
  expect_equal(canonical_nation("Western Samoa"), "Samoa")
  expect_equal(canonical_nation("Ceylon"), "Sri Lanka")
  expect_equal(canonical_nation("Papua and New Guinea"), "Papua New Guinea")

  # Different competitors, kept apart.
  expect_equal(canonical_nation("Soviet Union"), "Soviet Union")
  expect_equal(canonical_nation("East Germany"), "East Germany")
  expect_equal(canonical_nation("West Germany"), "West Germany")
})

test_that("'Korea' is not folded into South Korea", {
  # At the 2018 Asian Games "Korea" is the unified team, which appeared in the
  # same table as South Korea. Merging them put two rows on one nation.
  expect_equal(canonical_nation("Korea"), "Korea")
  expect_false(identical(canonical_nation("Korea"), "South Korea"))
})

test_that("nation_iso3 returns NA rather than guessing", {
  expect_true(is.na(nation_iso3("Not A Real Country")))
  # The prefix guess this replaced gave Austria, Australasia and Australia the
  # same code, so Austria's medals were scored against Australia's GDP.
  expect_equal(nation_iso3("Australia"), "AUS")
  expect_equal(nation_iso3("Austria"), "AUT")
  expect_false(identical(nation_iso3("Australia"), nation_iso3("Austria")))
  expect_equal(nation_iso3("Chinese Taipei"), "TWN")
  expect_false(identical(nation_iso3("Greece"), nation_iso3("Grenada")))
  expect_false(identical(nation_iso3("Chile"), nation_iso3("Chinese Taipei")))
})

test_that("UK home nations resolve separately, not all to GBR", {
  codes <- nation_iso3(c("England", "Scotland", "Wales", "Northern Ireland",
                         "Great Britain"))
  expect_equal(anyDuplicated(codes), 0L)
  expect_equal(codes[5], "GBR")
})

test_that("teams that are not economies map to NA and are listed", {
  for (tm in non_economy_teams()) expect_true(is.na(nation_iso3(tm)))
})

test_that("canonicalisation leaves one row per nation per edition", {
  # The invariant that caught the Korea bug. Runs against the real table when
  # it is present; skipped in a bare checkout.
  path <- system.file("extdata", "multisport_medal_tables.rds", package = "citius")
  skip_if(!file.exists(path) || file.info(path)$size == 0,
          "medal tables not installed")
  dt <- data.table::as.data.table(readRDS(path))
  dt[, canon := canonical_nation(nation)]
  dupes <- dt[, .N, by = .(games, year, canon)][N > 1]
  expect_equal(nrow(dupes), 0L)
})
