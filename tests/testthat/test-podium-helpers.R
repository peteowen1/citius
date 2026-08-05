# The podium harvester lives in citiusdata/scripts because it needs a live
# HTML cache, but the two rules that made it work are pure logic and belong
# under test here: both were wrong on the first attempt and both failed
# silently rather than erroring.

test_that("longest-prefix matching separates a nation from a squad listing", {
  # Podium cells run the nation straight into the squad with no separator.
  known <- c("South Africa", "Australia", "New Zealand", "Samoa", "India",
             "Indonesia", "Great Britain")
  by_len <- known[order(-nchar(known))]
  leading <- function(s) {
    hit <- by_len[startsWith(s, by_len)]
    if (length(hit)) hit[1] else NA_character_
  }
  expect_equal(leading("South AfricaJames MurphyZain Davids"), "South Africa")
  expect_equal(leading("AustraliaCoach: Stacey Marinkovich"), "Australia")
  expect_equal(leading("New ZealandGina Crampton"), "New Zealand")
  expect_true(is.na(leading("Ruritania A. N. Other")))
})

test_that("longest-prefix beats shortest, which is the whole point", {
  # Sorting shortest-first would match "India" inside "Indonesia" and "Samoa"
  # inside "Samoa Joe", quietly assigning medals to the wrong nation.
  known <- c("India", "Indonesia", "Samoa", "American Samoa")
  by_len <- known[order(-nchar(known))]
  leading <- function(s) {
    hit <- by_len[startsWith(s, by_len)]
    if (length(hit)) hit[1] else NA_character_
  }
  expect_equal(leading("IndonesiaBudi Santoso"), "Indonesia")
  expect_equal(leading("IndiaP. V. Sindhu"), "India")
  expect_equal(leading("American SamoaJohn Doe"), "American Samoa")
  expect_equal(leading("SamoaJohn Doe"), "Samoa")
})

test_that("missing placings are filled per row, not only when all are missing", {
  # A final-standings table renders the top three places as medal ICONS, so
  # they parse to NA while places 4 onward are numeric. Falling back to row
  # order only when EVERY position is NA missed exactly the podium rows.
  raw <- c(NA, NA, NA, "4", "5", "6")
  pos <- suppressWarnings(as.integer(gsub("[^0-9]", "", raw)))
  expect_true(anyNA(pos))
  expect_false(all(is.na(pos)))          # the condition that made it fail

  pos[is.na(pos)] <- seq_along(raw)[is.na(pos)]
  expect_equal(pos, 1:6)
  expect_equal(which(pos == 1), 1L)
  expect_equal(which(pos == 3), 3L)
})

test_that("a numeric gold column marks an NOC table, not a podium listing", {
  # Both carry Gold/Silver/Bronze headers. The NOC table holds counts; the
  # podium holds squads. Without this the podium parser would re-parse tables
  # the main harvester already handled.
  is_noc <- function(v) {
    v <- trimws(gsub("\\[.*?\\]", "", v))
    v <- v[nzchar(v)]
    length(v) > 0 && mean(grepl("^[0-9]+$", v)) >= 0.8
  }
  expect_true(is_noc(c("1", "1", "0", "0")))
  expect_false(is_noc(c("BrazilWeverton", "GermanyTimo Horn", "NigeriaDaniel")))
})
