# The podium harvester lives in citiusdata/scripts because it needs a live
# HTML cache, but the parsing rules are pure logic and belong under test: both
# were wrong on the first attempt and both failed silently rather than erroring.
#
# These tests source the REAL implementation from
# citiusdata/scripts/lib/podium_parsing.R -- until 2026-08-14 they tested a
# hand-copied twin defined inside this file, so the actual harvester logic
# could regress with the suite green. Same sibling-repo discovery as
# test-calibration-wiring.R; CI checks citiusdata out beside the package.

# Both layouts, same reason as test-calibration-wiring.R: local dev has
# `citiusverse/citiusdata`, CI checks the private sibling out as
# `citiusdata-sibling` and sets CITIUS_DATA_DIR. Accepting only the first is a
# test that silently stops running the moment it moves to CI.
podium_lib_path <- function() {
  env <- Sys.getenv("CITIUS_DATA_DIR", "")
  if (nzchar(env)) {
    lib <- file.path(env, "scripts", "lib", "podium_parsing.R")
    if (file.exists(lib)) return(lib)
  }
  p <- normalizePath(testthat::test_path("."), winslash = "/", mustWork = FALSE)
  for (i in seq_len(6L)) {
    for (nm in c("citiusdata", "citiusdata-sibling")) {
      lib <- file.path(p, nm, "scripts", "lib", "podium_parsing.R")
      if (file.exists(lib)) return(lib)
    }
    up <- dirname(p)
    if (identical(up, p)) break
    p <- up
  }
  NULL
}

with_podium_lib <- function(code) {
  lib <- podium_lib_path()
  skip_if(is.null(lib),
          "citiusdata/scripts/lib/podium_parsing.R not found beside the package")
  env <- new.env(parent = baseenv())
  sys.source(lib, envir = env)
  # eval() here runs only the literal test block written in THIS file, with the
  # sourced pp_* functions in scope -- no external or user input is evaluated.
  eval(substitute(code), envir = list2env(as.list(env), parent = parent.frame()))
}

test_that("longest-prefix matching separates a nation from a squad listing", {
  with_podium_lib({
    known <- pp_nation_prefixes(c("South Africa", "Australia", "New Zealand",
                                  "Samoa", "India", "Indonesia", "Great Britain"))
    expect_equal(pp_leading_nation("South AfricaJames MurphyZain Davids", known),
                 "South Africa")
    expect_equal(pp_leading_nation("AustraliaCoach: Stacey Marinkovich", known),
                 "Australia")
    expect_equal(pp_leading_nation("New ZealandGina Crampton", known),
                 "New Zealand")
    expect_true(is.na(pp_leading_nation("Ruritania A. N. Other", known)))
    expect_true(is.na(pp_leading_nation("", known)))
  })
})

test_that("longest-prefix beats shortest, which is the whole point", {
  # Sorting shortest-first would match "India" inside "Indonesia" and "Samoa"
  # inside "American Samoa", quietly assigning medals to the wrong nation.
  with_podium_lib({
    known <- pp_nation_prefixes(c("India", "Indonesia", "Samoa", "American Samoa"))
    expect_equal(pp_leading_nation("IndonesiaBudi Santoso", known), "Indonesia")
    expect_equal(pp_leading_nation("IndiaP. V. Sindhu", known), "India")
    expect_equal(pp_leading_nation("American SamoaJohn Doe", known), "American Samoa")
    expect_equal(pp_leading_nation("SamoaJohn Doe", known), "Samoa")
  })
})

test_that("whole-cell canonicalisation resolves abbreviations prefix match cannot", {
  # Standings tables carry forms that are not prefixes of the canonical name;
  # the 1992 Unified Team appears as "CIS".
  with_podium_lib({
    known <- pp_nation_prefixes(c("Unified Team", "Australia"))
    canon <- function(s) if (identical(s, "CIS")) "Unified Team" else s
    expect_true(is.na(pp_leading_nation("CIS", known)))
    expect_equal(pp_leading_nation("CIS", known, canonicalise = canon),
                 "Unified Team")
    # Host markers and footnotes are stripped before matching.
    expect_equal(pp_leading_nation("Australia (H)", known), "Australia")
    expect_equal(pp_leading_nation("Australia[a]", known), "Australia")
  })
})

test_that("missing placings are filled per row, not only when all are missing", {
  # A final-standings table renders the top three places as medal ICONS, so
  # they parse to NA while places 4 onward are numeric. Falling back to row
  # order only when EVERY position is NA missed exactly the podium rows.
  with_podium_lib({
    raw <- c(NA, NA, NA, "4", "5", "6")
    pos <- suppressWarnings(as.integer(gsub("[^0-9]", "", raw)))
    expect_true(anyNA(pos))
    expect_false(all(is.na(pos)))          # the condition that made it fail
    expect_equal(pp_fill_missing_places(pos), 1:6)
    # All-numeric input is untouched; all-NA input becomes row order.
    expect_equal(pp_fill_missing_places(c(1L, 2L, 3L)), 1:3)
    expect_equal(pp_fill_missing_places(c(NA_integer_, NA_integer_)), 1:2)
  })
})

test_that("a numeric gold column marks an NOC table, not a podium listing", {
  # Both carry Gold/Silver/Bronze headers. The NOC table holds counts; the
  # podium holds squads. Without this the podium parser would re-parse tables
  # the main harvester already handled.
  with_podium_lib({
    expect_true(pp_is_noc_count_column(c("1", "1", "0", "0")))
    expect_false(pp_is_noc_count_column(
      c("BrazilWeverton", "GermanyTimo Horn", "NigeriaDaniel")))
    # An all-empty column must be FALSE, not an error: mean() over zero
    # elements is NaN, and if (NaN >= 0.8) aborts -- the pre-lib inline check
    # had exactly that edge.
    expect_false(pp_is_noc_count_column(c("", "", "")))
  })
})
