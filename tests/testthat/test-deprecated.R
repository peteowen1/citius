test_that("every renamed athletics function has a working shim", {
  # A shim that warns but does not forward is worse than no shim: the caller
  # gets a warning and a silent NULL.
  pairs <- list(
    c("competition_results", "athletics_competition_results"),
    c("athlete_results", "athletics_athlete_results"),
    c("athlete_profile", "athletics_athlete_profile"),
    c("find_athlete", "athletics_find_athlete"),
    c("find_competition", "athletics_find_competition"),
    c("harvest_competitions", "athletics_harvest_competitions"))
  for (p in pairs) {
    expect_true(exists(p[1], envir = asNamespace("citius")), info = p[1])
    expect_true(exists(p[2], envir = asNamespace("citius")), info = p[2])
  }
})

test_that("the shim warns and names its replacement", {
  rlang::reset_warning_verbosity("citius_deprecated_find_athlete")
  expect_warning(
    tryCatch(find_athlete("zzz-nobody-zzz"), error = function(e) NULL),
    "deprecated"
  )
})

test_that("swimming adapters were not mangled by the athletics rename", {
  # "athlete_results" is a substring of "aquatics_athlete_results", so a
  # replace without word boundaries would have produced
  # "aquatics_athletics_athlete_results".
  for (f in c("aquatics_athlete_results", "aquatics_competitions",
              "aquatics_disciplines", "aquatics_results", "aquatics_base_url")) {
    expect_true(exists(f, envir = asNamespace("citius")), info = f)
  }
  expect_false(exists("aquatics_athletics_athlete_results", envir = asNamespace("citius")))
})

test_that("every sport-specific adapter is prefixed by its federation", {
  exported <- getNamespaceExports("citius")
  adapters <- grep("^(athletics_|aquatics_)", exported, value = TRUE)
  expect_gt(length(adapters), 8)

  # Deprecated shims: the documented exception, listed explicitly so a NEW
  # unprefixed adapter still fails this test.
  shims <- c("competition_results", "athlete_results", "athlete_profile",
             "find_athlete", "find_competition", "harvest_competitions")
  # Functions that merely LOOK like adapters. These transform data already in
  # hand rather than calling a federation, so a prefix would be misleading:
  # clean_results() works on any sport's results, and the store functions are
  # storage, not sources.
  not_adapters <- c("clean_results", "score_predictions",
                    "read_results_store", "write_results_store")

  leftovers <- setdiff(
    grep("_results$|^find_|_profile$|^harvest_", exported, value = TRUE),
    c(adapters, shims, not_adapters))
  expect_equal(leftovers, character(0))
})
