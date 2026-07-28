test_that("name splitting reads the surname from capitalisation, not position", {
  # The two feeds use opposite orders and both mark the surname with capitals.
  expect_equal(.split_name("Sam SHORT")$surname, "SHORT")
  expect_equal(.split_name("Sam SHORT")$given, "SAM")
  expect_equal(.split_name("SHORT Samuel")$surname, "SHORT")
  expect_equal(.split_name("SHORT Samuel")$given, "SAMUEL")
})

test_that("name splitting falls back to the declared order without a case signal", {
  # World Athletics writes "Taoufik Makhloufi" with no capitals to read.
  expect_equal(.split_name("Taoufik Makhloufi", "given_first")$surname, "MAKHLOUFI")
  expect_equal(.split_name("Makhloufi Taoufik", "surname_first")$surname, "MAKHLOUFI")
  # An all-caps name carries no signal either, so the fallback must still apply.
  expect_equal(.split_name("SAM SHORT", "given_first")$surname, "SHORT")
  expect_equal(.split_name("SHORT SAM", "surname_first")$surname, "SHORT")
})

test_that("multi-token surnames survive", {
  expect_equal(.split_name("VAN DER COLFF Deandra", "surname_first")$surname,
               "VANDERCOLFF")
  expect_equal(.split_name("Deandra VAN DER COLFF")$surname, "VANDERCOLFF")
})

test_that("the loose key joins short and full given names in both directions", {
  expect_equal(loose_key("Sam SHORT", "given_first"),
               loose_key("SHORT Samuel", "surname_first"))
  expect_equal(loose_key("Joshua LIENDO", "given_first"),
               loose_key("LIENDO Josh", "surname_first"))
  # A middle name we do not hold must not break the key.
  expect_equal(loose_key("Iona Alexandra ANDERSON", "given_first"),
               loose_key("ANDERSON Iona", "surname_first"))
})

test_that("the loose key is NA when there is no given name to take an initial from", {
  # Otherwise every mononym would collapse onto the same key.
  expect_true(is.na(loose_key("PELE", "surname_first")))
  expect_true(is.na(loose_key(NA_character_, "surname_first")))
})

test_that("crosswalk links the same athlete across sources", {
  x <- data.table::data.table(
    source = c("wa", "wa", "crs", "crs"),
    athlete_name = c("SHORT Samuel", "LIENDO Josh", "Sam SHORT", "Joshua LIENDO"),
    athlete_id = c("1", "2", NA, NA))
  xw <- athlete_crosswalk(x, name_order = c(wa = "surname_first",
                                            crs = "given_first"))
  expect_equal(data.table::uniqueN(xw$person_id), 2L)
  expect_true(all(xw$match_method == "loose"))
})

test_that("crosswalk refuses to merge an ambiguous loose key", {
  # ZHANG|Y covers 20 different swimmers in the real corpus. If the guard fails,
  # their careers fuse into one athlete and nothing downstream can detect it.
  x <- data.table::data.table(
    source = c("wa", "wa", "crs"),
    athlete_name = c("ZHANG Ying", "ZHANG Yichi", "Yu ZHANG"),
    athlete_id = c("1", "2", NA))
  xw <- athlete_crosswalk(x, name_order = c(wa = "surname_first",
                                            crs = "given_first"))
  expect_equal(data.table::uniqueN(xw$person_id), 3L)
  expect_true(all(xw$match_method == "unmatched"))
})

test_that("birthdate links athletes that no name rule can reach", {
  # "Christopher Bennett" / "Chris Bennett" share neither an exact key nor,
  # necessarily, anything a name rule should be trusted with alone.
  x <- data.table::data.table(
    source = c("wa", "crs"),
    athlete_name = c("Chris Bennett", "Christopher BENNETT"),
    birthdate = as.Date(c("1992-11-11", "1992-11-11")))
  xw <- athlete_crosswalk(x, name_order = c(wa = "given_first",
                                            crs = "given_first"))
  expect_equal(data.table::uniqueN(xw$person_id), 1L)
  expect_true(all(xw$match_method == "birthdate"))
})

test_that("a shared surname with different birthdates stays separate", {
  x <- data.table::data.table(
    source = c("wa", "crs"),
    athlete_name = c("Chris Bennett", "Christopher BENNETT"),
    birthdate = as.Date(c("1992-11-11", "2001-03-04")))
  xw <- athlete_crosswalk(x, name_order = c(wa = "given_first",
                                            crs = "given_first"))
  expect_equal(data.table::uniqueN(xw$person_id), 2L)
})

test_that("unmatched athletes are retained, not dropped", {
  # The unmatched rows ARE the harvest to-do list; silently dropping them would
  # make the gap invisible.
  x <- data.table::data.table(
    source = c("wa", "crs"),
    athlete_name = c("SJOESTROEM Sarah", "Nobody UNKNOWN"))
  xw <- athlete_crosswalk(x, name_order = c(wa = "surname_first",
                                            crs = "given_first"))
  expect_equal(nrow(xw), 2L)
  expect_true(all(xw$match_method == "unmatched"))
})

test_that("hyphens are split but apostrophes are not", {
  # Feeds disagree hyphen-vs-space for the same person, so a hyphenated given
  # name must match its spaced form. "Imara-Bella Patricia THORPE" is
  # "THORPE Imara Bella Patricia" at World Aquatics.
  expect_equal(athlete_key("Imara-Bella Patricia THORPE"),
               athlete_key("THORPE Imara Bella Patricia"))
  expect_equal(athlete_key("AL-SAID Ahmed"), athlete_key("Ahmed AL SAID"))
  # Apostrophes stay removed -- splitting would give BRIEN + O, which sorts
  # apart and no longer matches the unpunctuated spelling.
  expect_equal(athlete_key("Sean O'BRIEN"), athlete_key("OBRIEN Sean"))
})

test_that("a hyphenated surname is still not the same as its second half", {
  # Adam RAMSAY-PEATY is a different swimmer from Adam PEATY, and splitting
  # hyphens must not blur that.
  expect_false(identical(athlete_key("Adam RAMSAY-PEATY"), athlete_key("Adam PEATY")))
})

test_that("fuzzy_scope confines name matching to the sources named", {
  # Corpus-wide, surname+initial merged different people (Sophie Bateman with
  # BATEMAN Sarah). Scoping keeps the candidate pool small enough for it to mean
  # something: a link may only form if it involves an in-scope athlete.
  x <- data.table::data.table(
    source = c("games", "wa", "se", "se"),
    athlete_name = c("Sam SHORT", "SHORT Samuel", "Sophie Bateman", "Sarah Bateman"),
    athlete_id = c(NA, "1", "2", "3"))
  xw <- athlete_crosswalk(x, name_order = c(wa = "surname_first",
                                            se = "given_first",
                                            games = "given_first"),
                          fuzzy_scope = "games")
  # The Games swimmer still links to World Aquatics...
  short <- xw[grepl("SHORT|Samuel", athlete_name)]
  expect_equal(data.table::uniqueN(short$person_id), 1L)
  # ...but the two Batemans, neither of whom is in scope, stay separate.
  bate <- xw[grepl("Bateman", athlete_name)]
  expect_equal(data.table::uniqueN(bate$person_id), 2L)
})
