test_that("the score is bounded and built from its two components", {
  s <- sport_subjectivity()
  expect_true(all(s$assessment_share >= 0 & s$assessment_share <= 1))
  expect_true(all(s$officiating_scope >= 0 & s$officiating_scope <= 1))
  expect_true(all(s$subjectivity >= 0 & s$subjectivity <= 1))
  expect_equal(s$subjectivity,
               0.75 * s$assessment_share + 0.25 * s$officiating_scope)
})

test_that("the weight argument actually moves the score", {
  a <- sport_subjectivity(w = 0.75)
  b <- sport_subjectivity(w = 0.25)
  data.table::setkey(a, sport); data.table::setkey(b, sport)
  expect_false(isTRUE(all.equal(a[b$sport, subjectivity], b$subjectivity)))
  expect_equal(b$subjectivity,
               0.25 * b$assessment_share + 0.75 * b$officiating_scope)
})

test_that("the ordering matches what the sports actually are", {
  # Anchors: a judged sport must outrank a refereed team sport, which must
  # outrank a stopwatch sport. If an edit to the table breaks this ordering it
  # is the table that is wrong.
  v <- subjectivity_of(c("Artistic gymnastics", "Boxing", "Judo", "Football",
                         "Athletics", "Swimming", "Archery"))
  names(v) <- c("gym", "box", "judo", "football", "athletics", "swim", "archery")
  expect_gt(v[["gym"]], v[["judo"]])
  expect_gt(v[["box"]], v[["judo"]])
  expect_gt(v[["judo"]], v[["football"]])
  expect_gt(v[["football"]], v[["athletics"]])
  expect_gt(v[["athletics"]], v[["archery"]])
})

test_that("football separates on the two axes, which is why they are separate", {
  # Football has almost no performance-quality judging and a great deal of
  # refereeing discretion. A single-axis score cannot express that, and the
  # three-bucket classification could not place it at all.
  a <- subjectivity_of("Football", component = "assessment_share")
  b <- subjectivity_of("Football", component = "officiating_scope")
  expect_lt(a, 0.10)
  expect_gt(b, 0.60)
  # Gymnastics is the mirror image: all assessment, little in-play discretion.
  expect_gt(subjectivity_of("Artistic gymnastics", component = "assessment_share"), 0.90)
  expect_lt(subjectivity_of("Artistic gymnastics", component = "officiating_scope"), 0.35)
})

test_that("an unscored sport returns NA rather than a default", {
  expect_true(is.na(subjectivity_of("Quidditch")))
  expect_true(is.na(subjectivity_of("Aquatics")))
})

test_that("subjectivity_of is vectorised and order-preserving", {
  x <- c("Boxing", "Athletics", "Nonesuch", "Diving")
  v <- subjectivity_of(x)
  expect_equal(length(v), 4L)
  expect_true(is.na(v[3]))
  expect_equal(v[1], subjectivity_of("Boxing"))
})

test_that("every scored sport carries a stated basis", {
  s <- sport_subjectivity()
  expect_equal(nrow(s[!nzchar(basis)]), 0L)
  expect_false(anyDuplicated(s$sport) > 0)
})

test_that("team sports are flagged and are not the same thing as subjective", {
  s <- sport_subjectivity()
  expect_true(s[sport == "Football", team_sport])
  expect_false(s[sport == "Boxing", team_sport])
  # The whole point: team sports sit mid-scale, not at the top.
  expect_lt(max(s[team_sport == TRUE, subjectivity]),
            max(s[team_sport == FALSE, subjectivity]))
})
