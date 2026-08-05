test_that("the three classes carry the sports they should", {
  expect_equal(sport_adjudication("Athletics"), "measured")
  expect_equal(sport_adjudication("Swimming"), "measured")
  expect_equal(sport_adjudication("Track cycling"), "measured")
  expect_equal(sport_adjudication("Weightlifting"), "measured")

  expect_equal(sport_adjudication("Artistic gymnastics"), "judged")
  expect_equal(sport_adjudication("Diving"), "judged")
  expect_equal(sport_adjudication("Boxing"), "judged")

  expect_equal(sport_adjudication("Netball"), "opponent")
  expect_equal(sport_adjudication("Judo"), "opponent")
  expect_equal(sport_adjudication("Bowls"), "opponent")
})

test_that("an unknown sport returns NA rather than a guess", {
  # Same contract as match_event(): a wrong bucket is invisible, whereas an NA
  # shows up in the coverage check and forces a decision.
  expect_true(is.na(sport_adjudication("Quidditch")))
  expect_true(is.na(sport_adjudication("Aquatics")))  # a container, not a sport
})

test_that("it is vectorised and order-preserving", {
  got <- sport_adjudication(c("Athletics", "Boxing", "Netball", "Nonesuch"))
  expect_equal(got, c("measured", "judged", "opponent", NA_character_))
})

test_that("the contested list names both the assignment and its alternative", {
  cs <- sport_adjudication_contested()
  expect_true(all(c("sport", "assigned", "alternative", "why") %in% names(cs)))
  expect_true(all(cs$assigned != cs$alternative))
  # Every contested sport must actually be classified as the table claims.
  expect_equal(sport_adjudication(cs$sport), cs$assigned)
})

test_that("boxing and judo sit on opposite sides of the line", {
  # This is the boundary the sensitivity analysis exists to test. If a future
  # edit moves one without moving the analysis, this fails rather than quietly
  # changing the headline result.
  expect_equal(sport_adjudication("Boxing"), "judged")
  expect_equal(sport_adjudication("Judo"), "opponent")
})

test_that("sport_family folds the cross-edition renamings", {
  # Birmingham 2022 -> Glasgow 2026 renames all three of these. Left ungrouped,
  # a shift-share reads them as three sports dropped and three added.
  expect_equal(sport_family("Track cycling"), "Cycling")
  expect_equal(sport_family("Artistic gymnastics"), "Gymnastics")
  expect_equal(sport_family("Lawn bowls"), "Bowls")
  expect_equal(sport_family("Field hockey"), "Hockey")
})

test_that("an unfamiliar sport passes through sport_family unchanged", {
  expect_equal(sport_family("Athletics"), "Athletics")
  expect_equal(sport_family("Quidditch"), "Quidditch")
})

test_that("every family sits in exactly one adjudication class", {
  # Grouping must never merge a measured sport with a judged one.
  sports <- c("Cycling", "Track cycling", "Road cycling", "Mountain biking", "BMX",
              "Gymnastics", "Artistic gymnastics", "Rhythmic gymnastics",
              "Bowls", "Lawn bowls", "Hockey", "Field hockey",
              "Basketball", "3x3 basketball", "Volleyball", "Beach volleyball",
              "Athletics", "Para-athletics", "Swimming", "Para-swimming")
  d <- data.frame(fam = sport_family(sports), cls = sport_adjudication(sports))
  per_family <- tapply(d$cls, d$fam, function(x) length(unique(x)))
  expect_true(all(per_family == 1))
})

test_that("no sport is in more than one class", {
  sports <- c(
    "Athletics", "Swimming", "Track cycling", "Weightlifting", "Rowing",
    "Shooting", "Archery", "Artistic gymnastics", "Rhythmic gymnastics",
    "Diving", "Boxing", "Taekwondo", "Judo", "Wrestling", "Fencing",
    "Netball", "Bowls", "Hockey", "Squash", "Badminton"
  )
  cls <- sport_adjudication(sports)
  expect_false(anyNA(cls))
  expect_equal(length(cls), length(sports))
})
