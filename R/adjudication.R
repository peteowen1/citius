#' How a sport's winner is determined
#'
#' Classifies sports by **who decides the result**, which is the distinction
#' that matters when asking whether home advantage comes from officials rather
#' than from athletes.
#'
#' Three classes, not two. A two-way objective/subjective split has no honest
#' home for netball or judo: nobody scores the performance for quality, but the
#' result still passes through a referee, so they belong with neither the
#' stopwatch events nor the judged ones. Collapsing them into "subjective"
#' would load every team sport onto the judging effect; collapsing them into
#' "objective" would hide the discretion that is really there. Keep three and
#' let the analysis decide what to pool.
#'
#' - `"measured"` — the result is a physical quantity read off an instrument:
#'   a time, a distance, a mass, a score determined by where a projectile
#'   lands. Officials rule on legality (false start, foul, no-lift) but never
#'   on quality. Athletics, swimming, cycling, weightlifting, rowing,
#'   shooting, archery.
#' - `"judged"` — officials score the performance itself, and the score *is*
#'   the result. Artistic and rhythmic gymnastics, diving, artistic swimming,
#'   boxing, taekwondo, karate, equestrian dressage, freestyle and
#'   aerial disciplines.
#' - `"opponent"` — you win by beating someone, and officials referee rather
#'   than score. Team sports, racquet sports, bowls, and the grappling and
#'   fencing combat sports where points are awarded by officials but for
#'   defined actions rather than for quality.
#'
#' **Boxing is deliberately `"judged"` and judo is deliberately `"opponent"`.**
#' Both are combat sports refereed by officials, so the line could be drawn
#' either way, and where it is drawn changes the answer. Boxing's result comes
#' from judges' scorecards awarding rounds on a quality assessment; judo's
#' comes from defined scoring actions (ippon, waza-ari) which the referee
#' recognises rather than rates. `sport_adjudication_sensitivity()` reports how
#' much a conclusion moves if the two are swapped.
#'
#' @param sport Character vector of sport names, as they appear in
#'   `sport_medal_tables`.
#' @return Character vector of `"measured"`, `"judged"`, `"opponent"`, or `NA`
#'   for a sport not in the taxonomy. `NA` is returned rather than a guess, for
#'   the same reason [match_event()] does: a wrong bucket is invisible.
#' @export
sport_adjudication <- function(sport) {
  measured <- c(
    "Athletics", "Para-athletics", "Swimming", "Para-swimming",
    "Cycling", "Track cycling", "Road cycling", "Mountain biking", "BMX",
    "Para-track cycling", "Weightlifting", "Powerlifting", "Para powerlifting",
    "Rowing", "Canoeing", "Triathlon", "Shooting", "Archery",
    "Modern pentathlon", "Speed skating", "Short track speed skating",
    "Cross-country skiing", "Alpine skiing", "Biathlon", "Bobsleigh",
    "Luge", "Skeleton", "Nordic combined", "Ski jumping", "Sailing",
    "Athletics and para-athletics", "Lifesaving"
  )
  judged <- c(
    "Artistic gymnastics", "Rhythmic gymnastics", "Trampoline gymnastics",
    "Trampolining", "Gymnastics", "Diving", "Synchronised swimming",
    "Synchronized swimming", "Artistic swimming", "Boxing", "Taekwondo",
    "Karate", "Wushu", "Figure skating", "Freestyle skiing", "Snowboarding",
    "Surfing", "Skateboarding", "Breaking", "Equestrian", "Equestrian events",
    "Sport climbing"
  )
  opponent <- c(
    "Judo", "Wrestling", "Fencing", "Badminton", "Table tennis", "Tennis",
    "Squash", "Hockey", "Field hockey", "Netball", "Basketball",
    "3x3 basketball", "Volleyball", "Beach volleyball", "Handball",
    "Football", "Rugby sevens", "Rugby union", "Cricket", "Bowls",
    "Lawn bowls", "Baseball", "Softball", "Water polo", "Golf",
    "Curling", "Ice hockey", "Racquets", "Real tennis", "Polo",
    "Lacrosse", "Tug of war", "Jeu de paume", "Rackets", "Croquet", "Roque",
    "Pelota"
  )

  key <- trimws(sport)
  out <- rep(NA_character_, length(key))
  out[key %in% measured] <- "measured"
  out[key %in% judged]   <- "judged"
  out[key %in% opponent] <- "opponent"
  out
}


#' Group a sport into a family that is stable across editions
#'
#' Wikipedia names the same sport differently as a programme narrows, and a
#' comparison across editions reads those renamings as sports appearing and
#' disappearing. Between Birmingham 2022 and Glasgow 2026 the articles go
#' `Cycling` → `Track cycling`, `Gymnastics` → `Artistic gymnastics` and
#' `Lawn bowls` → `Bowls`. Taken literally that is three sports dropped and
#' three added, and a shift-share decomposition then attributes a large mix
#' effect to something that never happened.
#'
#' A family is a *narrowing*, not an identity: Glasgow's track-only cycling is
#' a subset of Birmingham's cycling. That is exactly what a shift-share wants —
#' the family's share of the programme falls, and the nation's rate within it is
#' still computed on whatever was contested.
#'
#' Every family sits in one adjudication class, so grouping never mixes
#' `measured` with `judged`.
#'
#' @param sport Character vector of sport names.
#' @return Character vector of family names; the input is returned unchanged
#'   where no family applies.
#' @export
sport_family <- function(sport) {
  fam <- c(
    "Cycling" = "Cycling", "Track cycling" = "Cycling",
    "Road cycling" = "Cycling", "Mountain biking" = "Cycling", "BMX" = "Cycling",
    "Gymnastics" = "Gymnastics", "Artistic gymnastics" = "Gymnastics",
    "Rhythmic gymnastics" = "Gymnastics", "Trampoline gymnastics" = "Gymnastics",
    "Trampolining" = "Gymnastics",
    "Bowls" = "Bowls", "Lawn bowls" = "Bowls",
    "Hockey" = "Hockey", "Field hockey" = "Hockey",
    "Synchronised swimming" = "Artistic swimming",
    "Synchronized swimming" = "Artistic swimming",
    "Artistic swimming" = "Artistic swimming",
    "Weightlifting" = "Weightlifting",
    "Powerlifting" = "Para powerlifting", "Para powerlifting" = "Para powerlifting",
    "Basketball" = "Basketball", "3x3 basketball" = "Basketball",
    "Volleyball" = "Volleyball", "Beach volleyball" = "Volleyball",
    "Athletics" = "Athletics", "Para-athletics" = "Athletics",
    "Swimming" = "Swimming", "Para-swimming" = "Swimming"
  )
  hit <- unname(fam[trimws(sport)])
  ifelse(is.na(hit), sport, hit)
}


#' Sports whose classification is contestable
#'
#' Returns the sports where a reasonable person could draw the line elsewhere,
#' so a result can be re-run with them reassigned. Reporting a host effect
#' without this is reporting one arbitrary choice as though it were the only
#' one.
#'
#' @return A `data.table` with `sport`, `assigned`, and `alternative`.
#' @export
sport_adjudication_contested <- function() {
  data.table::data.table(
    sport = c("Boxing", "Judo", "Wrestling", "Taekwondo", "Fencing",
              "Equestrian events", "Sport climbing", "Archery", "Shooting",
              "Sailing"),
    assigned = c("judged", "opponent", "opponent", "judged", "opponent",
                 "judged", "judged", "measured", "measured", "measured"),
    alternative = c("opponent", "judged", "judged", "opponent", "judged",
                    "measured", "measured", "opponent", "opponent", "opponent"),
    why = c(
      "Judges score rounds on quality, but it is still a head-to-head bout.",
      "Points are for defined actions, but the referee recognises them.",
      "As judo; passivity calls are discretionary.",
      "Electronic scoring since 2012 moved it toward measured/opponent.",
      "Apparatus scores hits electronically; priority calls are not.",
      "Dressage is judged; jumping is measured faults against a clock.",
      "Speed is a stopwatch; boulder and lead are scored by judges.",
      "Score is where the arrow lands, but matchplay format is head-to-head.",
      "As archery.",
      "Finishing positions are objective; protests are adjudicated."
    )
  )
}
