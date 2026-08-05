#' How official-dependent is a sport's result?
#'
#' A continuous 0-1 score, replacing the three-bucket
#' [sport_adjudication()] classification for anything that wants to regress on
#' subjectivity rather than compare groups.
#'
#' The buckets could not describe football. Nobody scores a footballer's
#' performance for quality, so it is not `judged`; but a penalty, an offside
#' call or a red card decides matches without any change in how the players
#' played, which is a great deal more official discretion than a 100 metres
#' carries. Bucketing it with netball and bowls hid that, and bucketing it with
#' gymnastics would have been worse.
#'
#' So the score is built from **two axes that are separately arguable**, and
#' both are reported so a reader can disagree with one without discarding the
#' other:
#'
#' \describe{
#'   \item{`assessment_share`}{How much of the result is a human's assessment of
#'     the quality of a performance, as against a physical measurement. Gymnastics
#'     is ~0.95: the judges' score *is* the result. Swimming is ~0.03: the clock
#'     is the result. Weightlifting is 0.25 because three referees rule each lift
#'     good or no-lift, which is an assessment even though the mass is objective.}
#'   \item{`officiating_scope`}{How much scope officials have to change the
#'     outcome through in-competition decisions, given the performances. Football
#'     is 0.75; a 100 metres is 0.15 (false start, lane infringement); tennis is
#'     0.20 because ball-tracking review binds the umpire.}
#' }
#'
#' `subjectivity = w * assessment_share + (1 - w) * officiating_scope`, with
#' `w = 0.75` by default. **These numbers are judgements, not measurements**, and
#' the honest use of them is to vary them: fit on `assessment_share` alone, on
#' `officiating_scope` alone, on the rank rather than the value, and check the
#' conclusion survives. `sport_subjectivity()` returns the components precisely
#' so that is cheap to do.
#'
#' @param w Weight on `assessment_share`. Default 0.75.
#' @return A `data.table` with `sport`, `assessment_share`, `officiating_scope`,
#'   `subjectivity`, `team_sport` and `basis`.
#' @export
sport_subjectivity <- function(w = 0.75) {
  sport <- c(
      # --- result is a measurement ---
      "Athletics", "Para-athletics", "Swimming", "Para-swimming", "Rowing",
      "Canoeing", "Cycling", "Track cycling", "Road cycling", "Mountain biking",
      "BMX", "Shooting", "Archery", "Triathlon", "Weightlifting",
      "Para powerlifting", "Powerlifting", "Sailing", "Modern pentathlon",
      "Speed skating", "Cross-country skiing", "Alpine skiing", "Biathlon",
      # --- head-to-head, factual scoring, refereed ---
      "Football", "Hockey", "Field hockey", "Basketball", "3x3 basketball",
      "Handball", "Water polo", "Rugby sevens", "Rugby union", "Netball",
      "Volleyball", "Beach volleyball", "Cricket", "Tennis", "Badminton",
      "Table tennis", "Squash", "Bowls", "Lawn bowls", "Golf", "Polo",
      "Baseball", "Softball", "Curling", "Ice hockey", "Tug of war",
      # --- combat: officials award points for defined actions ---
      "Fencing", "Judo", "Wrestling", "Taekwondo", "Karate", "Boxing", "Wushu",
      # --- judged for quality ---
      "Gymnastics", "Artistic gymnastics", "Rhythmic gymnastics",
      "Trampoline gymnastics", "Trampolining", "Diving",
      "Synchronised swimming", "Synchronized swimming", "Artistic swimming",
      "Figure skating", "Freestyle skiing", "Snowboarding", "Surfing",
      "Skateboarding", "Breaking", "Equestrian", "Equestrian events",
      "Sport climbing",
      # --- regional-games sports, scored on the same two axes ---
      "Bowling", "Tenpin bowling", "Racquetball", "Soft tennis", "Squash rackets",
      "Sepaktakraw", "Kabaddi", "Dragon boat", "Canoe sprint", "Rowing sculls",
      "Water skiing", "Roller sports", "Roller skating", "Ju-jitsu", "Jujitsu",
      "Kurash", "Pencak silat", "Sambo", "Basque pelota", "Pelota vasca",
      "Chess", "Bridge", "Xiangqi", "Go", "Esports", "Billiards sports",
      "Snooker", "Weiqi", "Netball sevens", "Lawn tennis", "Fistball",
      "Field archery", "Shooting sports", "Modern pentathlon and biathlon"
  )
  assessment_share <- c(
      0.05, 0.05, 0.03, 0.03, 0.03,
      0.05, 0.08, 0.08, 0.08, 0.08,
      0.08, 0.02, 0.02, 0.05, 0.25,
      0.25, 0.25, 0.05, 0.25,
      0.03, 0.03, 0.03, 0.03,
      0.05, 0.05, 0.05, 0.05, 0.05,
      0.05, 0.05, 0.05, 0.05, 0.05,
      0.03, 0.03, 0.05, 0.02, 0.02,
      0.02, 0.05, 0.03, 0.03, 0.03, 0.05,
      0.03, 0.03, 0.05, 0.05, 0.03,
      0.15, 0.45, 0.40, 0.30, 0.65, 0.85, 0.85,
      0.95, 0.95, 0.97,
      0.90, 0.90, 0.95,
      0.97, 0.97, 0.97,
      0.95, 0.92, 0.92, 0.95,
      0.90, 0.95, 0.55, 0.55,
      0.45,
      # regional-games sports
      0.02, 0.02, 0.03, 0.02, 0.05,
      0.05, 0.05, 0.03, 0.03, 0.03,
      0.03, 0.55, 0.55, 0.45, 0.45,
      0.45, 0.70, 0.40, 0.05, 0.05,
      0.02, 0.02, 0.02, 0.02, 0.03, 0.03,
      0.02, 0.02, 0.05, 0.02, 0.03,
      0.02, 0.02, 0.05
  )
  officiating_scope <- c(
      0.15, 0.15, 0.12, 0.12, 0.15,
      0.25, 0.35, 0.35, 0.35, 0.30,
      0.30, 0.08, 0.08, 0.25, 0.20,
      0.20, 0.20, 0.35, 0.25,
      0.20, 0.15, 0.15, 0.15,
      0.75, 0.65, 0.65, 0.70, 0.70,
      0.70, 0.80, 0.75, 0.75, 0.65,
      0.35, 0.35, 0.55, 0.20, 0.25,
      0.25, 0.55, 0.20, 0.20, 0.12, 0.60,
      0.35, 0.35, 0.25, 0.60, 0.30,
      0.55, 0.55, 0.55, 0.50, 0.45, 0.60, 0.40,
      0.25, 0.25, 0.25,
      0.20, 0.20, 0.15,
      0.20, 0.20, 0.20,
      0.20, 0.25, 0.25, 0.30,
      0.25, 0.25, 0.35, 0.35,
      0.25,
      # regional-games sports
      0.08, 0.08, 0.35, 0.25, 0.55,
      0.60, 0.65, 0.20, 0.20, 0.15,
      0.20, 0.30, 0.30, 0.55, 0.55,
      0.55, 0.50, 0.55, 0.35, 0.35,
      0.05, 0.10, 0.05, 0.05, 0.15, 0.10,
      0.10, 0.05, 0.65, 0.20, 0.30,
      0.08, 0.08, 0.20
  )
  basis <- c(
      "clock or tape; officials rule fouls and false starts only",
      "as athletics, plus classification set before competition",
      "clock; stroke and turn judges can disqualify",
      "as swimming",
      "clock; steering and interference calls",
      "clock; slalom gate touches are judged penalties",
      "clock or finish order; relegation for dangerous riding",
      "clock or finish order; relegation in sprint events",
      "finish order; some race-conduct calls",
      "clock; course conduct",
      "clock and finish order",
      "score is where the shot lands",
      "score is where the arrow lands",
      "clock; drafting and transition penalties",
      "mass is objective, but three referees rule each lift good or no-lift",
      "as weightlifting",
      "as weightlifting",
      "finishing positions objective; protests and redress adjudicated",
      "mixed disciplines; riding phase carries judging",
      "clock; lane and impeding calls",
      "clock",
      "clock; gate faults",
      "clock plus shooting penalties",
      "goals are factual, but penalties, offside and cards decide matches",
      "goals factual; penalty corners and cards heavily refereed",
      "goals factual; penalty corners and cards heavily refereed",
      "baskets factual; foul calls decide close games",
      "as basketball, shorter format so single calls matter more",
      "goals factual; heavy foul and suspension discretion",
      "goals factual; among the most referee-dependent team sports",
      "tries factual; breakdown and card calls are highly discretionary",
      "as rugby sevens",
      "goals factual; contact and obstruction calls are discretionary",
      "rally outcome factual; line calls largely instrumented",
      "as volleyball",
      "runs and wickets factual; LBW and dismissal calls, reduced by review",
      "line calls instrumented by ball tracking",
      "rally outcome factual; service and line calls",
      "rally outcome factual; service legality calls",
      "lets and strokes are among the most discretionary calls in sport",
      "distance to the jack is measured",
      "distance to the jack is measured",
      "strokes counted; rulings are rare and reviewable",
      "goals factual; heavy umpiring discretion on fouls",
      "runs factual; umpire calls on close plays",
      "as baseball",
      "stone position measured; burned-stone calls",
      "goals factual; heavy penalty discretion",
      "pull is decided by position; judges rule on form",
      "apparatus scores hits; right-of-way is a referee judgement",
      "ippon and waza-ari are graded by the referee panel",
      "points awarded for defined holds; passivity calls discretionary",
      "electronic body scoring since 2012; technical points still judged",
      "kata fully judged; kumite points awarded by a panel",
      "judges score rounds on quality and award the bout",
      "taolu is fully judged; sanda is scored by officials",
      "the judges' score is the result",
      "the judges' score is the result",
      "the judges' score is the result",
      "judged, with a measured difficulty component",
      "judged, with a measured difficulty component",
      "the judges' score is the result",
      "the judges' score is the result",
      "the judges' score is the result",
      "the judges' score is the result",
      "the judges' score is the result",
      "judged runs",
      "judged runs",
      "judged rides, with heat-based progression",
      "judged runs",
      "judged battles",
      "dressage judged, jumping measured against faults and a clock",
      "dressage judged, jumping measured against faults and a clock",
      "speed is a stopwatch; boulder and lead are judged",
      "pins are counted",
      "pins are counted",
      "rally outcome factual; hinder calls are discretionary",
      "rally outcome factual; line and service calls",
      "as squash; lets and strokes are discretionary",
      "points factual; heavy refereeing of contact and service",
      "raids are scored by officials with substantial discretion",
      "finish order against a clock",
      "clock",
      "clock",
      "clock",
      "judged runs on a scored trick basis",
      "artistic events judged; speed events timed",
      "ippon and points awarded by a referee panel",
      "as ju-jitsu",
      "points awarded for defined grips and throws",
      "kumite-style points awarded by a judging panel, with artistic elements",
      "points awarded for defined actions",
      "rally outcome factual; heavy refereeing",
      "as basque pelota",
      "the position on the board is decisive",
      "trick score is objective; play is adjudicated on procedure",
      "the position on the board is decisive",
      "the position on the board is decisive",
      "in-game score is objective; rulings on conduct only",
      "pots and points are counted",
      "pots and points are counted",
      "the position on the board is decisive",
      "goals factual; heavy umpiring discretion",
      "line calls largely instrumented",
      "points factual; service and fault calls",
      "score is where the arrow lands",
      "score is where the shot lands",
      "clock plus shooting penalties"
  )

  # Checked BEFORE the table is built. data.table() recycles a short column
  # with only a warning, which silently pairs every sport after the gap with
  # the wrong score -- exactly what happened when `basis` was one entry short.
  lens <- c(sport = length(sport), assessment = length(assessment_share),
            officiating = length(officiating_scope), basis = length(basis))
  if (length(unique(lens)) != 1L) {
    stop("sport_subjectivity(): column lengths differ - ",
         paste(names(lens), lens, sep = "=", collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(sport)) {
    stop("sport_subjectivity(): duplicated sport - ",
         paste(sport[duplicated(sport)], collapse = ", "), call. = FALSE)
  }

  d <- data.table::data.table(sport = sport,
                              assessment_share = assessment_share,
                              officiating_scope = officiating_scope,
                              basis = basis)

  d[, team_sport := sport %in% c(
    "Football", "Hockey", "Field hockey", "Basketball", "3x3 basketball",
    "Handball", "Water polo", "Rugby sevens", "Rugby union", "Netball",
    "Volleyball", "Beach volleyball", "Cricket", "Baseball", "Softball",
    "Curling", "Ice hockey", "Tug of war", "Polo")]

  d[, subjectivity := w * assessment_share + (1 - w) * officiating_scope]
  data.table::setorder(d, -subjectivity)
  d[]
}


#' Subjectivity score for a vector of sports
#'
#' @param sport Character vector of sport names.
#' @param w Weight on `assessment_share`; see [sport_subjectivity()].
#' @param component One of `"subjectivity"`, `"assessment_share"` or
#'   `"officiating_scope"`.
#' @return Numeric vector, `NA` where the sport is not scored.
#' @export
subjectivity_of <- function(sport, w = 0.75, component = "subjectivity") {
  component <- match.arg(component,
    c("subjectivity", "assessment_share", "officiating_scope"))
  tbl <- sport_subjectivity(w = w)
  tbl[[component]][match(trimws(sport), tbl$sport)]
}
