# Every fitted quantity is either wired at BOTH ends, or on the register below.
#
# WHY THIS FILE EXISTS. Three times now a quantity has been measured, believed
# and shipped while one half of its wiring was missing, and nothing errored:
#
#   1. `indoor`  - `estimate_ability()` read `calibration$indoor` for weeks and
#      no calibration ever carried it. The adjustment block was dead in every
#      deployed run.
#   2. `season`  - identical, and worse: it had been validated out of sample at
#      -0.66% relative RMSE before anyone noticed nothing set it.
#   3. `coasting_trait` (2026-08-01) - the mirror image. A build script attaches
#      it to the calibration, MODEL-LOG records it as "Banked & Adopted", and
#      NOTHING IN THE PACKAGE READS IT. The arm's measured gain therefore came
#      from something else in the same file.
#
# Two recurrences is a mistake; three is a missing guard. This is the guard.
#
# It checks BOTH directions, because the three failures above are two different
# bugs wearing the same clothes:
#
#   READER WITH NO SETTER - the package consumes `calibration$x`, nothing
#     produces it, so the feature silently does nothing.
#   SETTER WITH NO READER - a pipeline fits `x` and attaches it, the package
#     never looks at it, so an arm gets credited for an effect it never applied.
#
# HOW IT FAILS. The expected orphans are pinned in the two registers below. A
# NEW orphan appears as an extra element and fails with its name and the
# function or script that owns it. A register entry that has since been wired up
# fails too, so the register cannot rot into a permanent mute. Adding to the
# register is a visible, reviewable code change with a dated reason - that is the
# point, and it is the only way past this test.
#
# Reads are found by deparsing the package namespace rather than by reading
# `R/*.R`, so the check still runs against an installed package under
# `R CMD check`, where the sources are not present.

# ---------------------------------------------------------------------------
# THE REGISTER. Every entry needs a reason and a date. Prune it when you fix one.
# ---------------------------------------------------------------------------

# Read by the package, set by nothing.
KNOWN_UNSET <- c(
  # ability.R:672-680 reads `calibration$half_life` only when the caller did not
  # pass one. Deliberately forward-compatible: no calibration in the repo carries
  # it, and the branch is inert until one does. (2026-08-06)
  "half_life"
)

# Attached to a calibration, read by nothing in the package.
KNOWN_UNREAD <- c(
  # calibrate.R:410 fits per-event x round foul rates. `simulate_rounds()` has no
  # `foul_prob` argument, so a thrower who no-marks out of a qualifying round is
  # unrepresentable and this table is inert. Tested and RETRACTED on 2026-08-01
  # (gold Brier +1.83%); kept in the calibration so the measurement is not lost.
  # (2026-08-06)
  "foul_round",
  # run_athlete_foul_screening.R:13 calls `fit_athlete_foul_trait()`, which no
  # longer exists anywhere in the package - so that script cannot run at all and
  # the slot it writes is doubly dead. (2026-08-06)
  "athlete_foul"
)

# Layers the package reads that the DEPLOYED calibration deliberately omits.
# Every entry names the experiment that refuted it, so "off on purpose" cannot be
# confused with "silently lost". Populated from a real audit on 2026-08-12; see
# the deployment test at the bottom of this file.
# ONLY `season` is a choice. The other three are DEFECTS, registered so that a
# FIFTH one fails loudly rather than joining them unnoticed. Fixing any of them
# also fails this test until its line is removed -- the register must not rot
# into a permanent mute.
DEPLOYED_OFF <- c(
  # `casym`. Measured gold Brier -0.62% and medal -0.24%, BOTH SIGNIFICANT, and
  # OPTIMISATION-FRAMEWORK.md SS7 still lists it "pending - decide on the new
  # metric". A measured BENEFIT that has never reached a deployed calibration.
  # This is the most expensive entry here. (2026-08-12)
  "asymmetry",
  # Failure 1 in this file's own header, still absent. `estimate_ability()` has
  # read `calibration$indoor` since before this guard existed and no deployed
  # calibration has ever carried it. (2026-08-12)
  "indoor",
  # `apply_momentum()` strips momentum from history and adds it back at forecast
  # time, and no deployed calibration carries the table -- so BOTH halves are
  # inert and every forecast is of an athlete in average readiness. (2026-08-12)
  "momentum",
  # THE ONLY DELIBERATE ONE. A/B'd on 250 meets and REJECTED 2026-08-04: gold
  # Brier +2.02% on majors (p = 0.00059) and +0.74% across 948 scored finals
  # (p = 0.00019). Marks improved while placings degraded, because 59% of the
  # shift is common to the race and cancels from every pairwise comparison.
  # Keep it off. (2026-08-12)
  "season",

  # --- Found 2026-08-13, when this test stopped accepting a NULL element as a
  # carried table (see the note in the deployment check below). All five were
  # NULL in every deployed calibration and invisible to the old name-based
  # check. None is a defect; each is off by a documented default, and each is
  # listed so that "off by design" stays distinguishable from "silently lost".
  #
  # calibrate.R:472 sets it NULL outright: fit_form_sd() needs held-out meets,
  # so it cannot be fitted inside the same call.
  "form_sd",
  # context_per_family defaults FALSE (calibrate.R:326). Recorded as a
  # deliberate non-wiring on 2026-08-03.
  "round_family", "tier_family",
  # context_per_event defaults FALSE (calibrate.R:326). A family is still a pool
  # of events that behave differently, but the per-event fitter was refuted by
  # the backtest even at k = 0.
  "round_event", "tier_event"
)

# Slots that are diagnostics or run metadata, never model inputs. The estimator
# is not supposed to read these, so their absence from the read set is correct
# rather than a defect.
#
# Be sparing. An entry here is a claim that nothing SHOULD read the slot, and it
# switches this guard off for that slot permanently and silently -- which is the
# one failure mode the register below cannot express, because a metadata entry
# never fails when the slot gets wired. `race` sat here until 2026-08-13 and hid
# the package's largest unwired quantity; see its note in KNOWN_UNREAD. If the
# honest reason is "nothing reads it YET", that is KNOWN_UNREAD, not this list.
CALIBRATION_METADATA <- c(
  # Audited 2026-08-13, after `race` was found hiding here. Each remaining entry
  # was checked against BOTH the package and citiusdata/scripts.
  #
  # `converged`/`delta`/`sweeps` LEFT THIS LIST in the same audit: they were
  # stamped at build time, printed by rebaseline_chain.R:97 as the file was
  # written, and never consulted again -- so the deployed calibration has shipped
  # `converged = FALSE` (delta 1.66e-04) through every forecast and published
  # rating with nothing saying so. `.warn_unconverged()` now reads all three.
  "ability",                               # per-athlete side of the decomposition;
                                           # superseded by estimate_ability(), which
                                           # refits with recency, shrinkage and
                                           # context. Read by nothing anywhere --
                                           # revisit if it is ever wanted as a
                                           # race-adjusted prior. (2026-08-13)
  "min_races", "min_race_size",            # thresholds the fit was run under;
                                           # describe the fit, never adjust a
                                           # prediction. (2026-08-13)
  "provenance"                             # audit stamp; read by
                                           # backtest_athletics.R:69, outside the
                                           # package by design. (2026-08-13)
)

# Objects in the pipeline scripts that hold a calibration. Anything assigned into
# one of these is a claim that the package will use it.
CALIBRATION_OBJECTS <- c("cal", "cal2", "calib", "calibration")

# ---------------------------------------------------------------------------
# Scanners
# ---------------------------------------------------------------------------

#' Every `calibration$<x>` the package actually reads, with its owning function.
wiring_reads <- function() {
  ns <- asNamespace("citius")
  out <- list()
  for (o in ls(ns, all.names = TRUE)) {
    v <- tryCatch(get(o, envir = ns), error = function(e) NULL)
    if (!is.function(v)) next
    txt <- tryCatch(paste(deparse(v), collapse = "\n"), error = function(e) "")
    if (!nzchar(txt)) next
    direct <- regmatches(txt, gregexpr("calibration\\$[A-Za-z_][A-Za-z0-9_.]*", txt))[[1]]
    # `.context_precision(calibration, "round")` reaches the slot through
    # `calibration[[which]]`, so the element name is at the CALL site, not the
    # definition. Miss these and the two busiest slots in the package -- round
    # and tier -- look unread.
    indirect <- regmatches(
      txt, gregexpr('\\.context_precision\\(calibration, "[a-z_]+"', txt))[[1]]
    els <- c(sub("^calibration\\$", "", direct),
             sub('.*"([a-z_]+)"$', "\\1", indirect))
    if (length(els)) out[[length(out) + 1L]] <- data.frame(fn = o, element = els)
  }
  if (!length(out)) return(data.frame(fn = character(), element = character()))
  unique(do.call(rbind, out))
}

#' Every slot `calibrate()` itself produces, taken from a real call rather than
#' from parsing the source -- a list built with `structure(list(...))` cannot be
#' read reliably any other way, and a run cannot go stale.
wiring_calibrate_slots <- function() {
  set.seed(7)
  n_races <- 60L; n_per <- 8L; n_ath <- 24L
  ability <- stats::rnorm(n_ath, to_perf(10, -1L), 0.02)
  c_r <- stats::rnorm(n_races, 0, 0.006)
  rows <- lapply(seq_len(n_races), function(r) {
    who <- sample(n_ath, n_per)
    data.table::data.table(
      race_key = paste0("r", r), athlete_id = as.character(who),
      event_id = "AT-100Metres-M", date = Sys.Date() - r,
      round = "F", tier = "OW", indoor = FALSE, venue_country = "GBR",
      perf = ability[who] + c_r[r] + stats::rnorm(n_per, 0, 0.010))
  })
  names(suppressWarnings(calibrate(data.table::rbindlist(rows))))
}

#' The citiusverse root, or NULL. Tests run from `tests/testthat` under
#' `devtools::test()` and from a check directory under `R CMD check`, so the
#' sibling data repo is found by walking up rather than by a fixed offset.
wiring_verse_root <- function() {
  p <- normalizePath(testthat::test_path("."), winslash = "/", mustWork = FALSE)
  for (i in seq_len(6L)) {
    if (dir.exists(file.path(p, "citiusdata", "scripts"))) return(p)
    up <- dirname(p)
    if (identical(up, p)) break
    p <- up
  }
  NULL
}

#' Every element a pipeline script attaches to a calibration object.
wiring_script_setters <- function(root) {
  dir <- file.path(root, "citiusdata", "scripts")
  fs <- list.files(dir, pattern = "[.]R$", full.names = TRUE)
  pat <- "^\\s*([A-Za-z_.][A-Za-z0-9_.]*)\\$([A-Za-z_][A-Za-z0-9_.]*)\\s*<-"
  out <- list()
  for (f in fs) {
    # Strip comments first: a `#` example of an assignment is not an assignment.
    l <- sub("#.*$", "", readLines(f, warn = FALSE))
    idx <- which(regexpr(pat, l) > 0)
    if (!length(idx)) next
    obj <- sub(paste0(pat, ".*$"), "\\1", l[idx])
    el  <- sub(paste0(pat, ".*$"), "\\2", l[idx])
    ok <- obj %in% CALIBRATION_OBJECTS
    if (any(ok)) out[[length(out) + 1L]] <-
      data.frame(file = basename(f), line = idx[ok], element = el[ok])
  }
  if (!length(out)) return(data.frame(file = character(), line = integer(),
                                      element = character()))
  do.call(rbind, out)
}

# ---------------------------------------------------------------------------
# The checks
# ---------------------------------------------------------------------------

test_that("the wiring scan finds something to check", {
  # A "count the violations, pass if zero" guard passes vacuously on an empty
  # scan -- which is exactly what a broken scanner produces. Floors are set well
  # below the current counts (18 reads, 23 slots) so a real deletion trips them
  # rather than every routine edit.
  reads <- wiring_reads()
  expect_gte(nrow(reads), 20L)
  expect_gte(length(unique(reads$element)), 15L)
  expect_gte(length(wiring_calibrate_slots()), 15L)
})

test_that("every calibration element the package reads is set by something", {
  # Script-side setters (momentum, asymmetry, ...) live in citiusdata/scripts,
  # so without the sibling repo this test cannot tell "set by a script" from
  # "set by nothing" and reports false orphans. Same environment guard as the
  # two tests below — the full check is local-dev-only, like theirs.
  root <- wiring_verse_root()
  skip_if(is.null(root),
          "citiusdata/scripts not found beside the package; script-side setters invisible, orphan check unreliable")

  reads <- wiring_reads()
  set_by <- union(wiring_calibrate_slots(), wiring_script_setters(root)$element)

  orphans <- sort(setdiff(unique(reads$element), set_by))
  owners <- vapply(orphans, function(e)
    paste(sort(unique(reads$fn[reads$element == e])), collapse = ", "),
    character(1))

  expect_equal(
    orphans, sort(KNOWN_UNSET),
    info = paste0(
      "A calibration element is read by the package and produced by nothing, so ",
      "the code that reads it is dead. Wire a setter, or add it to KNOWN_UNSET ",
      "with a reason and a date.\n",
      paste0("  ", orphans, "  <- read by ", owners, collapse = "\n")))
})

test_that("every element calibrate() produces is read, or is declared metadata", {
  reads <- unique(wiring_reads()$element)
  slots <- setdiff(wiring_calibrate_slots(), CALIBRATION_METADATA)
  orphans <- sort(setdiff(slots, reads))

  expect_equal(
    orphans, sort(intersect(KNOWN_UNREAD, wiring_calibrate_slots())),
    info = paste0(
      "calibrate() fits and attaches ", paste(orphans, collapse = ", "),
      " and nothing in the package reads it. An arm carrying this element gets ",
      "credited for an effect it never applied. Wire a reader, declare it in ",
      "CALIBRATION_METADATA, or register it in KNOWN_UNREAD with a date."))
})

test_that("every element a pipeline script attaches to a calibration is read", {
  root <- wiring_verse_root()
  skip_if(is.null(root),
          "citiusdata/scripts not found beside the package; script-side wiring unchecked")

  setters <- wiring_script_setters(root)
  # Same vacuity trap as above: an empty scan must fail, not pass quietly.
  expect_gte(nrow(setters), 10L)

  reads <- unique(wiring_reads()$element)
  candidates <- setdiff(unique(setters$element), CALIBRATION_METADATA)
  orphans <- sort(setdiff(candidates, reads))
  where <- vapply(orphans, function(e) {
    s <- setters[setters$element == e, ]
    paste(paste0(s$file, ":", s$line), collapse = ", ")
  }, character(1))

  expect_equal(
    orphans, sort(intersect(KNOWN_UNREAD, candidates)),
    info = paste0(
      "A pipeline script attaches a quantity to a calibration that the package ",
      "never reads. This is how the coasting trait was recorded as adopted ",
      "while doing nothing. Wire a reader, or register it with a date.\n",
      paste0("  ", orphans, "  <- set at ", where, collapse = "\n")))
})

test_that("the DEPLOYED calibration carries a table for every layer the package reads", {
  # THE GAP THE THREE TESTS ABOVE CANNOT SEE (2026-08-12).
  #
  # They check that CODE reads what CODE sets. All three pass while the model
  # runs with half its adjustment layers switched off, because every layer is
  # gated on `!is.null(calibration$x)` and SKIPS IN SILENCE when the deployed
  # file has no table for it. A setter existing in some build script is not the
  # same claim as the shipped calibration carrying the result.
  #
  # Audited 2026-08-12 against calibration_corpus_csigma.rds: five of ten layers
  # were absent, three of them unintentionally. That is the same failure as the
  # `indoor` and `season` incidents this file was written for, one level further
  # out -- and it is why "we built that feature" and "that feature affects the
  # answer" keep coming apart here.
  #
  # A layer that is off ON PURPOSE goes on DEPLOYED_OFF with the experiment that
  # refuted it. Deliberately-off and silently-lost must not look identical.
  root <- wiring_verse_root()
  skip_if(is.null(root), "citiusdata not found beside the package")
  dep <- file.path(root, "citiusdata", "scripts", "_deployed.R")
  skip_if_not(file.exists(dep), "_deployed.R not found")

  src <- readLines(dep, warn = FALSE)
  hit <- grep("^\\s*calibration\\s*=", src, value = TRUE)
  skip_if(!length(hit), "_deployed.R names no calibration")
  cal_file <- file.path(root, "citiusdata", "data",
                        sub('^[^"]*"([^"]*)".*$', "\\1", hit[1]))
  skip_if_not(file.exists(cal_file),
              paste("deployed calibration not present:", basename(cal_file)))

  # NON-NULL, not merely NAMED (fixed 2026-08-13, found by deploying `coast`).
  #
  # `names()` alone gave a FALSE PASS. `calibrate.R:498` builds the list as
  # `indoor = if (isTRUE(context_indoor) ...) {...}`, and when that condition is
  # FALSE the element still EXISTS, holding NULL. So `names()` reported `indoor`
  # and `season` as carried while both were empty -- and every application site
  # is gated on `!is.null()`, so both were doing nothing.
  #
  # That is this test's own failure mode reproduced inside the test: a layer
  # present in form, absent in effect, reported as fine. The old deployed file
  # predated that calibrate() version and had no such names at all, which is the
  # only reason the gap stayed hidden.
  cal_obj <- readRDS(cal_file)
  have <- names(cal_obj)[!vapply(cal_obj, is.null, logical(1))]
  expect_gte(length(have), 10L)          # vacuity floor: an unreadable file must fail

  reads <- unique(wiring_reads()$element)
  # KNOWN_UNSET is already registered as "nothing anywhere produces this", so it
  # cannot also count as a deployment gap.
  candidates <- setdiff(reads, KNOWN_UNSET)
  missing <- sort(setdiff(candidates, have))

  expect_equal(
    missing, sort(intersect(DEPLOYED_OFF, candidates)),
    info = paste0(
      "The deployed calibration (", basename(cal_file), ") carries no table for: ",
      paste(missing, collapse = ", "),
      ".\nEach of those adjustment layers is gated on !is.null() and is doing ",
      "NOTHING in every shipped forecast, silently. Rebuild the calibration with ",
      "it, or add it to DEPLOYED_OFF naming the experiment that refuted it."))
})

test_that("the deployed stamp names the deployed calibration", {
  root <- wiring_verse_root()
  skip_if(is.null(root), "citiusdata/scripts not found beside the package")
  f <- file.path(root, "citiusdata", "scripts", "_deployed.R")
  skip_if_not(file.exists(f), "_deployed.R not found")

  src <- readLines(f, warn = FALSE)
  grab <- function(field) {
    hit <- grep(paste0("^\\s*", field, "\\s*="), src, value = TRUE)
    if (!length(hit)) return(NA_character_)
    sub('^[^"]*"([^"]*)".*$', "\\1", hit[1])
  }
  stamp <- grab("stamp")
  cal <- grab("calibration")
  expect_false(is.na(stamp))
  expect_false(is.na(cal))

  # `calibration_corpus_csigma.rds` -> "csigma". The stamp is written into every
  # published artefact and is the only thing a reader can use to tell which model
  # produced a card, so a stamp naming a different arm from the file is a
  # mislabelled forecast, not a cosmetic slip.
  arm <- sub("[.]rds$", "", sub("^calibration_corpus_?", "", cal))
  if (nzchar(arm)) {
    expect_true(
      grepl(arm, stamp, fixed = TRUE),
      info = paste0("DEPLOYED$stamp is '", stamp, "' but DEPLOYED$calibration is '",
                    cal, "'. Bump the stamp in the same edit as the calibration."))
  }
})
