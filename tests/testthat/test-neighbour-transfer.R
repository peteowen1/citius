# transfer_neighbour_ability(): the three standardisation mistakes its own
# documentation lists, each of which reversed the direction of the transfer for
# the case that motivated it (Ingebrigtsen, 2026-09-13), plus the weight cap.

rank_ref <- function(event_id, raw) {
  raw <- sort(raw)
  data.table::data.table(event_id = event_id,
                         pctl = (seq_along(raw) - 0.5) / length(raw),
                         ability_raw = raw)
}

ab_row <- function(athlete_id, event_id, ability_raw, w_total = 1, n_eff = 1) {
  data.table::data.table(athlete_id = athlete_id, event_id = event_id,
                         ability_raw = ability_raw, prior_mu = 0, shrinkage = 0,
                         ability = ability_raw, w_total = w_total, n_eff = n_eff)
}

edge <- function(r2) data.table::data.table(event_id = "EV-T", neighbour_event_id = "EV-N", r2 = r2)

base <- seq(0, 1, length.out = 101)
ref_t <- rank_ref("EV-T", 10 * base^3)          # skewed target: its own shape, not a normal
ref_n <- rank_ref("EV-N", base)

test_that("the implied value is read off the TARGET's own empirical distribution (mistake 2)", {
  ab <- rbind(ab_row("a1", "EV-T", 2), ab_row("a1", "EV-N", ref_n$ability_raw[91]))
  out <- transfer_neighbour_ability(ab, edge(1), rbind(ref_t, ref_n))
  # r2 = 1: the neighbour's percentile carries over unshrunk, so the implied
  # value is the target's 91st reference value, and it gets one race's weight.
  implied <- ref_t$ability_raw[91]
  expect_equal(out[event_id == "EV-T"]$ability_raw, (1 * 2 + 1 * implied) / 2, tolerance = 1e-9)
  expect_equal(out[event_id == "EV-N"]$ability_raw, ref_n$ability_raw[91])  # no edge into EV-N
})

test_that("only the neighbour's RANK matters, not its scale (mistake 1)", {
  # A monotone, non-linear rescaling of the whole neighbour event changes every
  # z-score but no percentile. A z-score transfer would move; this must not.
  f <- function(x) x^3 + 5 * x
  ab1 <- rbind(ab_row("a1", "EV-T", 2), ab_row("a1", "EV-N", ref_n$ability_raw[80]))
  ab2 <- rbind(ab_row("a1", "EV-T", 2), ab_row("a1", "EV-N", f(ref_n$ability_raw[80])))
  ref_n2 <- data.table::copy(ref_n)[, ability_raw := f(ability_raw)]
  o1 <- transfer_neighbour_ability(ab1, edge(0.5), rbind(ref_t, ref_n))
  o2 <- transfer_neighbour_ability(ab2, edge(0.5), rbind(ref_t, ref_n2))
  expect_equal(o1[event_id == "EV-T"]$ability_raw, o2[event_id == "EV-T"]$ability_raw, tolerance = 1e-9)
})

test_that("a population reference is required rather than read from an only= table (mistake 3)", {
  ab <- rbind(ab_row("a1", "EV-T", 2), ab_row("a1", "EV-N", 0.5))
  expect_error(transfer_neighbour_ability(ab, edge(0.5), NULL), "rank_reference")
})

test_that("a neighbour counts for r2 of one race, so deep histories barely move", {
  r2 <- 0.25
  implied <- ref_t$ability_raw[91]
  thin <- rbind(ab_row("a1", "EV-T", 2, w_total = 1, n_eff = 1),
                ab_row("a1", "EV-N", ref_n$ability_raw[91]))
  deep <- rbind(ab_row("a1", "EV-T", 2, w_total = 20, n_eff = 20),
                ab_row("a1", "EV-N", ref_n$ability_raw[91]))
  p <- stats::pnorm(sqrt(r2) * stats::qnorm(ref_n$pctl[91]))
  implied <- stats::approx(ref_t$pctl, ref_t$ability_raw, xout = p, rule = 2)$y
  ot <- transfer_neighbour_ability(thin, edge(r2), rbind(ref_t, ref_n))[event_id == "EV-T"]
  od <- transfer_neighbour_ability(deep, edge(r2), rbind(ref_t, ref_n))[event_id == "EV-T"]
  expect_equal(ot$ability_raw, (1 * 2 + r2 * implied) / (1 + r2), tolerance = 1e-9)
  expect_equal(od$ability_raw, (20 * 2 + r2 * implied) / (20 + r2), tolerance = 1e-9)
  expect_lt(abs(od$ability_raw - 2), abs(ot$ability_raw - 2) / 10)
})

test_that("no neighbour table is a true no-op", {
  ab <- rbind(ab_row("a1", "EV-T", 2), ab_row("a1", "EV-N", 0.5))
  expect_equal(transfer_neighbour_ability(ab)$ability_raw, ab$ability_raw)
})
