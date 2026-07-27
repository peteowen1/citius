# End-to-end demonstration: men's 100m.
# Pulls live histories, estimates ability, simulates the final.

devtools::load_all(here::here("citius"))
library(data.table)

FIELD <- c("Noah Lyles", "Kishane Thompson", "Letsile Tebogo", "Christian Coleman",
           "Akani Simbine", "Oblique Seville", "Ferdinand Omanyala", "Fred Kerley")

histories <- rbindlist(lapply(FIELD, function(nm) {
  cand <- find_athlete(nm)
  if (!nrow(cand)) return(NULL)
  r <- athlete_results(cand[sex == "M"][1]$athlete_id)
  if (nrow(r)) r[, athlete_name := nm]
  r
}), use.names = TRUE, fill = TRUE)

sprint <- histories[event_id == "AT-100Metres-M" & legal == TRUE & !is.na(perf)]
names_by_id <- unique(sprint[, .(athlete_id = as.character(athlete_id), athlete_name)])

context <- estimate_context_effects(sprint)
ability <- estimate_ability(sprint, as_of = Sys.Date())
ability <- merge(ability, names_by_id, by = "athlete_id")

sim <- simulate_event(ability, n_sims = 50000L, seed = 42)

results <- merge(medal_probs(sim), names_by_id, by = "athlete_id")
setorder(results, -p_gold)

results[, .(athlete_name, p_gold, p_medal, p_top8, median_mark)]
prob_better_than(sim, 9.80, who = "any")
