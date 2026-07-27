# Declare data.table non-standard-evaluation symbols so R CMD check stays quiet.
utils::globalVariables(c(
  ".", ".N", ".SD",
  # results schema
  "ability", "ability_raw", "age", "athlete_id", "athlete_name", "birthdate",
  "cv_prior", "date", "discipline", "distance", "event_id", "indoor",
  "is_technical", "legal", "mark", "mark_string", "orientation", "perf",
  "place", "prob", "round", "sex", "sex_code", "sport", "tier", "value_raw",
  "wind",
  # ability estimation
  "kappa", "last_date", "n", "n_eff", "prior_mu", "shrinkage", "sigma",
  "sigma_between", "sigma_raw", "tactical", "w",
  # context effects
  "eff", "r_adj", "resid", "resid2", "round_class", "tier_class",
  # calibration
  "a_i", "c_new", "c_r", "calibrated", "cond_share", "condition_sd",
  "foul_rate", "n_in_race", "n_multi_races", "n_obs", "n_races", "noise_var",
  "offset", "precision", "race_key", "sensitivity", "sensitivity_raw",
  "sigma_e", "sigma_within", "slope", "slope_adj", "sxx", "tactical_index", "y"
))
utils::globalVariables(c("shared", "implausible", "guess_frac", "guessed_cond"))
utils::globalVariables(c("family", "dev", "n_ages", "peak_age", "effect",
                         "age_now", "age_ref", "age_shift", "N"))
utils::globalVariables(c("supported", "plateau_lo", "plateau_hi", "min_density"))
utils::globalVariables(c("peak_identified"))
utils::globalVariables(c("country", "athlete_name", "implied", "p_gold", "p_medal"))
utils::globalVariables(c("w_total", "idx", "n_tot", "err", "half_life", "mae",
                         "stale_years", "gap", "age_last", "age_asof"))
utils::globalVariables(c("hl", "identified"))
utils::globalVariables(c("prob", "hit", "brier", "brier_base", "logloss",
                         "logloss_base", "base", "field", "bin", "observed",
                         "mean_predicted", "skill"))
utils::globalVariables(c("z", "df", "comp_start", "comp_name", "hit_medal", "winner_present"))
