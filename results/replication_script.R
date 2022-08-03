  # Load Data and Library
library(devtools)
  detach("package:STE", unload = TRUE)
  devtools::install_github("g-vansh/STE", force = T)
  library(STE)
  library(lfe)
  library(fixest)
  library(stargazer)
  library(glmnet)
  load("/shared/share_scp/STE_Official/results/data/cb_startups.Rdata")
  setwd("/shared/share_scp/STE_Official/results")
  set.seed(7)

  # SETUP:
  ml_vars.no_inter <- ml_vars[grep("^[^X]",ml_vars)]



#
#   par(mar=c(1,1,1,1))
#   # Estimate the propensity score of the treatment.
#   p_scores <- estimate_propensity(
#     treatment = cb_startups$bearly_stage_has_vc,
#     X = cb_startups[, ml_vars]
#   )
#
#   save(p_scores, file = "data/p_scores.Rdata")
#
#   hist(p_scores)

  load("data/p_scores.Rdata")
  cb_startups$predicted_early_stage_vc_rf <- p_scores


  source("scripts/estimate_main_effects.R")

  # Estimate the strategic treatment effect.
  cb_startups_eq <- estimate_ste(
    y = cb_startups$equity_growth,
    treatment = cb_startups$bearly_stage_has_vc,
    propensity = p_scores,
    df = cb_startups
  )


  # Remove NA values for analysis.
  cb_startups_eq.clean <- cb_startups_eq[sample(1:nrow(cb_startups_eq)),] %>%
    filter(!is.na(ste))

  # Study the determinants of STE.
  ste_features_eq <- STE::get_top_ste_determinants(
    X = cb_startups_eq[, ml_vars],
    teffect = cb_startups_eq$teffect
  )
  View(ste_features_eq)

  # Estimate the coherence value.
  coherence_value_eq <- STE::estimate_coherence(
    y = cb_startups_eq.clean$ste,
    x = cb_startups_eq.clean[, ml_vars],
    x.no_inter = cb_startups_eq.clean[, ml_vars.no_inter]
  )

  #######################################################

  # Estimate the strategic treatment effect.

  cb_startups_acq <- estimate_ste(
    y = cb_startups$acquired,
    treatment = cb_startups$bearly_stage_has_vc,
    propensity = p_scores,
    df = cb_startups
  )

  #######################################################

  # Estimate the strategic treatment effect.
  cb_startups_b <- estimate_ste(
    y = cb_startups$series_b_has_investment,
    treatment = cb_startups$bearly_stage_has_vc,
    propensity = p_scores,
    df = cb_startups
  )

  # Remove NA values for analysis.
  cb_startups_b.clean <- cb_startups_b[sample(1:nrow(cb_startups_b)),] %>%
    filter(!is.na(ste))

  # Study the determinants of STE.
  ste_features_b <- STE::get_top_ste_determinants(
    X = cb_startups_b[, ml_vars],
    teffect = cb_startups_b$teffect
  )
  View(ste_features_b)

  coherence_value_b <- STE::estimate_coherence(
    y = cb_startups_b.clean$ste,
    x = cb_startups_b.clean[, ml_vars],
    x.no_inter = cb_startups_b.clean[, ml_vars.no_inter]
  )



  ######################################## END OF SETUP SECTION RUN UP TO HERE


  #######################################################

  #
  source("scripts/plots.R")

  # Explore RF Results
  source("scripts/random_forest.R")

  # Study Outcomes
  source("scripts/plot_outcomes_across_p_score.R")

  # Study Effects
  source("scripts/estimate_main_effects.R")

  # Study STE Determinants
  source("scripts/STE_determinants.R")

  # Get Coherence
  source("scripts/coherence_estimates.R")
  ###############################################
