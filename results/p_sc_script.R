# Load Data and Library
detach("package:STE", unload = TRUE)
devtools::install_github("g-vansh/STE", force = T)
library(STE)
load("/shared/share_scp/STE_Official/results/cb_startups.Rdata")
# load("/shared/share_scp/STE_Official/results/seed7_10cv_pscores.Rdata")
setwd("/shared/share_scp/STE_Official/results")

# Estimate the propensity score of the treatment.
p_scores <- estimate_propensity(
  treatment = cb_startups$bearly_stage_has_vc,
  X = cb_startups[, ml_vars]
)

save(p_scores, file = "p_scores_optmtry_10fold.Rdata")
