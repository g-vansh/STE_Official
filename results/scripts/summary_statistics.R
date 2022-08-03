
############################################################
##
## First. Describe the data.
##
############################################################

# Print out some summary statistics
names(cb_startups)[grep(".*angel.*",names(cb_startups))]
cb_startups$bearly_stage_has_vc = as.double(cb_startups$bearly_stage_has_vc)
main.variables <- c("raised_amount",
                    "seed_amount",
                    "angel_amount","early_stage_amount",

                    "series_a_amount",
                    "series_b_amount",
                    "bseed_has_vc",
                    "bpre_seed_has_vc",
                    "bearly_stage_has_vc",
                    "equity_growth",
                    "ipo","acquired")


dim(cb_startups)

main.variables[(main.variables %in% names(cb_startups))]
stargazer::stargazer(data.frame(cb_startups[,main.variables]), out="tex/summary_stats.tex",
                     title="Summary Statistics",
                     covariate.labels = cb_startups.labels[main.variables],
                     float=FALSE,
                     omit.table.layout = "n",
                     summary.stat=c("mean","sd","N"))
