############################################################################
lasso_ste.all <- cv.glmnet(x=data.matrix(cb_startups_eq.clean[,ml_vars]),
                           y=cb_startups_eq.clean$ste,
                           standardize=TRUE)

lasso_ste.no_inter <- cv.glmnet(x=data.matrix(cb_startups_eq.clean[,ml_vars.no_inter]),
                                y=cb_startups_eq.clean$ste,
                                standardize=TRUE)

top_fit = which(lasso_ste.all$lambda==lasso_ste.all$lambda.min)
rsq.all <- lasso_ste.all$glmnet.fit$dev.ratio[top_fit]

top_fit = which(lasso_ste.no_inter$lambda==lasso_ste.no_inter$lambda.min)
rsq.no_inter <- lasso_ste.no_inter$glmnet.fit$dev.ratio[top_fit]

coherence.eq = rsq.all/rsq.no_inter-1

############################################################################

lasso_series_b.all <- cv.glmnet(x=data.matrix(cb_startups_b.clean[,ml_vars]),
                                y=cb_startups_b.clean$ste,
                                standardize=TRUE)

lasso_series_b.no_inter <- cv.glmnet(x=data.matrix(cb_startups_b.clean[,ml_vars.no_inter]),
                                     y=cb_startups_b.clean$ste,
                                     standardize=TRUE)

top_fit = which(lasso_series_b.all$lambda==lasso_series_b.all$lambda.min)
rsq_series_b.all <- lasso_series_b.all$glmnet.fit$dev.ratio[top_fit]

top_fit = which(lasso_series_b.no_inter$lambda==lasso_series_b.no_inter$lambda.min)
rsq_series_b.no_inter <- lasso_series_b.no_inter$glmnet.fit$dev.ratio[top_fit]

coherence.b = rsq_series_b.all/rsq_series_b.no_inter-1


col1 <- c("R Squared Full Model","R Squared Non Interacted Model","Value of Coherence")
col2 <- c(rsq.all,rsq.no_inter,coherence.eq)
col3 <- c(rsq_series_b.all,rsq_series_b.no_inter,coherence.b)

ds_coherence <- data.frame(col1,col2, col3)
names(ds_coherence) <- c("Statistic",
                         "Equity Growth STE Estimates",
                         "Series B STE Estimates")

stargazer(ds_coherence, summary = FALSE,
          out="tex/coherence.tex",
          title="Estimates of the Importance of Coherence",
          rownames=FALSE,
          omit.table.layout = "n",
          float=FALSE)



