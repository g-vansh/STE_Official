# Estiamte the main effects with the doubly robust model

do_double_lasso <- function(X,treatment, y,formula_stem,formula_end = "") {
  if (file.exists("double_lasso_formulas.rds")){
    double_lasso_formulas = readRDS("double_lasso_formulas.rds")

  }else {
    double_lasso_formulas = data.frame()
  }

  print(paste0("Model:" , formula_stem))
  print("Running first lasso")
  l1 <- cv.glmnet(data.matrix(X),y=as.double(treatment))
  print("Running second lasso")
  l2 <- cv.glmnet(data.matrix(X),y=as.double(y))
  print("Done")
  coefs_l1 = ml_vars[coef(l1)[-1]!=0]
  coefs_l2 = ml_vars[coef(l2)[-1]!=0]
  double_lasso_coefs <- union(coefs_l1, coefs_l2)


  m2_formula  = formula_stem
  for(cd in double_lasso_coefs) {
    if(str_length(cd) > 0 ){
      m2_formula = paste0(m2_formula, "+", cd)
    }
  }
  m2_formula = paste0(m2_formula, "", formula_end)
  m2_formula= as.formula(m2_formula)
  print(m2_formula)
  m4 <- felm(m2_formula, data=cb_startups )
  ret_obj = list(formula=m2_formula,
                 regression = m4)
  return (ret_obj)
}

cb_startups$raised_series_b = cb_startups$series_b_amount > 0


ols.eq.early_stage <- felm(equity_growth ~ bearly_stage_has_vc |founding_year|0|state_code, data=cb_startups)
ols.eq.seed <- felm(equity_growth ~ bseed_has_vc |founding_year|0|state_code, data=cb_startups)
ols.series_b.early_stage <- felm( raised_series_b ~ bearly_stage_has_vc |founding_year|0|state_code, data=cb_startups)

m5 <- do_double_lasso(X=cb_startups[,ml_vars],
                      treatment=cb_startups$bearly_stage_has_vc,
                      y=cb_startups$equity_growth,
                      formula_stem="equity_growth ~ bearly_stage_has_vc",
                      formula_end="|founding_year|0|state_code")

m6 <- do_double_lasso(X=cb_startups[,ml_vars],
                      treatment=cb_startups$bseed_has_vc,
                      y=cb_startups$equity_growth,
                      formula_stem="equity_growth ~ bseed_has_vc",
                      formula_end="|founding_year|0|state_code")

m7 <- do_double_lasso(X=cb_startups[,ml_vars],
                      treatment=cb_startups$bearly_stage_has_vc,
                      y=cb_startups$raised_series_b,
                      formula_stem="raised_series_b ~ bearly_stage_has_vc",
                      formula_end="|founding_year|0|state_code")

cb_startups.X <- cb_startups %>%
  mutate(log_series_b = log(ifelse(series_b_amount==0,NA,series_b_amount)))


regvars <- c("bearly_stage_has_vc","bseed_has_vc")

regs <- list(ols.eq.early_stage,  m5$regression, ols.eq.seed, m6$regression, ols.series_b.early_stage, m7$regression)



save(regs,file="double_lasso_objects.RData")

stargazer(regs, type="text",
          keep=regvars,
          dep.var.labels = c("Equity Growth","Equity Growth",
                             "Raised Series B",
                             "Raised Series B",
                             "Equity Growth"),
          model.names=FALSE,
          column.labels=c("OLS","Double LASSO",
                          "OLS","Double LASSO",
                          "OLS","Double LASSO"),
          covariate.labels = cb_startups.labels[regvars],
          keep.stat = c("n","rsq"),
          omit.table.layout = "n",
          float=FALSE)





stargazer(regs,
          out="tex/double_robust_estimator.tex",
          type="latex",
          keep=regvars,
          dep.var.labels = c("Equity Growth","Raised Series B",
                             "Equity Growth"),
          model.names=FALSE,
          column.labels=c("OLS","Double LASSO",
                          "OLS","Double LASSO",
                          "OLS","Double LASSO"),
          covariate.labels = cb_startups.labels[regvars],
          keep.stat = c("n","rsq"),
          title="Double LASSO Estimates of the Main Effect of Early Stage VC Financing",
          omit.table.layout = "n",
          float=FALSE
)

