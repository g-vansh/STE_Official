########################################################################
##
## Study the determinants of the strategic treatment effect.
##
#########################################################################

store_lasso_coef_table <- function(ste_features, filepath,title, remove_years = F, store_output=T){

  ste_features<- ste_features  %>%
    mutate(Variable = gsub("^Iind","Industry:",Variable)) %>%
    mutate(Variable = gsub("early_stage_amount_log","Log(Early Stage Amount)",Variable))%>%
    mutate(Variable = gsub("\\_"," ",Variable))%>%
    mutate(Variable = gsub("Iregion","Region:",Variable))%>%
    mutate(Variable = gsub("Icity","City:",Variable))%>%
    mutate(Variable = gsub("Iind","x Industry:",Variable))%>%
    mutate(Variable = gsub("Iyear","Year",Variable))%>%
    mutate(Variable = gsub("^X ","",Variable))%>%
    mutate(Variable = gsub("^\\s*ed","School:",Variable))%>%
    mutate(Variable = gsub("([a-z])([A-Z])","\\1 \\2",Variable))

  ste_features <- ste_features %>%
    filter(!grepl("Intercept",Variable))

  if(remove_years) {

    ste_features <- ste_features %>%
      filter(!grepl("^Year",Variable))

  }

  n = nrow(ste_features)
  to_print <- rbind(ste_features[1:20,],ste_features[(n-20):n,])

  outtex1 <- capture.output(stargazer(ste_features[1:20,], summary = FALSE,  title=title,
                       rownames=TRUE))

  outtex2 <- capture.output(stargazer(ste_features[(n-20):n,], summary = FALSE,  title=title,
                       rownames=TRUE))


  outtex <- starpolishr::star_panel(outtex1, outtex2, reg=F,
                                    panel.names=c("Most Positive Features","Most Negative Features"))
  outtex <- gsub("cccc","clcc",outtex)

  outtex <- gsub("^.*(begin|end)\\{table\\}.*$","",outtex)
  outtex <- gsub("^.*\\caption\\{.*$","",outtex)
  print(paste0(outtex))

  if(store_output) {
    out<- file(filepath)
    writeLines(outtex,out)
    close(out)

  }
}


store_lasso_coef_table(ste_features = ste_features_eq, filepath="tex/top_features_equity_growth.tex",
                       title="Strategic Determinant Function: Top Features for Equity Growth Treatment Effect",
                       remove_years = T)

store_lasso_coef_table(ste_features = ste_features_b, file="tex/top_features_series_b.tex",
                       title="Strategic Determinant Function: Top Features for Series B Treatment Effect",
                       remove_years = T)

# Univariate estimates

cb_startups$pweights <- ifelse(cb_startups$bearly_stage_has_vc == 1 ,
                               1/cb_startups$predicted_early_stage_vc_rf,
                               1/(1-cb_startups$predicted_early_stage_vc_rf))

cb_startups <- cb_startups %>%
  mutate(pweights = ifelse(is.infinite(pweights),0,pweights))

m.eq <- lm(equity_growth ~ bearly_stage_has_vc,weights=pweights, data=cb_startups)
m.series_a <- lm(series_a_has_investment ~ bearly_stage_has_vc,weights=pweights, data=cb_startups)
m.series_b <- lm(series_b_has_investment ~ bearly_stage_has_vc,weights=pweights, data=cb_startups)

stargazer(m.eq, m.series_a, m.series_b,
          out="tex/prop_score_weight_regression.tex",
          title="Propensity Score Weighted Treatment Effects",
          keep.stat = c("n"),
          float=FALSE)
