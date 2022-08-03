# Explore RF results
library(ggplot2)
library(ggpubr)
library(pROC)
library(STE)

p1 <- ggplot(data=cb_startups)+
  geom_histogram(aes(x=predicted_early_stage_vc_rf), bins=50, color="black",fill="white") +
  theme_classic() +
  xlab("Estimated Prob. of Receiving Early Stage VC")

plot(p1)

ggsave("tex/pred_histogram.png", plot=p1, width=6, height=4)

roc_obj  <- roc(bearly_stage_has_vc ~ predicted_early_stage_vc_rf, data=cb_startups)
ggroc(roc_obj,size=3) +
  theme_classic() + ggtitle(paste0("Out of Sample AUC: ",round(roc_obj$auc,2)))

ggsave("tex/roc_graph.png",width=5, height=5)


# Study complete model
# We now print out all the main features that predict this treatment into latex.

load("/shared/share_scp/STE_Official/results/data/rf_full_model.Rdata")

feature_importance <- randomForest::importance(rf_full_model)
typeof(feature_importance)

feature_names <- row.names(feature_importance)
feature_importance<- data.frame(feature_importance)
feature_importance$features <- row.names(feature_importance)
head(feature_importance)

feature_importance <- feature_importance %>%
  arrange(-MeanDecreaseAccuracy) %>%
  mutate(rank = 1:nrow(feature_importance)) %>%
  select(rank, features, MeanDecreaseAccuracy)

library(dplyr)
feature_importance.tex <- feature_importance %>%
  filter(!grepl("^Iyear",features)) %>%
  mutate(features= gsub("^ +","",features)) %>%
  mutate(features = gsub("early_stage_amount_log","Log(Early Stage Amount)",features)) %>%
  mutate(features = gsub("early_stage_bin","Early Stage Financing Bin",features)) %>%
  mutate(features = gsub("^Iind","Industry:",features)) %>%
  mutate(features = gsub("\\_"," ",features))%>%
  mutate(features = gsub("^X ","",features))%>%
  mutate(features = gsub("Icity","City:",features))%>%
  mutate(features = gsub("Iregion","State:",features))%>%
  mutate(features = gsub("^\\s*ed ","School: ",features))%>%
  mutate(features = gsub("Iind","x Industry:",features))%>%
  mutate(features = gsub("([a-z])([A-Z])","\\1 \\2",features))%>%
  dplyr::rename("Mean Decrease Accuracy"="MeanDecreaseAccuracy",
         "Variable"="features")

library(stargazer)
outtex <- stargazer(feature_importance.tex[1:40,], summary = FALSE,
                    rownames=FALSE,
                    omit.table.layout = "n",
                    float=FALSE)
outtex <- gsub("ccc","clc", outtex)
out=file("tex/top_features_random_forest.tex")
writeLines(outtex,out)
close(out)
