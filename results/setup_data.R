#!/user/vg2547/R/x86_64-pc-linux-gnu-library/4.1
rm(list =ls())

library(readstata13)
library(glmnet)
library(randomForest)
library(pROC)
#library(vroom)
library(dotwhisker)
library(rcompanion)
library(caret)
library(repr)
library(plyr)
library(binsreg)
library(stargazer)
library(ggplot2)
library(gtools)
library(tidyr)

library(dplyr)

library(hdm)
library(stringr)
library(lfe)
library(sandwich)


library(AER)
library(ggpubr)



### Logic Flags
flags.double_lasso <- T
flags.train_r_model <- T

### Load Data

setwd("/shared/share_scp/STE_Official/results")




cb_startups <- read.csv("/shared/share_scp/STE/cb_startups.csv") %>%
  filter(early_stage_has_investment==1) %>%
  filter(early_stage_amount > 0)# & early_stage_amount < 20000000)



cols <- names(cb_startups)
cb_startups = cb_startups %>%
  mutate(early_stage_amount_log = log(early_stage_amount),
         bearly_stage_has_vc = as.double(cb_startups$bearly_stage_has_vc),
         early_stage_bin = floor(log10(early_stage_amount)*2))

ml_vars <- c(cols[grepl("^I.*", cols)],
             cols[grepl(".*_ed.*", cols)],
              cols[grepl("^ed.*", cols)],"early_stage_bin")
sum(is.na(cb_startups[,ml_vars]))

colSums(is.na(cb_startups[,ml_vars]))
dim(cb_startups)


for(v in ml_vars){
  cb_startups[,v] = replace_na(cb_startups[,v],0)
}

table(cb_startups$bearly_stage_has_vc)

ml_vars <- ml_vars[!grepl(".*ed_.*_has_.*",ml_vars)]
ml_vars <- ml_vars[!grepl(".*has_vc",ml_vars)]

ml_vars.only_education <- ml_vars[grepl("^ed_[^_]*$",ml_vars)]

print("Creating education industry interactions")
#Interactions of education and industry
for(edcol in ml_vars.only_education){
  for(indcol in cols[grepl("^Iind*",cols)]){
    newvar <- paste0("X_",edcol,"_",indcol)
    #print(newvar)?
    x <- cb_startups[[edcol]] * cb_startups[[indcol]]
    if (sum(x,na.rm=T) > 10){
      cb_startups[[newvar]]<- cb_startups[[edcol]] * cb_startups[[indcol]]
    }
  }
}



print("Creating city industry interactions")
#Interactions of education and industry
for(citycol in cols[grepl("^Icity",cols)]){
  for(indcol in cols[grepl("^Iind*",cols)]){
    newvar <- paste0("X_",citycol,"_",indcol)
    #print(newvar)
    x <- cb_startups[[citycol]] * cb_startups[[indcol]]
    if (sum(x, na.rm = T) > 10){
      cb_startups[[newvar]]<- cb_startups[[citycol]] * cb_startups[[indcol]]
    }

  }
}


cols <- names(cb_startups)
ml_vars <- c(ml_vars ,  cols[grepl("^X_*",cols)])
print(paste0("A total of ", length(ml_vars)," as ML variables"))
length(ml_vars)


labnames <- c("raised_amount",
              "seed_amount",
              "angel_amount","early_stage_amount",

                      "series_a_amount",
                      "series_b_amount",
                      "bseed_has_vc",
                      "bpre_seed_has_vc",
                      "bearly_stage_has_vc",
                      "equity_growth",
                      "ipo","acquired")

cb_startups.labels <- c("Amount Raised Total",
                        "Seed Amount Raised",
                        "Angel Funding Amount Raised",
                        "Early Stage Amount Raised",
                        "Series A Amount Raised",
                        "Series B Amount Raised",
                        "Seed VC",
                        "Pre Seed VC",
                        "Early Stage VC",
                        "Equity Growth",
                        "IPO",
                        "Acquisition")
names(cb_startups.labels) <- labnames


cb_startups.labels
dim(cb_startups)




########################################################################
##
## Consider selection across VC financing
##
#########################################################################
early_stage_amount <- cb_startups$early_stage_amount

p1 <- ggplot() +
  geom_histogram(aes(x=early_stage_amount,y=..density..),
                 data=cb_startups%>% filter(bearly_stage_has_vc==0),
                 fill="grey",  color="black") +

  scale_x_log10(breaks = c(1000,10000,100000,10^6,10^7)) +
  theme_classic() +
  xlab("Early Stage Amount") +
  ggtitle("No Early Stage Venture Capital Received")

p2 <- ggplot() +
  geom_histogram(aes(x=early_stage_amount,y=..density..),
                 data=cb_startups%>% filter(bearly_stage_has_vc==1),
                 fill="white",  color="black") +

  scale_x_log10(breaks = c(1000,10000,100000,10^6,10^7)) +
  theme_classic() +
  xlab("Early Stage Amount") +
  ggtitle("Has Early Stage Venture Capital Received")


ggarrange(p1, p2, ncol=1, nrow=2)

ggsave("tex/vc_distribution_all_early_stage.png")


geom_histogram(aes(x=early_stage_amount,y=..density..),
               data=cb_startups%>% filter(bearly_stage_has_vc==1),
               fill="blue", alpha=.4,color="navy")

#focus only on founds from 100,000 to 10 million, which overlap fro seed and vc.
cb_startups <- cb_startups %>%
  filter(early_stage_amount > 10^5 & early_stage_amount < 10^7)




save(cb_startups,ml_vars,cb_startups.labels,file="data/cb_startups.Rdata")
