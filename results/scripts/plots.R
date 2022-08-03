cb_startups$series_b.teffect <- cb_startups_b$teffect
cb_startups$acq.teffect <- cb_startups_acq$teffect
cb_startups$series_b.ste <- cb_startups_b$ste
cb_startups$acq.ste <- cb_startups_acq$ste
cb_startups$teffect <- cb_startups_eq$teffect
cb_startups$ste <- cb_startups_eq$ste

ate = mean(cb_startups$teffect, na.rm=T)
series_b.ate = mean(cb_startups$series_b.teffect, na.rm=T)

library(ggplot2)
library(ggpubr)
p1 <- ggplot(aes(x=teffect), data=cb_startups) +
  geom_histogram(show.legend = FALSE,color="black", fill="white") +
  scale_fill_discrete(h = c(240, 10)) +
  theme_classic() +
  xlab("") +
  ggtitle("A. Histogram of Equity Growth \n Total Treatment Effect") +
  theme(plot.title = element_text(size = 10)) +
  geom_vline(xintercept = ate, linetype="dashed") +
  xlim(-0.15,0.15)

plot(p1)

p2 <- ggplot(aes(x=series_b.teffect), data=cb_startups) +
  geom_histogram(show.legend = FALSE,color="black", fill="white") +
  scale_fill_discrete(h = c(240, 10)) +
  theme_classic() +
  xlab("") +
  ggtitle("B. Histogram of Series B \n Total Treatment Effect") +
  theme(plot.title = element_text(size = 10)) +
  geom_vline(xintercept = series_b.ate, linetype="dashed") +
  xlim(-0.15,0.10)

plot(p2)

p3 <- ggplot(aes(x=teffect-ate), data=cb_startups) +
  geom_histogram(show.legend = FALSE,color="black", fill="white") +
  scale_fill_discrete(h = c(240, 10)) +
  theme_classic() +
  xlab("") +
  ggtitle("C. Histogram of Equity Growth \n Strategic Treatment Effect Only")+
  theme(plot.title = element_text(size = 10)) +
  xlim(-0.15,0.15)

p4 <- ggplot(aes(x=series_b.teffect-series_b.ate), data=cb_startups) +
  geom_histogram(show.legend = FALSE,color="black", fill="white") +
  scale_fill_discrete(h = c(240, 10)) +
  theme_classic() +
  xlab("") +
  theme(plot.title = element_text(size = 10)) +
  ggtitle("D. Histogram of Series B \n Strategic Treatment Effect Only")  +
  xlim(-0.15,0.10)

ggarrange(p1, p2, p3, p4, ncol=2,nrow=2)
ggsave("tex/dist_treatment_effect.png", width=7, height=4)


library(dplyr)

cb_startups.plot <- cb_startups %>%
  arrange(series_b.teffect) %>%
  dplyr::mutate(bin = floor((dplyr::row_number()-1)*20/n())) %>%
  group_by(bin) %>%
  dplyr::summarise(mean_series_b = mean(series_b.teffect),
            mean_eq = mean(teffect))





ggplot(aes(y=mean_eq, x = mean_series_b), data=cb_startups.plot)+
  geom_point(size=3) +
  ylab("Treatment Effect for Equity Growth") +
  xlab("Treatment Effect for Raising Series B")+
  geom_hline(yintercept = 0, linetype="dashed")+
  geom_vline(xintercept = 0, linetype="dashed")+
  theme_classic()

ggsave("tex/corr_plot_treatment_effects.png", width=8, height=5)

pos_bins <- cb_startups.plot %>%
  filter(bearly_stage_has_vc == 1) %>%
  group_by(bin) %>%
  dplyr::summarise(eq_mean = mean(equity_growth))

neg_bins <- cb_startups %>%
  filter(bearly_stage_has_vc == 0) %>%
  group_by(bin) %>%
  dplyr::summarise(eq_mean = mean(equity_growth))

p1<-ggplot() +
  stat_summary(aes(x=bin_mean, y=equity_growth),data = cb_startups %>% filter(bearly_stage_has_vc == 1),
               geom="point", fun="mean", col="blue", size=3,shape=2) +
  stat_summary(aes(x=bin_mean, y=equity_growth),data = cb_startups %>% filter(bearly_stage_has_vc == 0),
               geom="point", fun="mean", col="red", size=3) +
  theme_classic() +
  ylab("Prob of Equity Growth") +
  ggtitle("A. Equity Growth")

plot(p1)

p2<-ggplot(data=cb_startups) +
  stat_summary(aes(x=bin_mean, y=series_b_has_investment),data = cb_startups %>% filter(bearly_stage_has_vc == 1),
               geom="point", fun="mean", col="blue", size=3, shape=2) +
  stat_summary(aes(x=bin_mean, y=series_b_has_investment),data = cb_startups %>% filter(bearly_stage_has_vc == 0),
               geom="point", fun="mean", col="red", size=3) +
  theme_classic() +
  ylab("Prob of Series B") +
  ggtitle("B. Series B")

p3<-ggplot(data=cb_startups) +
  stat_summary(aes(x=bin_mean, y=acquired),data = cb_startups %>% filter(bearly_stage_has_vc == 1),
               geom="point", fun="mean", col="blue", size=3, shape=2) +
  stat_summary(aes(x=bin_mean, y=acquired),data = cb_startups %>% filter(bearly_stage_has_vc == 0),
               geom="point", fun="mean", col="red", size=3) +
  theme_classic() +
  ylab("Prob of Acquisition") +
  ggtitle("C. Acquisition")

p4<-ggplot(data=cb_startups) +
  stat_summary(aes(x=bin_mean, y=acq_price),data = cb_startups %>% filter(bearly_stage_has_vc == 1),
               geom="point", fun="mean", col="blue", size=3, shape=2) +
  stat_summary(aes(x=bin_mean, y=acq_price),data = cb_startups %>% filter(bearly_stage_has_vc == 0),
               geom="point", fun="mean", col="red", size=3) +
  theme_classic() +
  ylab("Dollars") +
  ggtitle("D. Acquisition Price")

ggarrange(p1, p2, p3, p4,  ncol=2,nrow=2)
ggsave("tex/binscatter_positive_and_neg.png", width=7, height=4)
