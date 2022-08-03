########################################################################
##
## Study outcomes across the predicted probability of selection.
##
#########################################################################

## The distribution of outcomes
#It is useful to note that while there is selection into who gets early stage financing, this selection is not as linear as one may exect.

N = nrow(cb_startups)
cb_startups <- cb_startups %>%
  arrange(predicted_early_stage_vc_rf) %>%
  mutate(bin = floor((1:N-1)*20/N))

cb_startups.binned <- cb_startups %>%
  dplyr::group_by(bin) %>%
  dplyr::summarise(bin_mean=mean(predicted_early_stage_vc_rf),
                   equity_growth = mean(equity_growth),
                   acquired = mean(acquired),
                   ipo=mean(ipo),
                   gets_vc = mean(bearly_stage_has_vc),
                   series_a = mean(series_a_has_investment),
                   series_b = mean(series_b_has_investment))



cb_startups <-  cb_startups.binned%>%
  select(bin, bin_mean) %>%
  right_join(cb_startups, by=c("bin"))




p.vc <- ggplot(aes(x=bin_mean,y=gets_vc), data=cb_startups.binned) +
  geom_point() + theme_classic()+ylab("")+
  geom_smooth(method="lm", se=F) +
  xlab("Prob. of Early Stage VC")+
  theme(plot.title = element_text(size = 10)) +
  ggtitle("A. Early Stage VC")
plot(p.vc)

p.eq <- ggplot(aes(x=bin_mean,y=equity_growth), data=cb_startups.binned) +
  geom_point() + theme_classic()+ylab("")+
  geom_smooth(method="lm", se=F) +
  xlab("Prob. of Early Stage VC")+
  theme(plot.title = element_text(size = 10)) +
  ggtitle("D. Equity Growth")


p.a <- ggplot(aes(x=bin_mean,y=series_a), data=cb_startups.binned) +
  geom_point() + theme_classic()+ylab("")+
  geom_smooth(method="lm", se=F) +
  xlab("Prob. of Early Stage VC")+
  theme(plot.title = element_text(size = 10)) +
  ggtitle("B. Series A Financing")


p.b <- ggplot(aes(x=bin_mean,y=series_b), data=cb_startups.binned) +
  geom_point() + theme_classic()+ylab("")+
  geom_smooth(method="lm", se=F) +
  xlab("Prob. of Early Stage VC")+
  theme(plot.title = element_text(size = 10)) +
  ggtitle("C. Series B Financing")


p.acq <- ggplot(aes(x=bin_mean,y=acquired), data=cb_startups.binned) +
  geom_point() + theme_classic()+ylab("")+
  geom_smooth(method="lm", se=F) +
  xlab("Prob. of Early Stage VC")+
  theme(plot.title = element_text(size = 10)) +

  ggtitle("E. Acquired")



p.ipo <- ggplot(aes(x=bin_mean,y=ipo), data=cb_startups.binned) +
  geom_point() + theme_classic()+ylab("")+
  geom_smooth(method="lm", se=F) +
  xlab("Prob. of Early Stage VC")+ theme(plot.title = element_text(size = 10)) +

  ggtitle("F. IPO")


ggarrange(p.vc,  p.a, p.b, p.eq,p.acq, p.ipo)
ggsave("tex/binscatters_p_vc_by_outcome.png", width = 7, height=5)
