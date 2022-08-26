# Final figure production for modeling paper
#source("figures-and-final-scripts/modeling-paper/scripts/modeling-analysis.R")



# Sensitivity plot --------------------------------------------------------

sens.df = data.frame(dim = 3:10, sens = se, sens.low = se.low, sens.high = se.hi, cv = "no")
cv.sens.df = data.frame(dim = 3:10, sens = cv.avg.sens, sens.low = cv.sens.low, sens.high = cv.sens.high, cv = "yes")

sens.df = sens.df %>% bind_rows(cv.sens.df)
sens.df$cv = as.factor(sens.df$cv)
levels(sens.df$cv) = c("Full model", "Cross-validated model")

sens.plot = ggplot(data = sens.df, aes(x = dim, y = sens, color = cv, group = cv)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = sens.low, ymax = sens.high), alpha = 0.1) +
  xlab("Days in milk") +
  ylab("Sensitivity") + ylim(0,1) +
  labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) + 
  scale_color_manual(values=c("darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "top")

ggsave(plot = sens.plot, filename = "figures-and-final-scripts/modeling-paper/figures/sensitivity.png", width = 8, height = 5)






# Specificity plot --------------------------------------------------------

spec.df = data.frame(dim = 3:10, spec = sp, spec.low = sp.low, spec.high = sp.hi, cv = "no")
cv.spec.df = data.frame(dim = 3:10, spec = cv.avg.spec, spec.low = cv.spec.low, spec.high = cv.spec.high, cv = "yes")

spec.df = spec.df %>% bind_rows(cv.spec.df)
spec.df$cv = as.factor(spec.df$cv)
levels(spec.df$cv) = c("Full model", "Cross-validated model")

spec.plot = ggplot(data = spec.df, aes(x = dim, y = spec, color = cv, group = cv)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = spec.low, ymax = spec.high), alpha = 0.1) +
  xlab("Days in milk") +
  ylab("Specificity") + ylim(0,1) +
  labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) + 
  scale_color_manual(values=c("darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "top")

ggsave(plot = spec.plot, filename = "figures-and-final-scripts/modeling-paper/figures/specificity.png", width = 8, height = 5)







# Positive predictive value plot ------------------------------------------

ppv.df = data.frame(dim = 3:10, ppv = ppv, ppv.low = ppv.low, ppv.high = ppv.hi, cv = "no")
cv.ppv.df = data.frame(dim = 3:10, ppv = cv.avg.ppv, ppv.low = cv.ppv.low, ppv.high = cv.ppv.high, cv = "yes")

ppv.df = ppv.df %>% bind_rows(cv.ppv.df)
ppv.df$cv = as.factor(ppv.df$cv)
levels(ppv.df$cv) = c("Full model", "Cross-validated model")

ppv.plot = ggplot(data = ppv.df, aes(x = dim, y = ppv, color = cv, group = cv)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = ppv.low, ymax = ppv.high), alpha = 0.1) +
  xlab("Days in milk") +
  ylab("Positive predictive value") + ylim(0,1) +
  labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) + 
  scale_color_manual(values=c("darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "top")

ggsave(plot = ppv.plot, filename = "figures-and-final-scripts/modeling-paper/figures/ppv.png", width = 8, height = 5)






# Negative predictive value plot ------------------------------------------

npv.df = data.frame(dim = 3:10, npv = npv, npv.low = npv.low, npv.high = npv.hi, cv = "no")
cv.npv.df = data.frame(dim = 3:10, npv = cv.avg.npv, npv.low = cv.npv.low, npv.high = cv.npv.high, cv = "yes")

npv.df = npv.df %>% bind_rows(cv.npv.df)
npv.df$cv = as.factor(npv.df$cv)
levels(npv.df$cv) = c("Full model", "Cross-validated model")

npv.plot = ggplot(data = npv.df, aes(x = dim, y = npv, color = cv, group = cv)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = npv.low, ymax = npv.high), alpha = 0.1) +
  xlab("Days in milk") +
  ylab("Negative predictive value") +
  labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) + 
  ylim(0,1) +
  scale_color_manual(values=c("darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "top")

ggsave(plot = npv.plot, filename = "figures-and-final-scripts/modeling-paper/figures/npv.png", width = 8, height = 5)





# Cross validated AUC plot ------------------------------------------------

auc.df = data.frame(dim = 3:10, AUC = AUC, AUC.low = AUC.low, AUC.high = AUC.high, cv = "no")
cv.auc.df = data.frame(dim = 3:10, AUC = cv.avg.auc, AUC.low = cv.auc.low, AUC.high = cv.auc.high, cv = "yes")

auc.df = auc.df %>% bind_rows(cv.auc.df)
auc.df$cv = as.factor(auc.df$cv)
levels(auc.df$cv) = c("Full model", "Cross-validated model")

auc.plot = ggplot(data = auc.df, aes(x = dim, y = AUC, color = cv, group = cv)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = AUC.low, ymax = AUC.high), alpha = 0.1) +
  xlab("Days in milk") +
  ylab("Area under ROC curve") + ylim(0,1) +
  labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) + 
  scale_color_manual(values=c("darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "top")

ggsave(plot = auc.plot, filename = "figures-and-final-scripts/modeling-paper/figures/auc.png", width = 8, height = 5)



