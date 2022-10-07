# Plotting least square mean results
library(tidyverse)


# Formatting settings for figures -----------------------------------------
image.width = 8
image.height = 5




# Lactose -----------------------------------------------------------------

lactose = read_csv("figures-and-final-scripts/descriptive-paper/ls-mean-data/ls_lactose.csv") %>% 
  mutate(Estimate = sqrt(Estimate),
         Lower = sqrt(Lower),
         Upper = sqrt(Upper))

lactose$pargrp = as.factor(lactose$pargrp)
levels(lactose$pargrp) = c("Parity 1", "Parity 2", "Parity 3+")

lactose.plot = ggplot(data = lactose, aes(x = dim, y = Estimate, color = pargrp, group = pargrp)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = lactose$Lower, ymax = lactose$Upper), alpha = 0.1) +
  xlab("Days in milk") + ylab("Lactose, %") + labs(color = NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "right")

ggsave(plot = lactose.plot, filename = "figures-and-final-scripts/descriptive-paper/figures/lactose.png", width = image.width, height = image.height)






# Protein -----------------------------------------------------------------

protein = read_csv("figures-and-final-scripts/descriptive-paper/ls-mean-data/ls_protein.csv") %>% 
  mutate(Estimate = Estimate^2,
         Lower = Lower^2,
         Upper = Upper^2)

protein$pargrp = as.factor(protein$pargrp)
levels(protein$pargrp) = c("Parity 1", "Parity 2", "Parity 3+")

protein.plot = ggplot(data = protein, aes(x = dim, y = Estimate, color = pargrp, group = pargrp)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = protein$Lower, ymax = protein$Upper), alpha = 0.1) +
  xlab("Days in milk") + ylab("Protein, %") + labs(color = NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "right")

ggsave(plot = protein.plot, filename = "figures-and-final-scripts/descriptive-paper/figures/protein.png", width = image.width, height = image.height)





# Fat ---------------------------------------------------------------------

fat = read_csv("figures-and-final-scripts/descriptive-paper/ls-mean-data/ls_fat.csv") %>% 
  mutate(Estimate = 10^Estimate,
         Lower = 10^Lower,
         Upper = 10^Upper)

fat$pargrp = as.factor(fat$pargrp)
levels(fat$pargrp) = c("Parity 1", "Parity 2", "Parity 3+")

fat.plot = ggplot(data = fat, aes(x = dim, y = Estimate, color = pargrp, group = pargrp)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = fat$Lower, ymax = fat$Upper), alpha = 0.1) +
  xlab("Days in milk") + ylab("Fat, %") + labs(color = NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "right")

ggsave(plot = fat.plot, filename = "figures-and-final-scripts/descriptive-paper/figures/fat.png", width = image.width, height = image.height)




# MUN ---------------------------------------------------------------------

mun = read_csv("figures-and-final-scripts/descriptive-paper/ls-mean-data/ls_mun.csv") %>% 
  mutate(Estimate = Estimate^2,
         Lower = Lower^2,
         Upper = Upper^2)

mun$pargrp = as.factor(mun$pargrp)
levels(mun$pargrp) = c("Parity 1", "Parity 2", "Parity 3+")

mun.plot = ggplot(data = mun, aes(x = dim, y = Estimate, color = pargrp, group = pargrp)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = mun$Lower, ymax = mun$Upper), alpha = 0.1) +
  xlab("Days in milk") + ylab("Milk urea nitrogen, mg/dL") + labs(color = NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "right")

ggsave(plot = mun.plot, filename = "figures-and-final-scripts/descriptive-paper/figures/mun.png", width = image.width, height = image.height)




# De Novo -----------------------------------------------------------------

denovo = read_csv("figures-and-final-scripts/descriptive-paper/ls-mean-data/ls_denovo.csv")

denovo$pargrp = as.factor(denovo$pargrp)
levels(denovo$pargrp) = c("Parity 1", "Parity 2", "Parity 3+")

denovo.plot = ggplot(data = denovo, aes(x = dim, y = Estimate, color = pargrp, group = pargrp)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = denovo$Lower, ymax = denovo$Upper), alpha = 0.1) +
  xlab("Days in milk") + ylab("De novo fatty acids, g/100 g milk") + labs(color = NULL) + ylim(0.7,3.5) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "right")

ggsave(plot = denovo.plot, filename = "figures-and-final-scripts/descriptive-paper/figures/denovo.png", width = image.width, height = image.height)




# Mixed -------------------------------------------------------------------

mixed = read_csv("figures-and-final-scripts/descriptive-paper/ls-mean-data/ls_mixed.csv") %>% 
  mutate(Estimate = Estimate^2,
         Lower = Lower^2,
         Upper = Upper^2)

mixed$pargrp = as.factor(mixed$pargrp)
levels(mixed$pargrp) = c("Parity 1", "Parity 2", "Parity 3+")

mixed.plot = ggplot(data = mixed, aes(x = dim, y = Estimate, color = pargrp, group = pargrp)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = mixed$Lower, ymax = mixed$Upper), alpha = 0.1) +
  xlab("Days in milk") + ylab("Mixed fatty acids, g/100 g milk") + labs(color = NULL) + ylim(0.7,3.5) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "right")

ggsave(plot = mixed.plot, filename = "figures-and-final-scripts/descriptive-paper/figures/mixed.png", width = image.width, height = image.height)




# Preformed ---------------------------------------------------------------

preformed = read_csv("figures-and-final-scripts/descriptive-paper/ls-mean-data/ls_preformed.csv")

preformed$pargrp = as.factor(preformed$pargrp)
levels(preformed$pargrp) = c("Parity 1", "Parity 2", "Parity 3+")

preformed.plot = ggplot(data = preformed, aes(x = dim, y = Estimate, color = pargrp, group = pargrp)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = preformed$Lower, ymax = preformed$Upper), alpha = 0.1) +
  xlab("Days in milk") + ylab("Preformed fatty acids, g/100 g milk") + labs(color = NULL) + ylim(0.7,3.5) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "right")

ggsave(plot = preformed.plot, filename = "figures-and-final-scripts/descriptive-paper/figures/preformed.png", width = image.width, height = image.height)




# Rel De Novo -------------------------------------------------------------

reldenovo = read_csv("figures-and-final-scripts/descriptive-paper/ls-mean-data/ls_reldenovo.csv")

reldenovo$pargrp = as.factor(reldenovo$pargrp)
levels(reldenovo$pargrp) = c("Parity 1", "Parity 2", "Parity 3+")

reldenovo.plot = ggplot(data = reldenovo, aes(x = dim, y = Estimate, color = pargrp, group = pargrp)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = reldenovo$Lower, ymax = reldenovo$Upper), alpha = 0.1) +
  xlab("Days in milk") + ylab("De novo fatty acids, rel %") + labs(color = NULL) + ylim(10,60) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "right")

ggsave(plot = reldenovo.plot, filename = "figures-and-final-scripts/descriptive-paper/figures/rel-denovo.png", width = image.width, height = image.height)




# Rel Mixed ---------------------------------------------------------------

relmixed = read_csv("figures-and-final-scripts/descriptive-paper/ls-mean-data/ls_relmixed.csv") %>% 
  mutate(Estimate = Estimate^2,
         Lower = Lower^2,
         Upper = Upper^2)

relmixed$pargrp = as.factor(relmixed$pargrp)
levels(relmixed$pargrp) = c("Parity 1", "Parity 2", "Parity 3+")

relmixed.plot = ggplot(data = relmixed, aes(x = dim, y = Estimate, color = pargrp, group = pargrp)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = relmixed$Lower, ymax = relmixed$Upper), alpha = 0.1) +
  xlab("Days in milk") + ylab("Mixed fatty acids, rel %") + labs(color = NULL) + ylim(10,60) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "right")

ggsave(plot = relmixed.plot, filename = "figures-and-final-scripts/descriptive-paper/figures/rel-mixed.png", width = image.width, height = image.height)




# Rel Preformed -----------------------------------------------------------

relpreformed = read_csv("figures-and-final-scripts/descriptive-paper/ls-mean-data/ls_relpreformed.csv") %>% 
  mutate(Estimate = Estimate^2,
         Lower = Lower^2,
         Upper = Upper^2)

relpreformed$pargrp = as.factor(relpreformed$pargrp)
levels(relpreformed$pargrp) = c("Parity 1", "Parity 2", "Parity 3+")

relpreformed.plot = ggplot(data = relpreformed, aes(x = dim, y = Estimate, color = pargrp, group = pargrp)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = relpreformed$Lower, ymax = relpreformed$Upper), alpha = 0.1) +
  xlab("Days in milk") + ylab("Preformed fatty acids, rel %") + labs(color = NULL) + ylim(10,60) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "right")

ggsave(plot = relpreformed.plot, filename = "figures-and-final-scripts/descriptive-paper/figures/rel-preformed.png", width = image.width, height = image.height)




# C160 --------------------------------------------------------------------

c160 = read_csv("figures-and-final-scripts/descriptive-paper/ls-mean-data/ls_c160.csv") %>% 
  mutate(Estimate = Estimate^2,
         Lower = Lower^2,
         Upper = Upper^2)

c160$pargrp = as.factor(c160$pargrp)
levels(c160$pargrp) = c("Parity 1", "Parity 2", "Parity 3+")

c160.plot = ggplot(data = c160, aes(x = dim, y = Estimate, color = pargrp, group = pargrp)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = c160$Lower, ymax = c160$Upper), alpha = 0.1) +
  xlab("Days in milk") + ylab("C16:0, g/100 g milk") + labs(color = NULL) + ylim(0.4,1.8) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "right")

ggsave(plot = c160.plot, filename = "figures-and-final-scripts/descriptive-paper/figures/c160.png", width = image.width, height = image.height)




# C180 --------------------------------------------------------------------

c180 = read_csv("figures-and-final-scripts/descriptive-paper/ls-mean-data/ls_c180.csv") %>% 
  mutate(Estimate = Estimate^2,
         Lower = Lower^2,
         Upper = Upper^2)

c180$pargrp = as.factor(c180$pargrp)
levels(c180$pargrp) = c("Parity 1", "Parity 2", "Parity 3+")

c180.plot = ggplot(data = c180, aes(x = dim, y = Estimate, color = pargrp, group = pargrp)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = c180$Lower, ymax = c180$Upper), alpha = 0.1) +
  xlab("Days in milk") + ylab("C18:0, g/100 g milk") + labs(color = NULL) + ylim(0.4,1.8) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "right")

ggsave(plot = c180.plot, filename = "figures-and-final-scripts/descriptive-paper/figures/c180.png", width = image.width, height = image.height)



# C181c9 ------------------------------------------------------------------

c181c9 = read_csv("figures-and-final-scripts/descriptive-paper/ls-mean-data/ls_c181c9.csv") %>% 
  mutate(Estimate = Estimate^2,
         Lower = Lower^2,
         Upper = Upper^2)

c181c9$pargrp = as.factor(c181c9$pargrp)
levels(c181c9$pargrp) = c("Parity 1", "Parity 2", "Parity 3+")

c181c9.plot = ggplot(data = c181c9, aes(x = dim, y = Estimate, color = pargrp, group = pargrp)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = c181c9$Lower, ymax = c181c9$Upper), alpha = 0.1) +
  xlab("Days in milk") + ylab("C18:1 cis-9, g/100 g milk") + labs(color = NULL) + ylim(0.4,1.8) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "right")

ggsave(plot = c181c9.plot, filename = "figures-and-final-scripts/descriptive-paper/figures/c181c9.png", width = image.width, height = image.height)




# Acetone -----------------------------------------------------------------

acetone = read_csv("figures-and-final-scripts/descriptive-paper/ls-mean-data/ls_acetone.csv")

acetone$pargrp = as.factor(acetone$pargrp)
levels(acetone$pargrp) = c("Parity 1", "Parity 2", "Parity 3+")

acetone.plot = ggplot(data = acetone, aes(x = dim, y = Estimate, color = pargrp, group = pargrp)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = acetone$Lower, ymax = acetone$Upper), alpha = 0.1) +
  xlab("Days in milk") + ylab("mACE, mmol/L") + labs(color = NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "right")

ggsave(plot = acetone.plot, filename = "figures-and-final-scripts/descriptive-paper/figures/acetone.png", width = image.width, height = image.height)




# BHB ---------------------------------------------------------------------

bhb = read_csv("figures-and-final-scripts/descriptive-paper/ls-mean-data/ls_bhb.csv")

bhb$pargrp = as.factor(bhb$pargrp)
levels(bhb$pargrp) = c("Parity 1", "Parity 2", "Parity 3+")

bhb.plot = ggplot(data = bhb, aes(x = dim, y = Estimate, color = pargrp, group = pargrp)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = bhb$Lower, ymax = bhb$Upper), alpha = 0.1) +
  xlab("Days in milk") + ylab("mBHB, mmol/L") + labs(color = NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "right")

ggsave(plot = bhb.plot, filename = "figures-and-final-scripts/descriptive-paper/figures/bhb.png", width = image.width, height = image.height)




# NEFA --------------------------------------------------------------------

nefa = read_csv("figures-and-final-scripts/descriptive-paper/ls-mean-data/ls_nefa.csv") %>% 
  mutate(Estimate = Estimate/1000,
         Lower = Lower/1000,
         Upper = Upper/1000)

nefa$pargrp = as.factor(nefa$pargrp)
levels(nefa$pargrp) = c("Parity 1", "Parity 2", "Parity 3+")

nefa.plot = ggplot(data = nefa, aes(x = dim, y = Estimate, color = pargrp, group = pargrp)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = nefa$Lower, ymax = nefa$Upper), alpha = 0.1) +
  xlab("Days in milk") + ylab("mpbNEFA, mmol/L") + labs(color = NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "right")

ggsave(plot = nefa.plot, filename = "figures-and-final-scripts/descriptive-paper/figures/nefa.png", width = image.width, height = image.height)


