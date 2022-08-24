# Script for generating final descriptive figures


# Formatting settings for figures -----------------------------------------

image.width = 8
image.height = 5




# Loading packages --------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)




# Data import and cleaning ------------------------------------------------

df <- read_csv("FINAL dataset.csv")


# Here, I filter the dataset to only include the relevant variables (cow ID, potential predictors, health events) and restrict the dataset to the timepoints 3-10 DIM. I also remove 7 jersey cows from the dataset.
df = df %>% 
  select(cow_id, dim, milkweightlbs, parity, dim_met, dim_da, dim_ket, dim_cull, dim_mast, lactose, protein, mun, fatb70a30, denovofa, mixedfa, preformedfa, dnrel, mixrel, pfrel, c160,
         c180, c181c9, acetone, nefaPREDICTED, bhbPREDICTED) %>% 
  filter(dim<=10 & dim>=3) %>% 
  filter(cow_id != 30351 & cow_id != 38267 & cow_id != 38469 & cow_id != 38836 & 
           cow_id != 60614 & cow_id != 60619 & cow_id != 60630)

# This creates binary variables for health outcomes. We only want to consider illness within the first 14 days because that is when most cows leave the pen of concern. A cow is therefore considered "sick" if it has metritis, ketosis, or DA.
df$metritis = 0; df$metritis[!is.na(df$dim_met) & df$dim_met <= 14] = 1
df$DA = 0; df$DA[!is.na(df$dim_da) & df$dim_da <= 14] = 1
df$ketosis = 0; df$ketosis[!is.na(df$dim_ket) & df$dim_ket <= 14] = 1
df$cull = 0; df$cull[!is.na(df$dim_cull)] = 1
df$mastitis = 0; df$mastitis[!is.na(df$dim_mast)] = 1
df$sick= 0; df$sick[df$metritis + df$DA + df$ketosis != 0] = 1
df$multiparous = 0; df$multiparous[df$parity>1] = 1
df$multiparous = as.factor(df$multiparous)


### Removes milk collections from dataset after they get sick (from ketosis, DA, metritis, or mastitis). We suspect that the milk composition will be disrupted, and we only are interested in predicting health outcomes from samples taken before they are diagnosed as sick. Mastitis is not a health outcome of concern for this study, but we highly suspect it will significantly impact milk composition.
df = df %>% 
  filter( (dim<=dim_ket | is.na(dim_ket)) &
            (dim<=dim_da | is.na(dim_da)) &
            (dim<=dim_met | is.na(dim_met)) &
            (dim<=dim_mast | is.na(dim_mast)) )

df$multi = 0; df$multi[df$parity == 2] = 1; df$multi[df$parity >= 3] = 2
df$multi = as.factor(df$multi)
levels(df$multi) = c("Parity 1", "Parity 2", "Parity 3+")

#
#
#
#

### End of initial data modification



# Lactose -----------------------------------------------------------------

# Lactose
dta = df %>% 
  group_by(dim, multi) %>% 
  summarise(se = sd(lactose) / sqrt(n()), lactose = mean(lactose))

lactose.plot = ggplot(data = dta, aes(x = dim, y = lactose, color = multi, group = multi)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$lactose - 1.96*dta$se, ymax = dta$lactose + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("Lactose, %") + labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "none")

ggsave(plot = lactose.plot, filename = "figures-and-final-scripts/figures/lactose.png", width = image.width, height = image.height)




# Protein -----------------------------------------------------------------

# Protein
dta = df %>% 
  group_by(dim, multi) %>% 
  summarise(se = sd(protein) / sqrt(n()), protein = mean(protein))

protein.plot = ggplot(data = dta, aes(x = dim, y = protein, color = multi, group = multi)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$protein - 1.96*dta$se, ymax = dta$protein + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("Protein, %") + labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "none")

ggsave(plot = protein.plot, filename = "figures-and-final-scripts/figures/protein.png", width = image.width, height = image.height)




# Milk Urea Nitrogen ------------------------------------------------------

# MUN
dta = df %>% 
  group_by(dim, multi) %>% 
  summarise(se = sd(mun) / sqrt(n()), mun = mean(mun))

mun.plot = ggplot(data = dta, aes(x = dim, y = mun, color = multi, group = multi)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$mun - 1.96*dta$se, ymax = dta$mun + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("Milk urea nitrogen, mg/dL") + labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "none")

ggsave(plot = mun.plot, filename = "figures-and-final-scripts/figures/mun.png", width = image.width, height = image.height)




# Fat ---------------------------------------------------------------------

# Fat
dta = df %>% 
  group_by(dim, multi) %>% 
  summarise(se = sd(fatb70a30) / sqrt(n()), fatb70a30 = mean(fatb70a30))

fat.plot = ggplot(data = dta, aes(x = dim, y = fatb70a30, color = multi, group = multi)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$fatb70a30 - 1.96*dta$se, ymax = dta$fatb70a30 + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("Fat, %") + labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "none")

ggsave(plot = fat.plot, filename = "figures-and-final-scripts/figures/fat.png", width = image.width, height = image.height)





# De novo fatty acids -----------------------------------------------------

# De Novo FA
dta = df %>% 
  group_by(dim, multi) %>% 
  summarise(se = sd(denovofa) / sqrt(n()), denovofa = mean(denovofa))

denovo.plot = ggplot(data = dta, aes(x = dim, y = denovofa, color = multi, group = multi)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$denovofa - 1.96*dta$se, ymax = dta$denovofa + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("De novo fatty acids, g/100 g milk") + labs(color=NULL) + ylim(0.7,3.5) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "none")

ggsave(plot = denovo.plot, filename = "figures-and-final-scripts/figures/denovo.png", width = image.width, height = image.height)





# Mixed fatty acids -------------------------------------------------------

# Mixed FA
dta = df %>% 
  group_by(dim, multi) %>% 
  summarise(se = sd(mixedfa) / sqrt(n()), mixedfa = mean(mixedfa))

mixed.plot = ggplot(data = dta, aes(x = dim, y = mixedfa, color = multi, group = multi)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$mixedfa - 1.96*dta$se, ymax = dta$mixedfa + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("Mixed fatty acids, g/100 g milk") + labs(color=NULL) + ylim(0.7,3.5) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "none")

ggsave(plot = mixed.plot, filename = "figures-and-final-scripts/figures/mixed.png", width = image.width, height = image.height)




# Preformed fatty acids ---------------------------------------------------

# Preformed FA
dta = df %>% 
  group_by(dim, multi) %>% 
  summarise(se = sd(preformedfa) / sqrt(n()), preformedfa = mean(preformedfa))

preformed.plot = ggplot(data = dta, aes(x = dim, y = preformedfa, color = multi, group = multi)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$preformedfa - 1.96*dta$se, ymax = dta$preformedfa + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("Preformed fatty acids, g/100 g milk") + labs(color=NULL) + ylim(0.7,3.5) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "none")

ggsave(plot = preformed.plot, filename = "figures-and-final-scripts/figures/preformed.png", width = image.width, height = image.height)




# Rel. % de novo fatty acids ----------------------------------------------

# Relative % of De Novo FA
dta = df %>% 
  group_by(dim, multi) %>% 
  summarise(se = sd(dnrel) / sqrt(n()), dnrel = mean(dnrel))

rel.denovo.plot = ggplot(data = dta, aes(x = dim, y = dnrel, color = multi, group = multi)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$dnrel - 1.96*dta$se, ymax = dta$dnrel + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("De novo fatty acids, rel %") + labs(color=NULL) + ylim(10,60) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "none")

ggsave(plot = rel.denovo.plot, filename = "figures-and-final-scripts/figures/rel-denovo.png", width = image.width, height = image.height)



# Rel. % mixed fatty acids ------------------------------------------------

# Relative % of Mixed FA
dta = df %>% 
  group_by(dim, multi) %>% 
  summarise(se = sd(mixrel) / sqrt(n()), mixrel = mean(mixrel))

rel.mixed.plot = ggplot(data = dta, aes(x = dim, y = mixrel, color = multi, group = multi)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$mixrel - 1.96*dta$se, ymax = dta$mixrel + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("Mixed fatty acids, rel %") + labs(color=NULL) + ylim(10,60) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "none")

ggsave(plot = rel.mixed.plot, filename = "figures-and-final-scripts/figures/rel-mixed.png", width = image.width, height = image.height)




# Rel. % preformed fatty acids --------------------------------------------

# Relative % of Preformed FA
dta = df %>% 
  group_by(dim, multi) %>% 
  summarise(se = sd(pfrel) / sqrt(n()), pfrel = mean(pfrel))

rel.preformed.plot = ggplot(data = dta, aes(x = dim, y = pfrel, color = multi, group = multi)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$pfrel - 1.96*dta$se, ymax = dta$pfrel + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("Preformed fatty acids, rel %") + labs(color=NULL) + ylim(10,60) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "none")

ggsave(plot = rel.preformed.plot, filename = "figures-and-final-scripts/figures/rel-preformed.png", width = image.width, height = image.height)




# C16:0 -------------------------------------------------------------------

# c160
dta = df %>% 
  group_by(dim, multi) %>% 
  summarise(se = sd(c160) / sqrt(n()), c160 = mean(c160))

c160.plot = ggplot(data = dta, aes(x = dim, y = c160, color = multi, group = multi)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$c160 - 1.96*dta$se, ymax = dta$c160 + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("C16:0, g/100 g milk") + labs(color=NULL) + ylim(0.4,1.8) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "none")

ggsave(plot = c160.plot, filename = "figures-and-final-scripts/figures/c160.png", width = image.width, height = image.height)




# C18:0 -------------------------------------------------------------------

# c180
dta = df %>% 
  group_by(dim, multi) %>% 
  summarise(se = sd(c180) / sqrt(n()), c180 = mean(c180))

c180.plot = ggplot(data = dta, aes(x = dim, y = c180, color = multi, group = multi)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$c180 - 1.96*dta$se, ymax = dta$c180 + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("C18:0, g/100 g milk") + labs(color=NULL) + ylim(0.4,1.8) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "none")

ggsave(plot = c180.plot, filename = "figures-and-final-scripts/figures/c180.png", width = image.width, height = image.height)




# C18:1 cis-9 -------------------------------------------------------------

# c181c9
dta = df %>% 
  group_by(dim, multi) %>% 
  summarise(se = sd(c181c9) / sqrt(n()), c181c9 = mean(c181c9))

c181c9.plot = ggplot(data = dta, aes(x = dim, y = c181c9, color = multi, group = multi)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$c181c9 - 1.96*dta$se, ymax = dta$c181c9 + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("C18:1 cis-9, g/100 g milk") + labs(color=NULL) + ylim(0.4,1.8) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "none")

ggsave(plot = c181c9.plot, filename = "figures-and-final-scripts/figures/c181c9.png", width = image.width, height = image.height)




# Acetone -----------------------------------------------------------------

# Acetone
dta = df %>% 
  group_by(dim, multi) %>% 
  summarise(se = sd(acetone) / sqrt(n()), acetone = mean(acetone))

acetone.plot = ggplot(data = dta, aes(x = dim, y = acetone, color = multi, group = multi)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$acetone - 1.96*dta$se, ymax = dta$acetone + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("mACE, mmol/L") + labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "none")

ggsave(plot = acetone.plot, filename = "figures-and-final-scripts/figures/acetone.png", width = image.width, height = image.height)




# Predicted blood NEFA ----------------------------------------------------

# nefaPREDICTED
df.scaled.nefa = df %>% 
  mutate(nefaPREDICTED = nefaPREDICTED/1000)

dta = df.scaled.nefa %>% 
  group_by(dim, multi) %>% 
  summarise(se = sd(nefaPREDICTED) / sqrt(n()), nefaPREDICTED = mean(nefaPREDICTED))

nefa.plot = ggplot(data = dta, aes(x = dim, y = nefaPREDICTED, color = multi, group = multi)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$nefaPREDICTED - 1.96*dta$se, ymax = dta$nefaPREDICTED + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("mpbNEFA, mmol/L") + labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "none")

ggsave(plot = nefa.plot, filename = "figures-and-final-scripts/figures/nefa.png", width = image.width, height = image.height)




# BHB ---------------------------------------------------------------------

#bhbPREDICTED
dta = df %>% 
  group_by(dim, multi) %>% 
  summarise(se = sd(bhbPREDICTED) / sqrt(n()), bhbPREDICTED = mean(bhbPREDICTED))

bhb.plot = ggplot(data = dta, aes(x = dim, y = bhbPREDICTED, color = multi, group = multi)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$bhbPREDICTED - 1.96*dta$se, ymax = dta$bhbPREDICTED + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("mBHB, mmol/L") + labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("mediumpurple", "darkorange", "deepskyblue1")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18),
        legend.position = "none")

ggsave(plot = bhb.plot, filename = "figures-and-final-scripts/figures/bhb.png", width = image.width, height = image.height)

