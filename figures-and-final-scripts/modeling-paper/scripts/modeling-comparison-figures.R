# Final comparison figure production for modeling paper


# Formatting settings for figures -----------------------------------------

image.width = 8
image.height = 5



# Importing, cleaning, and data modification ------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)

df <- read_csv("FINAL dataset.csv")


# Here, I filter the dataset to only include the relevant variables (cow ID, potential predictors, health events) and restrict the dataset to the timepoints 3-10 DIM. I also remove 7 jersey cows from the dataset.
df = df %>% 
  select(cow_id, dim, milkweightlbs, parity, dim_met, dim_da, dim_ket, dim_cull, dim_mast, lactose, protein, mun, fatb70a30, denovofa, mixedfa, preformedfa, dnrel, mixrel, pfrel, c160, c180, c181c9, acetone, nefaPREDICTED, bhbPREDICTED) %>% 
  filter(dim<=10 & dim>=3) %>% 
  filter(cow_id != 30351 & cow_id != 38267 & cow_id != 38469 & cow_id != 38836 & cow_id != 60614 & cow_id != 60619 & cow_id != 60630)

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


#
#
#
#

### End of initial data modification





# Lactose -----------------------------------------------------------------

df$sick = as.factor(df$sick)
levels(df$sick) = c("Healthy", "Sick")

# Lactose
dta = df %>% 
  group_by(dim, sick) %>% 
  summarise(se = sd(lactose) / sqrt(n()), lactose = mean(lactose))

lactose.plot = ggplot(data = dta, aes(x = dim, y = lactose, color = sick, group = sick)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$lactose - 1.96*dta$se, ymax = dta$lactose + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("Lactose, %") + labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("deepskyblue", "red2")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18))

ggsave(plot = lactose.plot, filename = "figures-and-final-scripts/modeling-paper/figures/lactose.png", width = image.width, height = image.height)






# Protein -----------------------------------------------------------------

# Protein
dta = df %>% 
  group_by(dim, sick) %>% 
  summarise(se = sd(protein) / sqrt(n()), protein = mean(protein))

protein.plot = ggplot(data = dta, aes(x = dim, y = protein, color = sick, group = sick)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$protein - 1.96*dta$se, ymax = dta$protein + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("Protein, %") + labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("deepskyblue", "red2")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18))

ggsave(plot = protein.plot, filename = "figures-and-final-scripts/modeling-paper/figures/protein.png", width = image.width, height = image.height)






# Milk urea nitrogen ------------------------------------------------------

# MUN
dta = df %>% 
  group_by(dim, sick) %>% 
  summarise(se = sd(mun) / sqrt(n()), mun = mean(mun))

mun.plot = ggplot(data = dta, aes(x = dim, y = mun, color = sick, group = sick)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$mun - 1.96*dta$se, ymax = dta$mun + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("Milk urea nitrogen, mg/dL") + labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("deepskyblue", "red2")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18))

ggsave(plot = mun.plot, filename = "figures-and-final-scripts/modeling-paper/figures/mun.png", width = image.width, height = image.height)





# Fat ---------------------------------------------------------------------

# Fat
dta = df %>% 
  group_by(dim, sick) %>% 
  summarise(se = sd(fatb70a30) / sqrt(n()), fatb70a30 = mean(fatb70a30))

fat.plot = ggplot(data = dta, aes(x = dim, y = fatb70a30, color = sick, group = sick)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$fatb70a30 - 1.96*dta$se, ymax = dta$fatb70a30 + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("Fat, %") + labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("deepskyblue", "red2")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18))

ggsave(plot = fat.plot, filename = "figures-and-final-scripts/modeling-paper/figures/fat.png", width = image.width, height = image.height)





# De novo fatty acids -----------------------------------------------------

# De Novo FA (0.7 to 3.5)
dta = df %>% 
  group_by(dim, sick) %>% 
  summarise(se = sd(denovofa) / sqrt(n()), denovofa = mean(denovofa))

denovo.plot = ggplot(data = dta, aes(x = dim, y = denovofa, color = sick, group = sick)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$denovofa - 1.96*dta$se, ymax = dta$denovofa + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("De novo fatty acids, g/100g milk") + labs(color=NULL) + ylim(0.7,4) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("deepskyblue", "red2")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18))

ggsave(plot = denovo.plot, filename = "figures-and-final-scripts/modeling-paper/figures/denovo.png", width = image.width, height = image.height)





# Mixed fatty acids -------------------------------------------------------

# Mixed FA
dta = df %>% 
  group_by(dim, sick) %>% 
  summarise(se = sd(mixedfa) / sqrt(n()), mixedfa = mean(mixedfa))

mixed.plot = ggplot(data = dta, aes(x = dim, y = mixedfa, color = sick, group = sick)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$mixedfa - 1.96*dta$se, ymax = dta$mixedfa + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("Mixed fatty acids, g/100g milk") + labs(color=NULL) + ylim(0.7,4) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("deepskyblue", "red2")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18))

ggsave(plot = mixed.plot, filename = "figures-and-final-scripts/modeling-paper/figures/mixed.png", width = image.width, height = image.height)





# Preformed fatty acids ---------------------------------------------------

# Preformed FA
dta = df %>% 
  group_by(dim, sick) %>% 
  summarise(se = sd(preformedfa) / sqrt(n()), preformedfa = mean(preformedfa))

preformed.plot = ggplot(data = dta, aes(x = dim, y = preformedfa, color = sick, group = sick)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$preformedfa - 1.96*dta$se, ymax = dta$preformedfa + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("Preformed fatty acids, g/100g milk") + labs(color=NULL) + ylim(0.7,4) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("deepskyblue", "red2")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18))

ggsave(plot = preformed.plot, filename = "figures-and-final-scripts/modeling-paper/figures/preformed.png", width = image.width, height = image.height)





# Rel. % De Novo fatty acids ----------------------------------------------

# Relative % of De Novo FA
dta = df %>% 
  group_by(dim, sick) %>% 
  summarise(se = sd(dnrel) / sqrt(n()), dnrel = mean(dnrel))

reldenovo.plot = ggplot(data = dta, aes(x = dim, y = dnrel, color = sick, group = sick)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$dnrel - 1.96*dta$se, ymax = dta$dnrel + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("De novo fatty acids, relative %") + labs(color=NULL) + ylim(10,60) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("deepskyblue", "red2")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18))

ggsave(plot = reldenovo.plot, filename = "figures-and-final-scripts/modeling-paper/figures/rel_denovo.png", width = image.width, height = image.height)





# Rel. % mixed fatty acids ------------------------------------------------

# Relative % of Mixed FA
dta = df %>% 
  group_by(dim, sick) %>% 
  summarise(se = sd(mixrel) / sqrt(n()), mixrel = mean(mixrel))

relmixed.plot = ggplot(data = dta, aes(x = dim, y = mixrel, color = sick, group = sick)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$mixrel - 1.96*dta$se, ymax = dta$mixrel + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("Mixed fatty acids, relative %") + labs(color=NULL) + ylim(10,60) + 
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("deepskyblue", "red2")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18))

ggsave(plot = relmixed.plot, filename = "figures-and-final-scripts/modeling-paper/figures/rel_mixed.png", width = image.width, height = image.height)





# Rel. % preformed fatty acids --------------------------------------------

# Relative % of Preformed FA
dta = df %>% 
  group_by(dim, sick) %>% 
  summarise(se = sd(pfrel) / sqrt(n()), pfrel = mean(pfrel))

relpreformed.plot = ggplot(data = dta, aes(x = dim, y = pfrel, color = sick, group = sick)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$pfrel - 1.96*dta$se, ymax = dta$pfrel + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("Preformed fatty acids, relative %") + labs(color=NULL) + ylim(10,60) + 
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("deepskyblue", "red2")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18))

ggsave(plot = relpreformed.plot, filename = "figures-and-final-scripts/modeling-paper/figures/rel_preformed.png", width = image.width, height = image.height)





# C16:0 -------------------------------------------------------------------

# c160 (0.6 - 1.7)
dta = df %>% 
  group_by(dim, sick) %>% 
  summarise(se = sd(c160) / sqrt(n()), c160 = mean(c160))

c160.plot = ggplot(data = dta, aes(x = dim, y = c160, color = sick, group = sick)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$c160 - 1.96*dta$se, ymax = dta$c160 + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("C16:0, g/100g milk") + labs(color=NULL) + ylim(0.5,2) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("deepskyblue", "red2")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18))

ggsave(plot = c160.plot, filename = "figures-and-final-scripts/modeling-paper/figures/c160.png", width = image.width, height = image.height)





# C18:0 -------------------------------------------------------------------

# c180
dta = df %>% 
  group_by(dim, sick) %>% 
  summarise(se = sd(c180) / sqrt(n()), c180 = mean(c180))

c180.plot = ggplot(data = dta, aes(x = dim, y = c180, color = sick, group = sick)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$c180 - 1.96*dta$se, ymax = dta$c180 + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("C18:0, g/100g milk") + labs(color=NULL) + ylim(0.5,2) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("deepskyblue", "red2")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18))

ggsave(plot = c180.plot, filename = "figures-and-final-scripts/modeling-paper/figures/c180.png", width = image.width, height = image.height)





# C18:1 cis-9 -------------------------------------------------------------

# c181c9
dta = df %>% 
  group_by(dim, sick) %>% 
  summarise(se = sd(c181c9) / sqrt(n()), c181c9 = mean(c181c9))

c181c9.plot = ggplot(data = dta, aes(x = dim, y = c181c9, color = sick, group = sick)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$c181c9 - 1.96*dta$se, ymax = dta$c181c9 + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("C18:1 cis-9, g/100g milk") + labs(color=NULL) + ylim(0.5,2) + 
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("deepskyblue", "red2")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18))

ggsave(plot = c181c9.plot, filename = "figures-and-final-scripts/modeling-paper/figures/c181c9.png", width = image.width, height = image.height)





# Acetone -----------------------------------------------------------------

# Acetone
dta = df %>% 
  group_by(dim, sick) %>% 
  summarise(se = sd(acetone) / sqrt(n()), acetone = mean(acetone))

acetone.plot = ggplot(data = dta, aes(x = dim, y = acetone, color = sick, group = sick)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$acetone - 1.96*dta$se, ymax = dta$acetone + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("Acetone, mmol/L") + labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("deepskyblue", "red2")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18))

ggsave(plot = acetone.plot, filename = "figures-and-final-scripts/modeling-paper/figures/acetone.png", width = image.width, height = image.height)





# Predicted blood NEFA ----------------------------------------------------

# nefaPREDICTED
df.scaled.nefa = df %>% 
  mutate(nefaPREDICTED = nefaPREDICTED/1000)

dta = df.scaled.nefa %>% 
  group_by(dim, sick) %>% 
  summarise(se = sd(nefaPREDICTED) / sqrt(n()), nefaPREDICTED = mean(nefaPREDICTED))

nefa.plot = ggplot(data = dta, aes(x = dim, y = nefaPREDICTED, color = sick, group = sick)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$nefaPREDICTED - 1.96*dta$se, ymax = dta$nefaPREDICTED + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("Predicted blood NEFA, mmol/L") + labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("deepskyblue", "red2")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18))

ggsave(plot = nefa.plot, filename = "figures-and-final-scripts/modeling-paper/figures/nefa.png", width = image.width, height = image.height)





# BHB ---------------------------------------------------------------------

#bhbPREDICTED


dta = df %>% 
  group_by(dim, sick) %>% 
  summarise(se = sd(bhbPREDICTED) / sqrt(n()), bhbPREDICTED = mean(bhbPREDICTED))

bhb.plot = ggplot(data = dta, aes(x = dim, y = bhbPREDICTED, color = sick, group = sick)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = dta$bhbPREDICTED - 1.96*dta$se, ymax = dta$bhbPREDICTED + 1.96*dta$se), alpha = 0.1) +
  xlab("Days in milk") + ylab("BHB, mmol/L") + labs(color=NULL) +
  scale_x_continuous(breaks = 3:10) +
  scale_color_manual(values=c("deepskyblue", "red2")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size=18))

ggsave(plot = bhb.plot, filename = "figures-and-final-scripts/modeling-paper/figures/bhb.png", width = image.width, height = image.height)


