# Final analysis script for modeling paper
# Author: Eric Teplitz


# Importing, cleaning, and initial data modification ----------------------

library(readr)
library(tidyr)
library(dplyr)
library(pls)
library(ggplot2)
library(pROC)
library(epiR)

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



# Descriptive metrics for manuscript --------------------------------------

# collapsed.df = df %>%
#   group_by(cow_id) %>%
#   summarize(metritis = mean(metritis),
#             ketosis = mean(ketosis),
#             DA = mean(DA),
#             cull = mean(cull),
#             mastitis = mean(mastitis),
#             sick = mean(sick))
# 
# collapsed.df %>% count(metritis)
# collapsed.df %>% count(ketosis)
# collapsed.df %>% count(DA)
# collapsed.df %>% count(cull)
# collapsed.df %>% count(mastitis)
# collapsed.df %>% count(sick)



# Partial least squares regression ----------------------------------------

# The dataset was stratified into 8 separate datasets for each day in milk (3-10 DIM)
df3 = df %>% filter(dim == 3)
df4 = df %>% filter(dim == 4)
df5 = df %>% filter(dim == 5)
df6 = df %>% filter(dim == 6)
df7 = df %>% filter(dim == 7)
df8 = df %>% filter(dim == 8)
df9 = df %>% filter(dim == 9)
df10 = df %>% filter(dim == 10)


# Running partial least squares regression for each timepoint
PLS3 <- plsr(sick ~ lactose+protein+mun+fatb70a30+denovofa+mixedfa+preformedfa+dnrel+mixrel+pfrel+c160+c180+c181c9+acetone+nefaPREDICTED+bhbPREDICTED+multiparous, 
             data=df3, scale=T)
PLS4 <- plsr(sick ~ lactose+protein+mun+fatb70a30+denovofa+mixedfa+preformedfa+dnrel+mixrel+pfrel+c160+c180+c181c9+acetone+nefaPREDICTED+bhbPREDICTED+multiparous, 
             data=df4, scale=T)
PLS5 <- plsr(sick ~ lactose+protein+mun+fatb70a30+denovofa+mixedfa+preformedfa+dnrel+mixrel+pfrel+c160+c180+c181c9+acetone+nefaPREDICTED+bhbPREDICTED+multiparous, 
             data=df5, scale=T)
PLS6 <- plsr(sick ~ lactose+protein+mun+fatb70a30+denovofa+mixedfa+preformedfa+dnrel+mixrel+pfrel+c160+c180+c181c9+acetone+nefaPREDICTED+bhbPREDICTED+multiparous, 
             data=df6, scale=T)
PLS7 <- plsr(sick ~ lactose+protein+mun+fatb70a30+denovofa+mixedfa+preformedfa+dnrel+mixrel+pfrel+c160+c180+c181c9+acetone+nefaPREDICTED+bhbPREDICTED+multiparous, 
             data=df7, scale=T)
PLS8 <- plsr(sick ~ lactose+protein+mun+fatb70a30+denovofa+mixedfa+preformedfa+dnrel+mixrel+pfrel+c160+c180+c181c9+acetone+nefaPREDICTED+bhbPREDICTED+multiparous, 
             data=df8, scale=T)
PLS9 <- plsr(sick ~ lactose+protein+mun+fatb70a30+denovofa+mixedfa+preformedfa+dnrel+mixrel+pfrel+c160+c180+c181c9+acetone+nefaPREDICTED+bhbPREDICTED+multiparous, 
             data=df9, scale=T)
PLS10 <- plsr(sick ~ lactose+protein+mun+fatb70a30+denovofa+mixedfa+preformedfa+dnrel+mixrel+pfrel+c160+c180+c181c9+acetone+nefaPREDICTED+bhbPREDICTED+multiparous, 
              data=df10, scale=T)





# Calculating predictive probability and AUC ------------------------------

# Calculating predictive probability for each PLS model
df3$predict.prob = predict(PLS3, ncomp = 3, type = "response")
df4$predict.prob = predict(PLS4, ncomp = 3, type = "response")
df5$predict.prob = predict(PLS5, ncomp = 3, type = "response")
df6$predict.prob = predict(PLS6, ncomp = 3, type = "response")
df7$predict.prob = predict(PLS7, ncomp = 3, type = "response")
df8$predict.prob = predict(PLS8, ncomp = 3, type = "response")
df9$predict.prob = predict(PLS9, ncomp = 3, type = "response")
df10$predict.prob = predict(PLS10, ncomp = 3, type = "response")

# Calculating AUC and 95% confidence interval for AUC, for each PLS model
AUC = rep(0,8)
AUC.low = rep(0,8)
AUC.high = rep(0,8)

AUC[1] = auc(df3$sick, df3$predict.prob)
AUC[2] = auc(df4$sick, df4$predict.prob)
AUC[3] = auc(df5$sick, df5$predict.prob)
AUC[4] = auc(df6$sick, df6$predict.prob)
AUC[5] = auc(df7$sick, df7$predict.prob)
AUC[6] = auc(df8$sick, df8$predict.prob)
AUC[7] = auc(df9$sick, df9$predict.prob)
AUC[8] = auc(df10$sick, df10$predict.prob)

AUC.low[1] = ci.auc(df3$sick, df3$predict.prob, conf.level = 0.95)[1]
AUC.low[2] = ci.auc(df4$sick, df4$predict.prob, conf.level = 0.95)[1]
AUC.low[3] = ci.auc(df5$sick, df5$predict.prob, conf.level = 0.95)[1]
AUC.low[4] = ci.auc(df6$sick, df6$predict.prob, conf.level = 0.95)[1]
AUC.low[5] = ci.auc(df7$sick, df7$predict.prob, conf.level = 0.95)[1]
AUC.low[6] = ci.auc(df8$sick, df8$predict.prob, conf.level = 0.95)[1]
AUC.low[7] = ci.auc(df9$sick, df9$predict.prob, conf.level = 0.95)[1]
AUC.low[8] = ci.auc(df10$sick, df10$predict.prob, conf.level = 0.95)[1]

AUC.high[1] = ci.auc(df3$sick, df3$predict.prob, conf.level = 0.95)[3]
AUC.high[2] = ci.auc(df4$sick, df4$predict.prob, conf.level = 0.95)[3]
AUC.high[3] = ci.auc(df5$sick, df5$predict.prob, conf.level = 0.95)[3]
AUC.high[4] = ci.auc(df6$sick, df6$predict.prob, conf.level = 0.95)[3]
AUC.high[5] = ci.auc(df7$sick, df7$predict.prob, conf.level = 0.95)[3]
AUC.high[6] = ci.auc(df8$sick, df8$predict.prob, conf.level = 0.95)[3]
AUC.high[7] = ci.auc(df9$sick, df9$predict.prob, conf.level = 0.95)[3]
AUC.high[8] = ci.auc(df10$sick, df10$predict.prob, conf.level = 0.95)[3]





# Determining a cutpoint --------------------------------------------------

# Loading cutpointr package
library(cutpointr)

# Using this "predicted probability" as a diagnostic test value, we can determine a cutoff used to classify cows as healthy vs. sick
# Below, I use a package called `cutpointr` that will determine a cutpoint for the "predicted probability" that maximizes the sum of the sensitivity and specificity
cp3 = cutpointr(data=df3, x=predict.prob, class=sick, method=maximize_metric, metric=sum_sens_spec)
cp4 = cutpointr(data=df4, x=predict.prob, class=sick, method=maximize_metric, metric=sum_sens_spec)
cp5 = cutpointr(data=df5, x=predict.prob, class=sick, method=maximize_metric, metric=sum_sens_spec)
cp6 = cutpointr(data=df6, x=predict.prob, class=sick, method=maximize_metric, metric=sum_sens_spec)
cp7 = cutpointr(data=df7, x=predict.prob, class=sick, method=maximize_metric, metric=sum_sens_spec)
cp8 = cutpointr(data=df8, x=predict.prob, class=sick, method=maximize_metric, metric=sum_sens_spec)
cp9 = cutpointr(data=df9, x=predict.prob, class=sick, method=maximize_metric, metric=sum_sens_spec)
cp10 = cutpointr(data=df10, x=predict.prob, class=sick, method=maximize_metric, metric=sum_sens_spec)

mergedCP = cp3 %>% bind_rows(cp4,cp5,cp6,cp7,cp8,cp9,cp10)





# Applying the cutpoint ---------------------------------------------------

# Here, I use the PLS-derived "predicted probability" as a diagnostic test
# If the "predicted probability" is above the cutpoint, then I classify the cow as `sick`
# If the "predicted probability" is less than or equal to the cutpoint I classify the cow as `healthy`
# The `confusionMatrix` function then creates a 2x2 table to compare the predicted health status with the true health status.

df3$test.result = 0; df3$test.result[df3$predict.prob > cp3$optimal_cutpoint] = 1
df4$test.result = 0; df4$test.result[df4$predict.prob > cp4$optimal_cutpoint] = 1
df5$test.result = 0; df5$test.result[df5$predict.prob > cp5$optimal_cutpoint] = 1
df6$test.result = 0; df6$test.result[df6$predict.prob > cp6$optimal_cutpoint] = 1
df7$test.result = 0; df7$test.result[df7$predict.prob > cp7$optimal_cutpoint] = 1
df8$test.result = 0; df8$test.result[df8$predict.prob > cp8$optimal_cutpoint] = 1
df9$test.result = 0; df9$test.result[df9$predict.prob > cp9$optimal_cutpoint] = 1
df10$test.result = 0; df10$test.result[df10$predict.prob > cp10$optimal_cutpoint] = 1

matrix3 = confusionMatrix(data=as.factor(df3$test.result), reference=as.factor(df3$sick), positive = "1")
matrix4 = confusionMatrix(data=as.factor(df4$test.result), reference=as.factor(df4$sick), positive = "1")
matrix5 = confusionMatrix(data=as.factor(df5$test.result), reference=as.factor(df5$sick), positive = "1")
matrix6 = confusionMatrix(data=as.factor(df6$test.result), reference=as.factor(df6$sick), positive = "1")
matrix7 = confusionMatrix(data=as.factor(df7$test.result), reference=as.factor(df7$sick), positive = "1")
matrix8 = confusionMatrix(data=as.factor(df8$test.result), reference=as.factor(df8$sick), positive = "1")
matrix9 = confusionMatrix(data=as.factor(df9$test.result), reference=as.factor(df9$sick), positive = "1")
matrix10 = confusionMatrix(data=as.factor(df10$test.result), reference=as.factor(df10$sick), positive = "1")





# Calculating sensitivity, specificity, PPV, and NPV ----------------------

# Here, I use the `epi.tests` function within a package called `epiR` to calculate sensitivity, specificity, positive predictive value, and negative predictive values as well as confidence intervals for these measures

# Interestingly, the `epi.tests` function requires a 2x2 matrix as its parameter, but the 2x2 table must be in the reverse orientation as the one returned by the `confusionMatrix` function
# Consequently, first I flip the order of rows/columns to fit requirements of `epi.tests`
# I determine a confidence level of 95% and extract the diagnostic test values from this function
# Note that I must do this for sensitivity, specificity, positive predictive value, and negative predictive value for each of the 8 PLS models.

se = rep(0,8)
se.low = rep(0,8)
se.hi = rep(0,8)

sp = rep(0,8)
sp.low = rep(0,8)
sp.hi = rep(0,8)

ppv = rep(0,8)
ppv.low = rep(0,8)
ppv.hi = rep(0,8)

npv = rep(0,8)
npv.low = rep(0,8)
npv.hi = rep(0,8)


# Day 3
epi.matrix3 = matrix3$table[c(2,1),c(2,1)] # flips the order of rows/columns to fit requirements of epi.tests
epi3 = epi.tests(epi.matrix3, conf.level = 0.95)

se[1] = as.numeric(epi3$detail$se[1])
se.low[1] = as.numeric(epi3$detail$se[2])
se.hi[1] = as.numeric(epi3$detail$se[3])

sp[1] = as.numeric(epi3$detail$sp[1])
sp.low[1] = as.numeric(epi3$detail$sp[2])
sp.hi[1] = as.numeric(epi3$detail$sp[3])

ppv[1] = as.numeric(epi3$detail$pv.pos[1])
ppv.low[1] = as.numeric(epi3$detail$pv.pos[2])
ppv.hi[1] = as.numeric(epi3$detail$pv.pos[3])

npv[1] = as.numeric(epi3$detail$pv.neg[1])
npv.low[1] = as.numeric(epi3$detail$pv.neg[2])
npv.hi[1] = as.numeric(epi3$detail$pv.neg[3])



# Day 4
epi.matrix4 = matrix4$table[c(2,1),c(2,1)] # flips the order of rows/columns to fit requirements of epi.tests
epi4 = epi.tests(epi.matrix4, conf.level = 0.95)

se[2] = as.numeric(epi4$detail$se[1])
se.low[2] = as.numeric(epi4$detail$se[2])
se.hi[2] = as.numeric(epi4$detail$se[3])

sp[2] = as.numeric(epi4$detail$sp[1])
sp.low[2] = as.numeric(epi4$detail$sp[2])
sp.hi[2] = as.numeric(epi4$detail$sp[3])

ppv[2] = as.numeric(epi4$detail$pv.pos[1])
ppv.low[2] = as.numeric(epi4$detail$pv.pos[2])
ppv.hi[2] = as.numeric(epi4$detail$pv.pos[3])

npv[2] = as.numeric(epi4$detail$pv.neg[1])
npv.low[2] = as.numeric(epi4$detail$pv.neg[2])
npv.hi[2] = as.numeric(epi4$detail$pv.neg[3])


# Day 5
epi.matrix5 = matrix5$table[c(2,1),c(2,1)] # flips the order of rows/columns to fit requirements of epi.tests
epi5 = epi.tests(epi.matrix5, conf.level = 0.95)

se[3] = as.numeric(epi5$detail$se[1])
se.low[3] = as.numeric(epi5$detail$se[2])
se.hi[3] = as.numeric(epi5$detail$se[3])

sp[3] = as.numeric(epi5$detail$sp[1])
sp.low[3] = as.numeric(epi5$detail$sp[2])
sp.hi[3] = as.numeric(epi5$detail$sp[3])

ppv[3] = as.numeric(epi5$detail$pv.pos[1])
ppv.low[3] = as.numeric(epi5$detail$pv.pos[2])
ppv.hi[3] = as.numeric(epi5$detail$pv.pos[3])

npv[3] = as.numeric(epi5$detail$pv.neg[1])
npv.low[3] = as.numeric(epi5$detail$pv.neg[2])
npv.hi[3] = as.numeric(epi5$detail$pv.neg[3])


# Day 6
epi.matrix6 = matrix6$table[c(2,1),c(2,1)] # flips the order of rows/columns to fit requirements of epi.tests
epi6 = epi.tests(epi.matrix6, conf.level = 0.95)

se[4] = as.numeric(epi6$detail$se[1])
se.low[4] = as.numeric(epi6$detail$se[2])
se.hi[4] = as.numeric(epi6$detail$se[3])

sp[4] = as.numeric(epi6$detail$sp[1])
sp.low[4] = as.numeric(epi6$detail$sp[2])
sp.hi[4] = as.numeric(epi6$detail$sp[3])

ppv[4] = as.numeric(epi6$detail$pv.pos[1])
ppv.low[4] = as.numeric(epi6$detail$pv.pos[2])
ppv.hi[4] = as.numeric(epi6$detail$pv.pos[3])

npv[4] = as.numeric(epi6$detail$pv.neg[1])
npv.low[4] = as.numeric(epi6$detail$pv.neg[2])
npv.hi[4] = as.numeric(epi6$detail$pv.neg[3])


# Day 7
epi.matrix7 = matrix7$table[c(2,1),c(2,1)] # flips the order of rows/columns to fit requirements of epi.tests
epi7 = epi.tests(epi.matrix7, conf.level = 0.95)

se[5] = as.numeric(epi7$detail$se[1])
se.low[5] = as.numeric(epi7$detail$se[2])
se.hi[5] = as.numeric(epi7$detail$se[3])

sp[5] = as.numeric(epi7$detail$sp[1])
sp.low[5] = as.numeric(epi7$detail$sp[2])
sp.hi[5] = as.numeric(epi7$detail$sp[3])

ppv[5] = as.numeric(epi7$detail$pv.pos[1])
ppv.low[5] = as.numeric(epi7$detail$pv.pos[2])
ppv.hi[5] = as.numeric(epi7$detail$pv.pos[3])

npv[5] = as.numeric(epi7$detail$pv.neg[1])
npv.low[5] = as.numeric(epi7$detail$pv.neg[2])
npv.hi[5] = as.numeric(epi7$detail$pv.neg[3])


# Day 8
epi.matrix8 = matrix8$table[c(2,1),c(2,1)] # flips the order of rows/columns to fit requirements of epi.tests
epi8 = epi.tests(epi.matrix8, conf.level = 0.95)

se[6] = as.numeric(epi8$detail$se[1])
se.low[6] = as.numeric(epi8$detail$se[2])
se.hi[6] = as.numeric(epi8$detail$se[3])

sp[6] = as.numeric(epi8$detail$sp[1])
sp.low[6] = as.numeric(epi8$detail$sp[2])
sp.hi[6] = as.numeric(epi8$detail$sp[3])

ppv[6] = as.numeric(epi8$detail$pv.pos[1])
ppv.low[6] = as.numeric(epi8$detail$pv.pos[2])
ppv.hi[6] = as.numeric(epi8$detail$pv.pos[3])

npv[6] = as.numeric(epi8$detail$pv.neg[1])
npv.low[6] = as.numeric(epi8$detail$pv.neg[2])
npv.hi[6] = as.numeric(epi8$detail$pv.neg[3])


# Day 9
epi.matrix9 = matrix9$table[c(2,1),c(2,1)] # flips the order of rows/columns to fit requirements of epi.tests
epi9 = epi.tests(epi.matrix9, conf.level = 0.95)

se[7] = as.numeric(epi9$detail$se[1])
se.low[7] = as.numeric(epi9$detail$se[2])
se.hi[7] = as.numeric(epi9$detail$se[3])

sp[7] = as.numeric(epi9$detail$sp[1])
sp.low[7] = as.numeric(epi9$detail$sp[2])
sp.hi[7] = as.numeric(epi9$detail$sp[3])

ppv[7] = as.numeric(epi9$detail$pv.pos[1])
ppv.low[7] = as.numeric(epi9$detail$pv.pos[2])
ppv.hi[7] = as.numeric(epi9$detail$pv.pos[3])

npv[7] = as.numeric(epi9$detail$pv.neg[1])
npv.low[7] = as.numeric(epi9$detail$pv.neg[2])
npv.hi[7] = as.numeric(epi9$detail$pv.neg[3])


# Day 10
epi.matrix10 = matrix10$table[c(2,1),c(2,1)] # flips the order of rows/columns to fit requirements of epi.tests
epi10 = epi.tests(epi.matrix10, conf.level = 0.95)

se[8] = as.numeric(epi10$detail$se[1])
se.low[8] = as.numeric(epi10$detail$se[2])
se.hi[8] = as.numeric(epi10$detail$se[3])

sp[8] = as.numeric(epi10$detail$sp[1])
sp.low[8] = as.numeric(epi10$detail$sp[2])
sp.hi[8] = as.numeric(epi10$detail$sp[3])

ppv[8] = as.numeric(epi10$detail$pv.pos[1])
ppv.low[8] = as.numeric(epi10$detail$pv.pos[2])
ppv.hi[8] = as.numeric(epi10$detail$pv.pos[3])

npv[8] = as.numeric(epi10$detail$pv.neg[1])
npv.low[8] = as.numeric(epi10$detail$pv.neg[2])
npv.hi[8] = as.numeric(epi10$detail$pv.neg[3])









# Writing the cross-validation function -----------------------------------

# This custom function below is used to cross validate the model

# The input parameters include a dataset and a desired number of groups into which I split the dataset
# For example, if I wanted to cross validate the PLS model at 3 DIM, I could use the parameters `input.data = df3` and `num.groups = 10`
# The function will split df3 into 10 groups randomly. 9/10 groups will be isolated as the "training set" and the remaining group will be isolated as the "testing set"
# Subsequently, I run the PLS regression on the training set
# Next, I calculate the "predicted probabilities" of the training set using the PLS algorithm and determine an optimal cutpoint for this "predicted probability" that maximizes the sum of sensitivity and specificity
# Again, I use 3 PLS components to calculate the "predicted probability".

# Next, I use the PLS model algorithm obtained from the training set and use it to calculate the "predicted probability" for each cow in the testing set using 3 components
# Using the pre-determined cutoff from the training set, I then classify cows as `sick` if the corresponding "predicted probability" is above the cutoff
# Cows with "predicted probability" less than or equal to the cutoff are classified as healthy

# Finally, I create a 2x2 table to compare the predicted health status with the true health status
# The `cross.validate` function returns this 2x2 table.

# Additionally, I calculate the area under the ROC curve and add this value to the 2x2 table object called `matrix`
# This will allow me to calculate a cross-validated AUC for each model
# Note that I must detach the `cutpointr` package before calculating AUC
# The `pROC` package (used to calculate AUC) and the `cutpointr` package use similar language, and consequently detaching the `cutpointr` package eliminates the ambiguity in using the correct function.

# Splitting each dataframe into groups randomly
cross.validate = function(input.data, num.groups) {
  
  selections = sample(1:length(input.data$cow_id), size = length(input.data$cow_id)/num.groups, replace = F)
  
  input.data$dummy = 0; input.data$dummy[selections] = 1
  
  testing.set = subset(input.data, input.data$dummy == 1)
  training.set = subset(input.data, input.data$dummy == 0)
  
  CV.PLS <- plsr(sick ~ lactose+protein+mun+fatb70a30+denovofa+mixedfa+preformedfa+dnrel+mixrel+pfrel+c160+c180+c181c9+acetone+nefaPREDICTED+bhbPREDICTED+multiparous, data=training.set, scale=T, validation = "CV")
  
  training.set$predict.prob = predict(CV.PLS, ncomp = 3, type = "response")
  
  library(cutpointr)
  cv.cp = cutpointr(data=training.set, x=predict.prob, class=sick, method = maximize_metric, metric = sum_sens_spec)
  
  testing.set$predict.prob = predict(CV.PLS, ncomp=3, newdata=testing.set)
  
  testing.set$test.result = 0; testing.set$test.result[testing.set$predict.prob > cv.cp$optimal_cutpoint] = 1
  
  matrix = confusionMatrix(as.factor(testing.set$test.result), reference=as.factor(testing.set$sick), positive="1")
  
  # This line calculates the AUC within this function so that we can subsequently calculate cross validated AUC
  detach("package:cutpointr", unload=TRUE)
  library(pROC)
  matrix$auc = pROC::auc(testing.set$sick, testing.set$predict.prob)
  
  return(matrix)
  
}








# Cross validation of each PLS model --------------------------------------

# SET NUMBER OF BOOTSTRAP REPETITIONS HERE
num.sim = 100
set.seed(1)



# To robustly cross-validate our model, I use the `cross.validate` function as part of a loop
# The code below will repeatedly run the `cross.validate` function `num.sim` times, and for each repetition it will calculate sensitivity, specificity, positive predictive value, negative predictive value, and the area under the ROC curve
# This process is repeated for each PLS model and the number of groups specified in the `cross.validate` function is set at 2
# Consequently, the dataset for each day in milk will be randomly split into two equal groups: a training set and a testing set

# The cross-validate values for sensitivity, specificity, positive predictive value, negative predictive value, and AUC is calculated as the mean value across `num.sim` repetitions
# Bootstrapped 95% confidence intervals are calculated as the 2.5th percentile (lower bound) and 97.5th percentile (upper bound) of the resulting vector of diagnostic values
# For example, a vector will be produced with `num.sim` values for sensitivity
# The 95% confidence interval will be calculated as the 2.5th percentile and 97.5th percentile of this vector

cv.avg.sens = rep(0,8)
cv.sens.low = rep(0,8)
cv.sens.high = rep(0,8)

cv.avg.spec = rep(0,8)
cv.spec.low = rep(0,8)
cv.spec.high = rep(0,8)

cv.avg.ppv = rep(0,8)
cv.ppv.low = rep(0,8)
cv.ppv.high = rep(0,8)

cv.avg.npv = rep(0,8)
cv.npv.low = rep(0,8)
cv.npv.high = rep(0,8)

cv.avg.auc = rep(0,8)
cv.auc.low = rep(0,8)
cv.auc.high = rep(0,8)

cv.sens = rep(0,num.sim)
cv.spec = rep(0,num.sim)
cv.ppv = rep(0,num.sim)
cv.npv = rep(0,num.sim)
cv.auc = rep(0,num.sim)

# DIM 3
for (i in 1:num.sim) {
  
  m = cross.validate(input.data = df3, num.groups = 2)
  cv.sens[i] = as.numeric(m$byClass[1])
  cv.spec[i] = as.numeric(m$byClass[2])
  cv.ppv[i] = as.numeric(m$byClass[3])
  cv.npv[i] = as.numeric(m$byClass[4])
  cv.auc[i] = m$auc
  
}
cv.avg.sens[1] = mean(cv.sens)
cv.sens.low[1] = quantile(cv.sens, probs = 2.5/100, na.rm = T)
cv.sens.high[1] = quantile(cv.sens, probs = 97.5/100, na.rm = T)

cv.avg.spec[1] = mean(cv.spec)
cv.spec.low[1] = quantile(cv.spec, probs = 2.5/100, na.rm = T)
cv.spec.high[1] = quantile(cv.spec, probs = 97.5/100, na.rm = T)

cv.avg.ppv[1] = mean(cv.ppv)
cv.ppv.low[1] = quantile(cv.ppv, probs = 2.5/100, na.rm = T)
cv.ppv.high[1] = quantile(cv.ppv, probs = 97.5/100, na.rm = T)

cv.avg.npv[1] = mean(cv.npv)
cv.npv.low[1] = quantile(cv.npv, probs = 2.5/100, na.rm = T)
cv.npv.high[1] = quantile(cv.npv, probs = 97.5/100, na.rm = T)

cv.avg.auc[1] = mean(cv.auc)
cv.auc.low[1] = quantile(cv.auc, probs = 2.5/100, na.rm = T)
cv.auc.high[1] = quantile(cv.auc, probs = 97.5/100, na.rm = T)



# DIM 4
for (i in 1:num.sim) {
  
  m = cross.validate(input.data = df4, num.groups = 2)
  cv.sens[i] = as.numeric(m$byClass[1])
  cv.spec[i] = as.numeric(m$byClass[2])
  cv.ppv[i] = as.numeric(m$byClass[3])
  cv.npv[i] = as.numeric(m$byClass[4])
  cv.auc[i] = m$auc
  
}
cv.avg.sens[2] = mean(cv.sens)
cv.sens.low[2] = quantile(cv.sens, probs = 2.5/100, na.rm = T)
cv.sens.high[2] = quantile(cv.sens, probs = 97.5/100, na.rm = T)

cv.avg.spec[2] = mean(cv.spec)
cv.spec.low[2] = quantile(cv.spec, probs = 2.5/100, na.rm = T)
cv.spec.high[2] = quantile(cv.spec, probs = 97.5/100, na.rm = T)

cv.avg.ppv[2] = mean(cv.ppv)
cv.ppv.low[2] = quantile(cv.ppv, probs = 2.5/100, na.rm = T)
cv.ppv.high[2] = quantile(cv.ppv, probs = 97.5/100, na.rm = T)

cv.avg.npv[2] = mean(cv.npv)
cv.npv.low[2] = quantile(cv.npv, probs = 2.5/100, na.rm = T)
cv.npv.high[2] = quantile(cv.npv, probs = 97.5/100, na.rm = T)

cv.avg.auc[2] = mean(cv.auc)
cv.auc.low[2] = quantile(cv.auc, probs = 2.5/100, na.rm = T)
cv.auc.high[2] = quantile(cv.auc, probs = 97.5/100, na.rm = T)


# DIM 5
for (i in 1:num.sim) {
  
  m = cross.validate(input.data = df5, num.groups = 2)
  cv.sens[i] = as.numeric(m$byClass[1])
  cv.spec[i] = as.numeric(m$byClass[2])
  cv.ppv[i] = as.numeric(m$byClass[3])
  cv.npv[i] = as.numeric(m$byClass[4])
  cv.auc[i] = m$auc
  
}
cv.avg.sens[3] = mean(cv.sens)
cv.sens.low[3] = quantile(cv.sens, probs = 2.5/100, na.rm = T)
cv.sens.high[3] = quantile(cv.sens, probs = 97.5/100, na.rm = T)

cv.avg.spec[3] = mean(cv.spec)
cv.spec.low[3] = quantile(cv.spec, probs = 2.5/100, na.rm = T)
cv.spec.high[3] = quantile(cv.spec, probs = 97.5/100, na.rm = T)

cv.avg.ppv[3] = mean(cv.ppv)
cv.ppv.low[3] = quantile(cv.ppv, probs = 2.5/100, na.rm = T)
cv.ppv.high[3] = quantile(cv.ppv, probs = 97.5/100, na.rm = T)

cv.avg.npv[3] = mean(cv.npv)
cv.npv.low[3] = quantile(cv.npv, probs = 2.5/100, na.rm = T)
cv.npv.high[3] = quantile(cv.npv, probs = 97.5/100, na.rm = T)

cv.avg.auc[3] = mean(cv.auc)
cv.auc.low[3] = quantile(cv.auc, probs = 2.5/100, na.rm = T)
cv.auc.high[3] = quantile(cv.auc, probs = 97.5/100, na.rm = T)



# DIM 6
for (i in 1:num.sim) {
  
  m = cross.validate(input.data = df6, num.groups = 2)
  cv.sens[i] = as.numeric(m$byClass[1])
  cv.spec[i] = as.numeric(m$byClass[2])
  cv.ppv[i] = as.numeric(m$byClass[3])
  cv.npv[i] = as.numeric(m$byClass[4])
  cv.auc[i] = m$auc
  
}
cv.avg.sens[4] = mean(cv.sens)
cv.sens.low[4] = quantile(cv.sens, probs = 2.5/100, na.rm = T)
cv.sens.high[4] = quantile(cv.sens, probs = 97.5/100, na.rm = T)

cv.avg.spec[4] = mean(cv.spec)
cv.spec.low[4] = quantile(cv.spec, probs = 2.5/100, na.rm = T)
cv.spec.high[4] = quantile(cv.spec, probs = 97.5/100, na.rm = T)

cv.avg.ppv[4] = mean(cv.ppv)
cv.ppv.low[4] = quantile(cv.ppv, probs = 2.5/100, na.rm = T)
cv.ppv.high[4] = quantile(cv.ppv, probs = 97.5/100, na.rm = T)

cv.avg.npv[4] = mean(cv.npv)
cv.npv.low[4] = quantile(cv.npv, probs = 2.5/100, na.rm = T)
cv.npv.high[4] = quantile(cv.npv, probs = 97.5/100, na.rm = T)

cv.avg.auc[4] = mean(cv.auc)
cv.auc.low[4] = quantile(cv.auc, probs = 2.5/100, na.rm = T)
cv.auc.high[4] = quantile(cv.auc, probs = 97.5/100, na.rm = T)


# DIM 7
for (i in 1:num.sim) {
  
  m = cross.validate(input.data = df7, num.groups = 2)
  cv.sens[i] = as.numeric(m$byClass[1])
  cv.spec[i] = as.numeric(m$byClass[2])
  cv.ppv[i] = as.numeric(m$byClass[3])
  cv.npv[i] = as.numeric(m$byClass[4])
  cv.auc[i] = m$auc
  
}
cv.avg.sens[5] = mean(cv.sens)
cv.sens.low[5] = quantile(cv.sens, probs = 2.5/100, na.rm = T)
cv.sens.high[5] = quantile(cv.sens, probs = 97.5/100, na.rm = T)

cv.avg.spec[5] = mean(cv.spec)
cv.spec.low[5] = quantile(cv.spec, probs = 2.5/100, na.rm = T)
cv.spec.high[5] = quantile(cv.spec, probs = 97.5/100, na.rm = T)

cv.avg.ppv[5] = mean(cv.ppv)
cv.ppv.low[5] = quantile(cv.ppv, probs = 2.5/100, na.rm = T)
cv.ppv.high[5] = quantile(cv.ppv, probs = 97.5/100, na.rm = T)

cv.avg.npv[5] = mean(cv.npv)
cv.npv.low[5] = quantile(cv.npv, probs = 2.5/100, na.rm = T)
cv.npv.high[5] = quantile(cv.npv, probs = 97.5/100, na.rm = T)

cv.avg.auc[5] = mean(cv.auc)
cv.auc.low[5] = quantile(cv.auc, probs = 2.5/100, na.rm = T)
cv.auc.high[5] = quantile(cv.auc, probs = 97.5/100, na.rm = T)


# DIM 8
for (i in 1:num.sim) {
  
  m = cross.validate(input.data = df8, num.groups = 2)
  cv.sens[i] = as.numeric(m$byClass[1])
  cv.spec[i] = as.numeric(m$byClass[2])
  cv.ppv[i] = as.numeric(m$byClass[3])
  cv.npv[i] = as.numeric(m$byClass[4])
  cv.auc[i] = m$auc
  
}
cv.avg.sens[6] = mean(cv.sens)
cv.sens.low[6] = quantile(cv.sens, probs = 2.5/100, na.rm = T)
cv.sens.high[6] = quantile(cv.sens, probs = 97.5/100, na.rm = T)

cv.avg.spec[6] = mean(cv.spec)
cv.spec.low[6] = quantile(cv.spec, probs = 2.5/100, na.rm = T)
cv.spec.high[6] = quantile(cv.spec, probs = 97.5/100, na.rm = T)

cv.avg.ppv[6] = mean(cv.ppv)
cv.ppv.low[6] = quantile(cv.ppv, probs = 2.5/100, na.rm = T)
cv.ppv.high[6] = quantile(cv.ppv, probs = 97.5/100, na.rm = T)

cv.avg.npv[6] = mean(cv.npv)
cv.npv.low[6] = quantile(cv.npv, probs = 2.5/100, na.rm = T)
cv.npv.high[6] = quantile(cv.npv, probs = 97.5/100, na.rm = T)

cv.avg.auc[6] = mean(cv.auc)
cv.auc.low[6] = quantile(cv.auc, probs = 2.5/100, na.rm = T)
cv.auc.high[6] = quantile(cv.auc, probs = 97.5/100, na.rm = T)



# DIM 9
for (i in 1:num.sim) {
  
  m = cross.validate(input.data = df9, num.groups = 2)
  cv.sens[i] = as.numeric(m$byClass[1])
  cv.spec[i] = as.numeric(m$byClass[2])
  cv.ppv[i] = as.numeric(m$byClass[3])
  cv.npv[i] = as.numeric(m$byClass[4])
  cv.auc[i] = m$auc
  
}
cv.avg.sens[7] = mean(cv.sens)
cv.sens.low[7] = quantile(cv.sens, probs = 2.5/100, na.rm = T)
cv.sens.high[7] = quantile(cv.sens, probs = 97.5/100, na.rm = T)

cv.avg.spec[7] = mean(cv.spec)
cv.spec.low[7] = quantile(cv.spec, probs = 2.5/100, na.rm = T)
cv.spec.high[7] = quantile(cv.spec, probs = 97.5/100, na.rm = T)

cv.avg.ppv[7] = mean(cv.ppv)
cv.ppv.low[7] = quantile(cv.ppv, probs = 2.5/100, na.rm = T)
cv.ppv.high[7] = quantile(cv.ppv, probs = 97.5/100, na.rm = T)

cv.avg.npv[7] = mean(cv.npv)
cv.npv.low[7] = quantile(cv.npv, probs = 2.5/100, na.rm = T)
cv.npv.high[7] = quantile(cv.npv, probs = 97.5/100, na.rm = T)

cv.avg.auc[7] = mean(cv.auc)
cv.auc.low[7] = quantile(cv.auc, probs = 2.5/100, na.rm = T)
cv.auc.high[7] = quantile(cv.auc, probs = 97.5/100, na.rm = T)



# DIM 10
for (i in 1:num.sim) {
  
  m = cross.validate(input.data = df10, num.groups = 2)
  cv.sens[i] = as.numeric(m$byClass[1])
  cv.spec[i] = as.numeric(m$byClass[2])
  cv.ppv[i] = as.numeric(m$byClass[3])
  cv.npv[i] = as.numeric(m$byClass[4])
  cv.auc[i] = m$auc
  
}
cv.avg.sens[8] = mean(cv.sens)
cv.sens.low[8] = quantile(cv.sens, probs = 2.5/100, na.rm = T)
cv.sens.high[8] = quantile(cv.sens, probs = 97.5/100, na.rm = T)

cv.avg.spec[8] = mean(cv.spec)
cv.spec.low[8] = quantile(cv.spec, probs = 2.5/100, na.rm = T)
cv.spec.high[8] = quantile(cv.spec, probs = 97.5/100, na.rm = T)

cv.avg.ppv[8] = mean(cv.ppv)
cv.ppv.low[8] = quantile(cv.ppv, probs = 2.5/100, na.rm = T)
cv.ppv.high[8] = quantile(cv.ppv, probs = 97.5/100, na.rm = T)

cv.avg.npv[8] = mean(cv.npv)
cv.npv.low[8] = quantile(cv.npv, probs = 2.5/100, na.rm = T)
cv.npv.high[8] = quantile(cv.npv, probs = 97.5/100, na.rm = T)

cv.avg.auc[8] = mean(cv.auc)
cv.auc.low[8] = quantile(cv.auc, probs = 2.5/100, na.rm = T)
cv.auc.high[8] = quantile(cv.auc, probs = 97.5/100, na.rm = T)









