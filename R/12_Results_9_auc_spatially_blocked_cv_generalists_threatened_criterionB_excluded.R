# Install required packages
if(!requireNamespace("lme4", quietly=TRUE))
  install.packages("lme4", quiet=TRUE, dependencies=TRUE)
if(!requireNamespace("dplyr", quietly=TRUE))
  install.packages("dplyr", quiet=TRUE, dependencies=TRUE)
if(!requireNamespace("pROC", quietly=TRUE))
  install.packages("pROC", quiet=TRUE, dependencies=TRUE)
if(!requireNamespace("ggplot2", quietly=TRUE))
  install.packages("ggplot2", quiet=TRUE, dependencies=TRUE)

# Load required packages
library(lme4)
library(dplyr)
library(pROC)
library(ggplot2)

# Set working directory
setwd("My_working_directory")

# Import data
ext.risk<-read.delim("species_data.txt")

# Filter data for generalists
ext.risk.mammals<-ext.risk %>% filter(habitat_breadth>1)

# Recode red list categories as numeric
ext.risk.mammals[,32] <- ifelse(ext.risk.mammals[,32] == "LC", 0,
                                ifelse(ext.risk.mammals[,32] == "NT", 0,
                                       ifelse(ext.risk.mammals[,32] == "VU", 1,
                                              ifelse(ext.risk.mammals[,32] == "EN", 1,
                                                     ifelse(ext.risk.mammals[,32] == "CR", 1, NA)))))


# Exclude threatened species under criterionB
ext.risk.mammals$threatened_under_criterionB <- ifelse(ext.risk.mammals[,31] == 1 & ext.risk.mammals[,32] == 1, 1, 0)
threatened.under.criterionB.excluded<-ext.risk.mammals %>% filter(threatened_under_criterionB<1)

# Convert categorical variables to factor
threatened.under.criterionB.excluded$order<-as.factor(threatened.under.criterionB.excluded$order)
threatened.under.criterionB.excluded$rl_cat<-as.factor(threatened.under.criterionB.excluded$rl_cat)

# Select variables from dataset
vars<-c("order",
        "patch_matrix",
        "change_patch_matrix", 
        "continuum_p5",
        "continuum_p10",
        "continuum_p50",
        "continuum_p90", 
        "continuum_p95",
        "change_continuum_p5",
        "change_continuum_p10",
        "change_continuum_p50", 
        "change_continuum_p90",
        "change_continuum_p95",
        "hybrid_p5", 
        "hybrid_p10",
        "hybrid_p50",
        "hybrid_p90", 
        "hybrid_p95",
        "change_hybrid_p5", 
        "change_hybrid_p10",
        "change_hybrid_p50",
        "change_hybrid_p90", 
        "change_hybrid_p95",
        "range_size",
        "gestation_length",
        "weaning_age",
        "realm",
        "rl_cat")

# Replace values greater than 0 with 0 in columns of change
threatened.under.criterionB.excluded.gain.excluded <- threatened.under.criterionB.excluded %>%
  mutate_at(vars(change_patch_matrix,
                 change_continuum_p5,
                 change_continuum_p10,
                 change_continuum_p50,
                 change_continuum_p90,
                 change_continuum_p95,
                 change_hybrid_p5,
                 change_hybrid_p10,
                 change_hybrid_p50,
                 change_hybrid_p90,
                 change_hybrid_p95), ~replace(., . > 0, 0)) %>%
  # Replace negative values with absolute values
  mutate_at(vars(change_patch_matrix,
                 change_continuum_p5,
                 change_continuum_p10,
                 change_continuum_p50,
                 change_continuum_p90,
                 change_continuum_p95,
                 change_hybrid_p5,
                 change_hybrid_p10,
                 change_hybrid_p50,
                 change_hybrid_p90,
                 change_hybrid_p95),
            abs)

# Remove species with missing (NA) values
threatened.under.criterionB.excluded.gain.excluded.na.omit<-na.omit(threatened.under.criterionB.excluded.gain.excluded[,vars])

# Create data frame for extinction risk (ER) models
ext.risk.mammals.data<-data.frame(or=threatened.under.criterionB.excluded.gain.excluded.na.omit$order,
                                  pm=threatened.under.criterionB.excluded.gain.excluded.na.omit$patch_matrix,
                                  rpm=threatened.under.criterionB.excluded.gain.excluded.na.omit$change_patch_matrix,
                                  cm=threatened.under.criterionB.excluded.gain.excluded.na.omit$continuum_p95,
                                  rcm=threatened.under.criterionB.excluded.gain.excluded.na.omit$change_continuum_p95,
                                  hm=threatened.under.criterionB.excluded.gain.excluded.na.omit$hybrid_p95,
                                  rhm=threatened.under.criterionB.excluded.gain.excluded.na.omit$change_hybrid_p95,
                                  rs=threatened.under.criterionB.excluded.gain.excluded.na.omit$range_size,
                                  gl=threatened.under.criterionB.excluded.gain.excluded.na.omit$gestation_length,
                                  wa=threatened.under.criterionB.excluded.gain.excluded.na.omit$weaning_age,
                                  rlm=threatened.under.criterionB.excluded.gain.excluded.na.omit$realm,
                                  rl.cat=threatened.under.criterionB.excluded.gain.excluded.na.omit$rl_cat)

# Scale the numerical variables
num_vars <- sapply(ext.risk.mammals.data, is.numeric)
ext.risk.mammals.data.scaled <- ext.risk.mammals.data
ext.risk.mammals.data.scaled[, num_vars] <- scale(ext.risk.mammals.data[, num_vars])

#----
# Define training  and testing datasets
# Using all realms but Afrotropic as training set and Afrotropic as testing set
train.but.afrotropic <- ext.risk.mammals.data.scaled %>% filter(rlm=="Australasia" | rlm=="Indomalayan" | rlm=="Nearctic" | rlm=="Neotropic" | rlm=="Palearctic")
test.afroropic <- ext.risk.mammals.data.scaled %>% filter(rlm=="Afrotropic")

# Using all realms but Australasia as training set and Australasia as testing set
train.but.australasia <- ext.risk.mammals.data.scaled %>% filter(rlm=="Afrotropic" | rlm=="Indomalayan" | rlm=="Nearctic" | rlm=="Neotropic" | rlm=="Palearctic")
test.australasia <- ext.risk.mammals.data.scaled %>% filter(rlm=="Australasia")

# Using all realms but Indomalayan as training set and Indomalayan as testing set
train.but.indomalayan <- ext.risk.mammals.data.scaled %>% filter(rlm=="Afrotropic" | rlm=="Australasia" | rlm=="Nearctic" | rlm=="Neotropic" | rlm=="Palearctic")
test.indomalayan <- ext.risk.mammals.data.scaled %>% filter(rlm=="Indomalayan")

# Using all realms but Nearctic as training set and Nearctic as testing set
train.but.nearctic <- ext.risk.mammals.data.scaled %>% filter(rlm=="Afrotropic" | rlm=="Australasia" | rlm=="Indomalayan" | rlm=="Neotropic" | rlm=="Palearctic")
test.nearctic <- ext.risk.mammals.data.scaled %>% filter(rlm=="Nearctic")

# Using all realms but Neotropic as training set and Neotropic as testing set
train.but.neotropic <- ext.risk.mammals.data.scaled %>% filter(rlm=="Afrotropic" | rlm=="Australasia" | rlm=="Indomalayan" | rlm=="Nearctic" | rlm=="Palearctic")
test.neotropic <- ext.risk.mammals.data.scaled %>% filter(rlm=="Neotropic")

# Using all realms but Palearctic as training set and Palearctic as testing set
train.but.palearctic <- ext.risk.mammals.data.scaled %>% filter(rlm=="Afrotropic" | rlm=="Australasia" | rlm=="Indomalayan" | rlm=="Nearctic" | rlm=="Neotropic")
test.palearctic <- ext.risk.mammals.data.scaled %>% filter(rlm=="Palearctic")

#----
# ER ~ pm
hc.pm.but.afrotropic<-glmer(rl.cat~pm+rpm+rs+gl+wa+(1|or), family=binomial, data=train.but.afrotropic)
hc.pm.but.australasia<-glmer(rl.cat~pm+rpm+rs+gl+wa+(1|or), family=binomial, data=train.but.australasia)
hc.pm.but.indomalayan<-glmer(rl.cat~pm+rpm+rs+gl+wa+(1|or), family=binomial, data=train.but.indomalayan)
hc.pm.but.nearctic<-glmer(rl.cat~pm+rpm+rs+gl+wa+(1|or), family=binomial, data=train.but.nearctic)
hc.pm.but.neotropic<-glmer(rl.cat~pm+rpm+rs+gl+wa+(1|or), family=binomial, data=train.but.neotropic)
hc.pm.but.palearctic<-glmer(rl.cat~pm+rpm+rs+gl+wa+(1|or), family=binomial, data=train.but.palearctic)

# Calculate predicted probability  for each species in test dataset
predicted.pm.but.afrotropic <- predict(hc.pm.but.afrotropic, test.afroropic, type="response", allow.new.levels = TRUE)
predicted.pm.but.australasia <- predict(hc.pm.but.australasia, test.australasia, type="response", allow.new.levels = TRUE)
predicted.pm.but.indomalayan <- predict(hc.pm.but.indomalayan, test.indomalayan, type="response", allow.new.levels = TRUE)
predicted.pm.but.nearctic <- predict(hc.pm.but.nearctic, test.nearctic, type="response", allow.new.levels = TRUE)
predicted.pm.but.neotropic <- predict(hc.pm.but.neotropic, test.neotropic, type="response", allow.new.levels = TRUE)
predicted.pm.but.palearctic <- predict(hc.pm.but.palearctic, test.palearctic, type="response", allow.new.levels = TRUE)

# Calculate AUC values
hc.pm.but.afrotropic.auc<-auc(test.afroropic$rl.cat, predicted.pm.but.afrotropic)
hc.pm.but.australasia.auc<-auc(test.australasia$rl.cat, predicted.pm.but.australasia)
hc.pm.but.indomalayan.auc<-auc(test.indomalayan$rl.cat, predicted.pm.but.indomalayan)
hc.pm.but.nearctic.auc<-auc(test.nearctic$rl.cat, predicted.pm.but.nearctic)
hc.pm.but.neotropic.auc<-auc(test.neotropic$rl.cat, predicted.pm.but.neotropic)
hc.pm.but.palearctic.auc<-auc(test.palearctic$rl.cat, predicted.pm.but.palearctic)

# Convert AUC values into a data frame
hc.pm.auc.values<-as.data.frame(rbind(hc.pm.but.afrotropic.auc[1], hc.pm.but.australasia.auc[1], hc.pm.but.indomalayan.auc[1], hc.pm.but.nearctic.auc[1], hc.pm.but.neotropic.auc[1], hc.pm.but.palearctic.auc[1]))

# Assign new name to the column of the data frame
colnames(hc.pm.auc.values) <- c("pm.auc.values")

# Calculate  mean and standard error
hc.pm.auc.values.model <- lm(pm.auc.values ~ 1, hc.pm.auc.values)
coef.hc.pm.auc.values.model<-data.frame(coef(summary(hc.pm.auc.values.model)))

# Calculate confidence interval
confint.hc.pm.auc.values.model<-as.data.frame(confint(hc.pm.auc.values.model))

# Create data frame with AUC value and its corresponding confidence interval
auc.pm<-data.frame(auc=coef.hc.pm.auc.values.model[,1], ci_lower=confint.hc.pm.auc.values.model$`2.5 %`, ci_upper=confint.hc.pm.auc.values.model$`97.5 %`)

#----
# ER ~ cm
hc.cm.but.afrotropic<-glmer(rl.cat~cm+rcm+rs+gl+wa+(1|or), family=binomial, data=train.but.afrotropic)
hc.cm.but.australasia<-glmer(rl.cat~cm+rcm+rs+gl+wa+(1|or), family=binomial, data=train.but.australasia)
hc.cm.but.indomalayan<-glmer(rl.cat~cm+rcm+rs+gl+wa+(1|or), family=binomial, data=train.but.indomalayan)
hc.cm.but.nearctic<-glmer(rl.cat~cm+rcm+rs+gl+wa+(1|or), family=binomial, data=train.but.nearctic)
hc.cm.but.neotropic<-glmer(rl.cat~cm+rcm+rs+gl+wa+(1|or), family=binomial, data=train.but.neotropic)
hc.cm.but.palearctic<-glmer(rl.cat~cm+rcm+rs+gl+wa+(1|or), family=binomial, data=train.but.palearctic)

# Calculate predicted probability for each species in test dataset
predicted.cm.but.afrotropic <- predict(hc.cm.but.afrotropic, test.afroropic, type="response", allow.new.levels = TRUE)
predicted.cm.but.australasia <- predict(hc.cm.but.australasia, test.australasia, type="response", allow.new.levels = TRUE)
predicted.cm.but.indomalayan <- predict(hc.cm.but.indomalayan, test.indomalayan, type="response", allow.new.levels = TRUE)
predicted.cm.but.nearctic <- predict(hc.cm.but.nearctic, test.nearctic, type="response", allow.new.levels = TRUE)
predicted.cm.but.neotropic <- predict(hc.cm.but.neotropic, test.neotropic, type="response", allow.new.levels = TRUE)
predicted.cm.but.palearctic <- predict(hc.cm.but.palearctic, test.palearctic, type="response", allow.new.levels = TRUE)

# Calculate AUC values
hc.cm.but.afrotropic.auc<-auc(test.afroropic$rl.cat, predicted.cm.but.afrotropic)
hc.cm.but.australasia.auc<-auc(test.australasia$rl.cat, predicted.cm.but.australasia)
hc.cm.but.indomalayan.auc<-auc(test.indomalayan$rl.cat, predicted.cm.but.indomalayan)
hc.cm.but.nearctic.auc<-auc(test.nearctic$rl.cat, predicted.cm.but.nearctic)
hc.cm.but.neotropic.auc<-auc(test.neotropic$rl.cat, predicted.cm.but.neotropic)
hc.cm.but.palearctic.auc<-auc(test.palearctic$rl.cat, predicted.cm.but.palearctic)

# Convert AUC values into a data frame
hc.cm.auc.values<-as.data.frame(rbind(hc.cm.but.afrotropic.auc[1], hc.cm.but.australasia.auc[1], hc.cm.but.indomalayan.auc[1], hc.cm.but.nearctic.auc[1], hc.cm.but.neotropic.auc[1], hc.cm.but.palearctic.auc[1]))

# Assign new name to the column of the data frame
colnames(hc.cm.auc.values) <- c("cm.auc.values")

# Calculate mean and standard error
hc.cm.auc.values.model <- lm(cm.auc.values ~ 1, hc.cm.auc.values)
coef.hc.cm.auc.values.model<-data.frame(coef(summary(hc.cm.auc.values.model)))

# Calculate confidence interval
confint.hc.cm.auc.values.model<-as.data.frame(confint(hc.cm.auc.values.model))

# Create data frame with mean AUC value and confidence interval
auc.cm<-data.frame(auc=coef.hc.cm.auc.values.model[,1], ci_lower=confint.hc.cm.auc.values.model$`2.5 %`, ci_upper=confint.hc.cm.auc.values.model$`97.5 %`)

#----
# ER ~ hm
hc.hm.but.afrotropic<-glmer(rl.cat~hm+rhm+rs+gl+wa+(1|or), family=binomial, data=train.but.afrotropic)
hc.hm.but.australasia<-glmer(rl.cat~hm+rhm+rs+gl+wa+(1|or), family=binomial, data=train.but.australasia)
hc.hm.but.indomalayan<-glmer(rl.cat~hm+rhm+rs+gl+wa+(1|or), family=binomial, data=train.but.indomalayan)
hc.hm.but.nearctic<-glmer(rl.cat~hm+rhm+rs+gl+wa+(1|or), family=binomial, data=train.but.nearctic)
hc.hm.but.neotropic<-glmer(rl.cat~hm+rhm+rs+gl+wa+(1|or), family=binomial, data=train.but.neotropic)
hc.hm.but.palearctic<-glmer(rl.cat~hm+rhm+rs+gl+wa+(1|or), family=binomial, data=train.but.palearctic)

# Calculate predicted probability for each species in test dataset
predicted.hm.but.afrotropic <- predict(hc.hm.but.afrotropic, test.afroropic, type="response", allow.new.levels = TRUE)
predicted.hm.but.australasia <- predict(hc.hm.but.australasia, test.australasia, type="response", allow.new.levels = TRUE)
predicted.hm.but.indomalayan <- predict(hc.hm.but.indomalayan, test.indomalayan, type="response", allow.new.levels = TRUE)
predicted.hm.but.nearctic <- predict(hc.hm.but.nearctic, test.nearctic, type="response", allow.new.levels = TRUE)
predicted.hm.but.neotropic <- predict(hc.hm.but.neotropic, test.neotropic, type="response", allow.new.levels = TRUE)
predicted.hm.but.palearctic <- predict(hc.hm.but.palearctic, test.palearctic, type="response", allow.new.levels = TRUE)

# Calculate AUC values
hc.hm.but.afrotropic.auc<-auc(test.afroropic$rl.cat, predicted.hm.but.afrotropic)
hc.hm.but.australasia.auc<-auc(test.australasia$rl.cat, predicted.hm.but.australasia)
hc.hm.but.indomalayan.auc<-auc(test.indomalayan$rl.cat, predicted.hm.but.indomalayan)
hc.hm.but.nearctic.auc<-auc(test.nearctic$rl.cat, predicted.hm.but.nearctic)
hc.hm.but.neotropic.auc<-auc(test.neotropic$rl.cat, predicted.hm.but.neotropic)
hc.hm.but.palearctic.auc<-auc(test.palearctic$rl.cat, predicted.hm.but.palearctic)

# Convert AUC values into a data frame
hc.hm.auc.values<-as.data.frame(rbind(hc.hm.but.afrotropic.auc[1], hc.hm.but.australasia.auc[1], hc.hm.but.indomalayan.auc[1], hc.hm.but.nearctic.auc[1], hc.hm.but.neotropic.auc[1], hc.hm.but.palearctic.auc[1]))

# Assign new name to the column of the data frame
colnames(hc.hm.auc.values) <- c("hm.auc.values")

# Calculate mean and standard error
hc.hm.auc.values.model <- lm(hm.auc.values ~ 1, hc.hm.auc.values)
coef.hc.hm.auc.values.model<-data.frame(coef(summary(hc.hm.auc.values.model)))

# Calculate confidence interval
confint.hc.hm.auc.values.model<-as.data.frame(confint(hc.hm.auc.values.model))

# Create data frame with mean AUC value confidence interval
auc.hm<-data.frame(auc=coef.hc.hm.auc.values.model[,1], ci_lower=confint.hc.hm.auc.values.model$`2.5 %`, ci_upper=confint.hc.hm.auc.values.model$`97.5 %`)

#----
# Combine data frames with mean AUC values and confidence intervals together
auc.pmcmhm<-rbind(auc.pm, auc.cm, auc.hm)

# Assign habitat model and habitat breadth
auc.pmcmhm$parameter<-c("pm", "cm", "hm")
habitat_models<-c("Patch-matrix", "Continuum", "Hybrid")
auc.pmcmhm$habitat_model<-habitat_models
auc.pmcmhm$habitat_breadth <- "Generalists"

# Save AUC values and confidence intervals
write.table(auc.pmcmhm, file="auc_spatially_blocked_cv_pmcmhm_generalists_threatened_criterionB_excluded.csv", sep="\t", row.names=F)

