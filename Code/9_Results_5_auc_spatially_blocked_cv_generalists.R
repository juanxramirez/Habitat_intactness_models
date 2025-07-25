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
setwd("C:/Data")

# Import data
ext.risk<-read.delim("species_data.txt")

# Filter data for generalists
ext.risk.mammals<-ext.risk %>% filter(habitat_breadth>1)

# Recode red list categories as numeric
ext.risk.mammals[,7] <- ifelse(ext.risk.mammals[,7] == "LC", 0,
                                ifelse(ext.risk.mammals[,7] == "NT", 0,
                                       ifelse(ext.risk.mammals[,7] == "VU", 1,
                                              ifelse(ext.risk.mammals[,7] == "EN", 1,
                                                     ifelse(ext.risk.mammals[,7] == "CR", 1, NA)))))

# Convert categorical variables to factor
ext.risk.mammals$order<-as.factor(ext.risk.mammals$order)
ext.risk.mammals$family<-as.factor(ext.risk.mammals$family)
ext.risk.mammals$genus<-as.factor(ext.risk.mammals$genus)
ext.risk.mammals$realm<-as.factor(ext.risk.mammals$realm)
ext.risk.mammals$rl_cat<-as.factor(ext.risk.mammals$rl_cat)

# Select variables from dataset
vars<-c("order",
        "family",
        "genus",
        "patch_matrix",
        "continuum_p5",
        "continuum_p10",
        "continuum_p50",
        "continuum_p90", 
        "continuum_p95",
        "hybrid_p5", 
        "hybrid_p10",
        "hybrid_p50",
        "hybrid_p90", 
        "hybrid_p95",
        "range_size",
        "gestation_length",
        "weaning_age",
        "realm",
        "rl_cat")

# Remove species with missing (NA) values
ext.risk.mammals.na.omit<-na.omit(ext.risk.mammals[,vars])

# Create data frame for extinction risk (ER) models
ext.risk.mammals.data<-data.frame(or=ext.risk.mammals.na.omit$order,
                                  fam=ext.risk.mammals.na.omit$family,
                                  gen=ext.risk.mammals.na.omit$genus,
                                  pm=ext.risk.mammals.na.omit$patch_matrix,
                                  cm=ext.risk.mammals.na.omit$continuum_p95,
                                  hm=ext.risk.mammals.na.omit$hybrid_p95,
                                  rs=ext.risk.mammals.na.omit$range_size,
                                  gl=ext.risk.mammals.na.omit$gestation_length,
                                  wa=ext.risk.mammals.na.omit$weaning_age,
                                  rlm=ext.risk.mammals.na.omit$realm,
                                  rl.cat=ext.risk.mammals.na.omit$rl_cat)

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
hc.pm.but.afrotropic<-glmer(rl.cat~pm+rs+gl+wa+(1|fam/gen), family=binomial, data=train.but.afrotropic, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hc.pm.but.australasia<-glmer(rl.cat~pm+rs+gl+wa+(1|fam/gen), family=binomial, data=train.but.australasia, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hc.pm.but.indomalayan<-glmer(rl.cat~pm+rs+gl+wa+(1|fam/gen), family=binomial, data=train.but.indomalayan, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hc.pm.but.nearctic<-glmer(rl.cat~pm+rs+gl+wa+(1|fam/gen), family=binomial, data=train.but.nearctic, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hc.pm.but.neotropic<-glmer(rl.cat~pm+rs+gl+wa+(1|fam/gen), family=binomial, data=train.but.neotropic, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hc.pm.but.palearctic<-glmer(rl.cat~pm+rs+gl+wa+(1|fam/gen), family=binomial, data=train.but.palearctic, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

# Calculate predicted probability  for each species in test dataset
predicted.pm.but.afrotropic <- predict(hc.pm.but.afrotropic, test.afroropic, type="response", allow.new.levels = TRUE)
predicted.pm.but.australasia <- predict(hc.pm.but.australasia, test.australasia, type="response", allow.new.levels = TRUE)
predicted.pm.but.indomalayan <- predict(hc.pm.but.indomalayan, test.indomalayan, type="response", allow.new.levels = TRUE)
predicted.pm.but.nearctic <- predict(hc.pm.but.nearctic, test.nearctic, type="response", allow.new.levels = TRUE)
predicted.pm.but.neotropic <- predict(hc.pm.but.neotropic, test.neotropic, type="response", allow.new.levels = TRUE)
predicted.pm.but.palearctic <- predict(hc.pm.but.palearctic, test.palearctic, type="response", allow.new.levels = TRUE)

# Calculate AUC values
hc.pm.but.afrotropic.auc<-auc(test.afroropic$rl.cat, predicted.pm.but.afrotropic,  quiet=TRUE)
hc.pm.but.australasia.auc<-auc(test.australasia$rl.cat, predicted.pm.but.australasia,  quiet=TRUE)
hc.pm.but.indomalayan.auc<-auc(test.indomalayan$rl.cat, predicted.pm.but.indomalayan,  quiet=TRUE)
hc.pm.but.nearctic.auc<-auc(test.nearctic$rl.cat, predicted.pm.but.nearctic, quiet=TRUE)
hc.pm.but.neotropic.auc<-auc(test.neotropic$rl.cat, predicted.pm.but.neotropic,  quiet=TRUE)
hc.pm.but.palearctic.auc<-auc(test.palearctic$rl.cat, predicted.pm.but.palearctic,  quiet=TRUE)

# Convert AUC values into a data frame
hc.pm.auc.values<-as.data.frame(rbind(hc.pm.but.afrotropic.auc[1], hc.pm.but.australasia.auc[1], hc.pm.but.indomalayan.auc[1], hc.pm.but.nearctic.auc[1], hc.pm.but.neotropic.auc[1], hc.pm.but.palearctic.auc[1]))

# Assign new name to the column of the data frame
colnames(hc.pm.auc.values) <- c("pm.auc.values")

# Calculate mean and standard error
hc.pm.auc.values.model <- lm(pm.auc.values ~ 1, hc.pm.auc.values)
coef.hc.pm.auc.values.model<-data.frame(coef(summary(hc.pm.auc.values.model)))

# Calculate confidence interval
confint.hc.pm.auc.values.model<-as.data.frame(confint(hc.pm.auc.values.model))

# Create data frame with mean AUC value and confidence interval
auc.pm<-data.frame(auc=coef.hc.pm.auc.values.model[,1], ci_lower=confint.hc.pm.auc.values.model$`2.5 %`, ci_upper=confint.hc.pm.auc.values.model$`97.5 %`)

#----
# ER ~ cm
hc.cm.but.afrotropic<-glmer(rl.cat~cm+rs+gl+wa+(1|fam/gen), family=binomial, data=train.but.afrotropic, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hc.cm.but.australasia<-glmer(rl.cat~cm+rs+gl+wa+(1|fam/gen), family=binomial, data=train.but.australasia, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hc.cm.but.indomalayan<-glmer(rl.cat~cm+rs+gl+wa+(1|fam/gen), family=binomial, data=train.but.indomalayan, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hc.cm.but.nearctic<-glmer(rl.cat~cm+rs+gl+wa+(1|fam/gen), family=binomial, data=train.but.nearctic, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hc.cm.but.neotropic<-glmer(rl.cat~cm+rs+gl+wa+(1|fam/gen), family=binomial, data=train.but.neotropic, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hc.cm.but.palearctic<-glmer(rl.cat~cm+rs+gl+wa+(1|fam/gen), family=binomial, data=train.but.palearctic, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

# Calculate predicted probability for each species in test dataset
predicted.cm.but.afrotropic <- predict(hc.cm.but.afrotropic, test.afroropic, type="response", allow.new.levels = TRUE)
predicted.cm.but.australasia <- predict(hc.cm.but.australasia, test.australasia, type="response", allow.new.levels = TRUE)
predicted.cm.but.indomalayan <- predict(hc.cm.but.indomalayan, test.indomalayan, type="response", allow.new.levels = TRUE)
predicted.cm.but.nearctic <- predict(hc.cm.but.nearctic, test.nearctic, type="response", allow.new.levels = TRUE)
predicted.cm.but.neotropic <- predict(hc.cm.but.neotropic, test.neotropic, type="response", allow.new.levels = TRUE)
predicted.cm.but.palearctic <- predict(hc.cm.but.palearctic, test.palearctic, type="response", allow.new.levels = TRUE)

# Calculate AUC values
hc.cm.but.afrotropic.auc<-auc(test.afroropic$rl.cat, predicted.cm.but.afrotropic, quiet=TRUE)
hc.cm.but.australasia.auc<-auc(test.australasia$rl.cat, predicted.cm.but.australasia, quiet=TRUE)
hc.cm.but.indomalayan.auc<-auc(test.indomalayan$rl.cat, predicted.cm.but.indomalayan, quiet=TRUE)
hc.cm.but.nearctic.auc<-auc(test.nearctic$rl.cat, predicted.cm.but.nearctic, quiet=TRUE)
hc.cm.but.neotropic.auc<-auc(test.neotropic$rl.cat, predicted.cm.but.neotropic, quiet=TRUE)
hc.cm.but.palearctic.auc<-auc(test.palearctic$rl.cat, predicted.cm.but.palearctic, quiet=TRUE)

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
hc.hm.but.afrotropic<-glmer(rl.cat~hm+rs+gl+wa+(1|fam/gen), family=binomial, data=train.but.afrotropic, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hc.hm.but.australasia<-glmer(rl.cat~hm+rs+gl+wa+(1|fam/gen), family=binomial, data=train.but.australasia, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hc.hm.but.indomalayan<-glmer(rl.cat~hm+rs+gl+wa+(1|fam/gen), family=binomial, data=train.but.indomalayan, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hc.hm.but.nearctic<-glmer(rl.cat~hm+rs+gl+wa+(1|fam/gen), family=binomial, data=train.but.nearctic, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hc.hm.but.neotropic<-glmer(rl.cat~hm+rs+gl+wa+(1|fam/gen), family=binomial, data=train.but.neotropic, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hc.hm.but.palearctic<-glmer(rl.cat~hm+rs+gl+wa+(1|fam/gen), family=binomial, data=train.but.palearctic, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

# Calculate predicted probability for each species in test dataset
predicted.hm.but.afrotropic <- predict(hc.hm.but.afrotropic, test.afroropic, type="response", allow.new.levels = TRUE)
predicted.hm.but.australasia <- predict(hc.hm.but.australasia, test.australasia, type="response", allow.new.levels = TRUE)
predicted.hm.but.indomalayan <- predict(hc.hm.but.indomalayan, test.indomalayan, type="response", allow.new.levels = TRUE)
predicted.hm.but.nearctic <- predict(hc.hm.but.nearctic, test.nearctic, type="response", allow.new.levels = TRUE)
predicted.hm.but.neotropic <- predict(hc.hm.but.neotropic, test.neotropic, type="response", allow.new.levels = TRUE)
predicted.hm.but.palearctic <- predict(hc.hm.but.palearctic, test.palearctic, type="response", allow.new.levels = TRUE)

# Calculate AUC values
hc.hm.but.afrotropic.auc<-auc(test.afroropic$rl.cat, predicted.hm.but.afrotropic, quiet=TRUE)
hc.hm.but.australasia.auc<-auc(test.australasia$rl.cat, predicted.hm.but.australasia, quiet=TRUE)
hc.hm.but.indomalayan.auc<-auc(test.indomalayan$rl.cat, predicted.hm.but.indomalayan, quiet=TRUE)
hc.hm.but.nearctic.auc<-auc(test.nearctic$rl.cat, predicted.hm.but.nearctic, quiet=TRUE)
hc.hm.but.neotropic.auc<-auc(test.neotropic$rl.cat, predicted.hm.but.neotropic, quiet=TRUE)
hc.hm.but.palearctic.auc<-auc(test.palearctic$rl.cat, predicted.hm.but.palearctic, quiet=TRUE)

# Convert AUC values into a data frame
hc.hm.auc.values<-as.data.frame(rbind(hc.hm.but.afrotropic.auc[1], hc.hm.but.australasia.auc[1], hc.hm.but.indomalayan.auc[1], hc.hm.but.nearctic.auc[1], hc.hm.but.neotropic.auc[1], hc.hm.but.palearctic.auc[1]))

# Assign new name to the column of the data frame
colnames(hc.hm.auc.values) <- c("hm.auc.values")

# Calculate mean and standard error
hc.hm.auc.values.model <- lm(hm.auc.values ~ 1, hc.hm.auc.values)
coef.hc.hm.auc.values.model<-data.frame(coef(summary(hc.hm.auc.values.model)))

# Calculate confidence interval
confint.hc.hm.auc.values.model<-as.data.frame(confint(hc.hm.auc.values.model))

# Create data frame with mean AUC value and corresponding confidence interval
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
write.table(auc.pmcmhm, file="auc_spatially_blocked_cv_pmcmhm_generalists.csv", sep="\t", row.names=F)

