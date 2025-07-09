# Install required packages
if(!requireNamespace("lme4", quietly=TRUE))
  install.packages("lme4", quiet=TRUE, dependencies=TRUE)
if(!requireNamespace("dplyr", quietly=TRUE))
  install.packages("dplyr", quiet=TRUE, dependencies=TRUE)
if(!requireNamespace("ggeffects", quietly=TRUE))
  install.packages("ggeffects", quiet=TRUE, dependencies=TRUE)
if(!requireNamespace("ggplot2", quietly=TRUE))
  install.packages("ggplot2", quiet=TRUE, dependencies=TRUE)

# Load required packages
library(lme4)
library(dplyr)
library(ggeffects)
library(ggplot2)

# Set working directory
setwd("C:/Data")

# Import data
ext.risk<-read.delim("species_data.txt")

# Filter data for specialists
ext.risk.mammals<-ext.risk %>% filter(habitat_breadth==1)

# Recode red list categories as numeric
ext.risk.mammals[,7] <- ifelse(ext.risk.mammals[,7] == "LC", 0,
                                     ifelse(ext.risk.mammals[,7] == "NT", 0,
                                            ifelse(ext.risk.mammals[,7] == "VU", 1,
                                                   ifelse(ext.risk.mammals[,7] == "EN", 1,
                                                          ifelse(ext.risk.mammals[,7] == "CR", 1, NA)))))

# Number of threatened/non-threatened specialists
threatened.nonthreatened<-c("speciesName", "order", "family", "genus", "habitat_breadth", "criterionB", "rl_cat")
ext.risk.mammals.thretened.nonthreatened<-na.omit(ext.risk.mammals[,threatened.nonthreatened])

# Number of threatened species 
ext.risk.mammals.threatened<-ext.risk.mammals.thretened.nonthreatened %>% filter(rl_cat==1)
summary(ext.risk.mammals.threatened)

# Number of threatened species under criterion B
ext.risk.mammals.threatened.under.criterionB<-ext.risk.mammals.threatened %>% filter(criterionB==1)
summary (ext.risk.mammals.threatened.under.criterionB)

# Convert categorical variables to factor
ext.risk.mammals$order<-as.factor(ext.risk.mammals$order)
ext.risk.mammals$family<-as.factor(ext.risk.mammals$family)
ext.risk.mammals$genus<-as.factor(ext.risk.mammals$genus)
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
        "rl_cat")

# Remove species with missing (NA) values
ext.risk.mammals.na.omit<-na.omit(ext.risk.mammals[,vars])

# Create data frame for extinction risk (ER) models
ext.risk.mammals.data<-data.frame(or=ext.risk.mammals.na.omit$order,
                                  fam=ext.risk.mammals.na.omit$family,
                                  gen=ext.risk.mammals.na.omit$genus,
                                  pm=ext.risk.mammals.na.omit$patch_matrix,
                                  cm=ext.risk.mammals.na.omit$continuum_p50,
                                  hm=ext.risk.mammals.na.omit$hybrid_p95,
                                  rs=ext.risk.mammals.na.omit$range_size,
                                  gl=ext.risk.mammals.na.omit$gestation_length,
                                  wa=ext.risk.mammals.na.omit$weaning_age,
                                  rl.cat=ext.risk.mammals.na.omit$rl_cat)

# Scale the numerical variables
num_vars <- sapply(ext.risk.mammals.data, is.numeric)
ext.risk.mammals.data.scaled <- ext.risk.mammals.data
ext.risk.mammals.data.scaled[, num_vars] <- scale(ext.risk.mammals.data[, num_vars])

#----
# Set ER models
pm.glmer<-glmer(rl.cat~pm+rs+gl+wa+(1|or), family=binomial, data=ext.risk.mammals.data.scaled, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
cm.glmer<-glmer(rl.cat~cm+rs+gl+wa+(1|or), family=binomial, data=ext.risk.mammals.data.scaled, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hm.glmer<-glmer(rl.cat~hm+rs+gl+wa+(1|or), family=binomial, data=ext.risk.mammals.data.scaled, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

# Extract coefficients for pm, cm, and hm while holding other predictors constant at their mean
coef.pm<-fixef(pm.glmer)["pm"]
coef.cm<-fixef(cm.glmer)["cm"]
coef.hm<-fixef(hm.glmer)["hm"]

# Extract the confidence interval for the estimated coefficient of the predictor of interest
coef.pm.ci<-confint(pm.glmer, method="Wald")["pm",]
coef.cm.ci<-confint(cm.glmer, method="Wald")["cm",]
coef.hm.ci<-confint(hm.glmer, method="Wald")["hm",]

# Create data frame with extracted coefficients
pm.effectsize.table <- data.frame(parameter = "pm", std_coefficient=coef.pm[[1]], ci_lower=coef.pm.ci[[1]], ci_upper=coef.pm.ci[[2]])
cm.effectsize.table <- data.frame(parameter = "cm", std_coefficient=coef.cm[[1]], ci_lower=coef.cm.ci[[1]], ci_upper=coef.cm.ci[[2]])
hm.effectsize.table <- data.frame(parameter = "hm", std_coefficient=coef.hm[[1]], ci_lower=coef.hm.ci[[1]], ci_upper=coef.hm.ci[[2]])

# Combine data frames together
pmcmhm.effectsize.table<-rbind(pm.effectsize.table, cm.effectsize.table, hm.effectsize.table)

# Assign habitat model and habitat breadth
habitat_models<-c("Patch-matrix", "Continuum", "Hybrid")
pmcmhm.effectsize.table$habitat_model<-habitat_models
pmcmhm.effectsize.table$habitat_breadth <- "Specialists"

# Save effect sizes
write.table(pmcmhm.effectsize.table, file="effectsize_pmcmhm_specialists.csv", sep="\t", row.names=F)

#----
# Create data frame with predicted probabilities for pm
effect.pm<-data.frame(ggpredict(pm.glmer, terms = "pm [all]"))
effect.pm$predictor <- "pm"
effect.pm$habitat_model<-"Patch-matrix"
effect.pm$x_bs <- effect.pm$x * sd(ext.risk.mammals.data$pm) + mean(ext.risk.mammals.data$pm) # back-scale predictor variable
effect.pm$predicted_bt <- plogis(effect.pm$predicted) # back-transform predicted probabilities
effect.pm$conf.low_bt <- plogis(effect.pm$conf.low)
effect.pm$conf.high_bt <- plogis(effect.pm$conf.high)

# Create data frame with predicted probabilities for cm
effect.cm<-data.frame(ggpredict(cm.glmer, terms = "cm [all]"))
effect.cm$predictor <- "cm"
effect.cm$habitat_model<-"Continuum"
effect.cm$x_bs <- effect.cm$x * sd(ext.risk.mammals.data$cm) + mean(ext.risk.mammals.data$cm) # back-scale predictor variable
effect.cm$predicted_bt <- plogis(effect.cm$predicted) # back-transform predicted probabilities
effect.cm$conf.low_bt <- plogis(effect.cm$conf.low)
effect.cm$conf.high_bt <- plogis(effect.cm$conf.high)

# Create data frame with predicted probabilities for hm
effect.hm<-data.frame(ggpredict(hm.glmer, terms = "hm [all]"))
effect.hm$predictor <- "hm"
effect.hm$habitat_model<-"Hybrid"
effect.hm$x_bs <- effect.hm$x * sd(ext.risk.mammals.data$hm) + mean(ext.risk.mammals.data$hm) # back-scale predictor variable
effect.hm$predicted_bt <- plogis(effect.hm$predicted) # back-transform predicted probabilities
effect.hm$conf.low_bt <- plogis(effect.hm$conf.low)
effect.hm$conf.high_bt <- plogis(effect.hm$conf.high)

# Combine data frames together
effect.pmcmhm<-rbind(effect.pm, effect.cm, effect.hm)

# Assign habitat breadth
effect.pmcmhm$habitat_breadth <- "Specialists"

# Save predicted probabilities
write.table(effect.pmcmhm, file="predicted_probability_pmcmhm_specialists.csv", sep="\t", row.names=F)

