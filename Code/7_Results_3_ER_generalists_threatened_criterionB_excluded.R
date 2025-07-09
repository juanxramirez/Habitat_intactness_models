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

# Filter data for generalists
ext.risk.mammals<-ext.risk %>% filter(habitat_breadth>1)

# Recode red list categories as numeric
ext.risk.mammals[,7] <- ifelse(ext.risk.mammals[,7] == "LC", 0,
                                     ifelse(ext.risk.mammals[,7] == "NT", 0,
                                            ifelse(ext.risk.mammals[,7] == "VU", 1,
                                                   ifelse(ext.risk.mammals[,7] == "EN", 1,
                                                          ifelse(ext.risk.mammals[,7] == "CR", 1, NA)))))


# Exclude threatened species under criterionB
ext.risk.mammals$threatened_under_criterionB <- ifelse(ext.risk.mammals[,11] == 1 & ext.risk.mammals[,7] == 1, 1, 0)
threatened.under.criterionB.excluded<-ext.risk.mammals %>% filter(threatened_under_criterionB<1)

# Convert categorical variables to factor
threatened.under.criterionB.excluded$order<-as.factor(threatened.under.criterionB.excluded$order)
threatened.under.criterionB.excluded$family<-as.factor(threatened.under.criterionB.excluded$family)
threatened.under.criterionB.excluded$genus<-as.factor(threatened.under.criterionB.excluded$genus)
threatened.under.criterionB.excluded$rl_cat<-as.factor(threatened.under.criterionB.excluded$rl_cat)

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
threatened.under.criterionB.excluded.na.omit<-na.omit(threatened.under.criterionB.excluded[,vars])

# Create data frame for extinction risk (ER) models
ext.risk.mammals.data<-data.frame(or=threatened.under.criterionB.excluded.na.omit$order,
                                   fam=threatened.under.criterionB.excluded.na.omit$family,
                                   gen=threatened.under.criterionB.excluded.na.omit$genus,
                                   pm=threatened.under.criterionB.excluded.na.omit$patch_matrix,
                                   cm=threatened.under.criterionB.excluded.na.omit$continuum_p95,
                                   hm=threatened.under.criterionB.excluded.na.omit$hybrid_p95,
                                   rs=threatened.under.criterionB.excluded.na.omit$range_size,
                                   gl=threatened.under.criterionB.excluded.na.omit$gestation_length,
                                   wa=threatened.under.criterionB.excluded.na.omit$weaning_age,
                                   rl.cat=threatened.under.criterionB.excluded.na.omit$rl_cat)

# Scale the numerical variables
num_vars <- sapply(ext.risk.mammals.data, is.numeric)
ext.risk.mammals.data.scaled <- ext.risk.mammals.data
ext.risk.mammals.data.scaled[, num_vars] <- scale(ext.risk.mammals.data[, num_vars])

#-------------------------
# Set ER models 
pm.glmer<-glmer(rl.cat~pm+rs+gl+wa+(1|fam/gen), family=binomial, data=ext.risk.mammals.data.scaled, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
cm.glmer<-glmer(rl.cat~cm+rs+gl+wa+(1|fam/gen), family=binomial, data=ext.risk.mammals.data.scaled, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hm.glmer<-glmer(rl.cat~hm+rs+gl+wa+(1|fam/gen), family=binomial, data=ext.risk.mammals.data.scaled, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

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
pmcmhm.effectsize.table$habitat_breadth <- "Generalists"

# Save effect sizes
write.table(pmcmhm.effectsize.table, file="effectsize_pmcmhm_generalists_threatened_criterionB_excluded.csv", sep="\t", row.names=F)

