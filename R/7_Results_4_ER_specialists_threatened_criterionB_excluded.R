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
setwd("My_working_directory")

# Import data
ext.risk<-read.delim("species_data.txt")

# Filter data for specialists
ext.risk.mammals<-ext.risk %>% filter(habitat_breadth==1)

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
                                  rl.cat=threatened.under.criterionB.excluded.gain.excluded.na.omit$rl_cat)

# Scale the numerical variables
num_vars <- sapply(ext.risk.mammals.data, is.numeric)
ext.risk.mammals.data.scaled <- ext.risk.mammals.data
ext.risk.mammals.data.scaled[, num_vars] <- scale(ext.risk.mammals.data[, num_vars])

#-------------------------
# Set ER models 
pm.glmer<-glmer(rl.cat~pm+rpm+rs+gl+wa+(1|or), family=binomial, data=ext.risk.mammals.data.scaled)
cm.glmer<-glmer(rl.cat~cm+rcm+rs+gl+wa+(1|or), family=binomial, data=ext.risk.mammals.data.scaled)
hm.glmer<-glmer(rl.cat~hm+rhm+rs+gl+wa+(1|or), family=binomial, data=ext.risk.mammals.data.scaled)

# Extract coefficients for pm, rpm, cm, rcm, hm, and rhm while holding other predictors constant at their mean
coef.pm<-fixef(pm.glmer)["pm"]
coef.rpm<-fixef(pm.glmer)["rpm"]
coef.cm<-fixef(cm.glmer)["cm"]
coef.rcm<-fixef(cm.glmer)["rcm"]
coef.hm<-fixef(hm.glmer)["hm"]
coef.rhm<-fixef(hm.glmer)["rhm"]

# Extract the confidence interval for the estimated coefficient of the predictor of interest
coef.pm.ci<-confint(pm.glmer, method="Wald")["pm",]
coef.rpm.ci<-confint(pm.glmer, method="Wald")["rpm",]
coef.cm.ci<-confint(cm.glmer, method="Wald")["cm",]
coef.rcm.ci<-confint(cm.glmer, method="Wald")["rcm",]
coef.hm.ci<-confint(hm.glmer, method="Wald")["hm",]
coef.rhm.ci<-confint(hm.glmer, method="Wald")["rhm",]

# Create data frame with extracted coefficients
pm.effectsize.table <- data.frame(parameter = "pm", std_coefficient=coef.pm[[1]], ci_lower=coef.pm.ci[[1]], ci_upper=coef.pm.ci[[2]])
rpm.effectsize.table <- data.frame(parameter = "rpm", std_coefficient=coef.rpm[[1]], ci_lower=coef.rpm.ci[[1]], ci_upper=coef.rpm.ci[[2]])
cm.effectsize.table <- data.frame(parameter = "cm", std_coefficient=coef.cm[[1]], ci_lower=coef.cm.ci[[1]], ci_upper=coef.cm.ci[[2]])
rcm.effectsize.table <- data.frame(parameter = "rcm", std_coefficient=coef.rcm[[1]], ci_lower=coef.rcm.ci[[1]], ci_upper=coef.rcm.ci[[2]])
hm.effectsize.table <- data.frame(parameter = "hm", std_coefficient=coef.hm[[1]], ci_lower=coef.hm.ci[[1]], ci_upper=coef.hm.ci[[2]])
rhm.effectsize.table <- data.frame(parameter = "rhm", std_coefficient=coef.rhm[[1]], ci_lower=coef.rhm.ci[[1]], ci_upper=coef.rhm.ci[[2]])

# Combine data frames together
pmcmhm.effectsize.table<-rbind(pm.effectsize.table, cm.effectsize.table, hm.effectsize.table)
rpmrcmrhm.effectsize.table<-rbind(rpm.effectsize.table, rcm.effectsize.table, rhm.effectsize.table)

# Assign habitat model and habitat breadth
habitat_models<-c("Patch-matrix", "Continuum", "Hybrid")
pmcmhm.effectsize.table$habitat_model<-habitat_models
pmcmhm.effectsize.table$habitat_breadth <- "Specialists"
rpmrcmrhm.effectsize.table$habitat_model<-habitat_models
rpmrcmrhm.effectsize.table$habitat_breadth <- "Specialists"

# Save effect sizes
write.table(pmcmhm.effectsize.table, file="effectsize_pmcmhm_specialists_threatened_criterionB_excluded.csv", sep="\t", row.names=F)
write.table(rpmrcmrhm.effectsize.table, file="effectsize_rpmrcmrhm_specialists_threatened_criterionB_excluded.csv", sep="\t", row.names=F)

