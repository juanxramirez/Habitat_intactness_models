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

# Scale the numerical variables
num_vars <- sapply(threatened.under.criterionB.excluded.gain.excluded.na.omit, is.numeric)
ext.risk.mammals.data.scaled <- threatened.under.criterionB.excluded.gain.excluded.na.omit
ext.risk.mammals.data.scaled[, num_vars] <- scale(threatened.under.criterionB.excluded.gain.excluded.na.omit[, num_vars])

# Create data frames for extinction risk (ER) models
data.pm<-data.frame(or=ext.risk.mammals.data.scaled$order,
                    pm=ext.risk.mammals.data.scaled$patch_matrix,
                    rpm=ext.risk.mammals.data.scaled$change_patch_matrix,
                    rs=ext.risk.mammals.data.scaled$range_size,
                    gl=ext.risk.mammals.data.scaled$gestation_length,
                    wa=ext.risk.mammals.data.scaled$weaning_age,
                    rl.cat=ext.risk.mammals.data.scaled$rl_cat)

data.cm<-data.frame(or=ext.risk.mammals.data.scaled$or,
                    cm=ext.risk.mammals.data.scaled$continuum_p95,
                    rcm=ext.risk.mammals.data.scaled$change_continuum_p95,
                    rs=ext.risk.mammals.data.scaled$range_size,
                    gl=ext.risk.mammals.data.scaled$gestation_length,
                    wa=ext.risk.mammals.data.scaled$weaning_age,
                    rl.cat=ext.risk.mammals.data.scaled$rl_cat)

data.hm<-data.frame(or=ext.risk.mammals.data.scaled$or,
                    hm=ext.risk.mammals.data.scaled$hybrid_p95,
                    rhm=ext.risk.mammals.data.scaled$change_hybrid_p95,
                    rs=ext.risk.mammals.data.scaled$range_size,
                    gl=ext.risk.mammals.data.scaled$gestation_length,
                    wa=ext.risk.mammals.data.scaled$weaning_age,
                    rl.cat=ext.risk.mammals.data.scaled$rl_cat)

#----
# Set number of folds for cross-validation
k <- 10

# Calculate the number of observations in the dataset
n <- dim(ext.risk.mammals.data.scaled)[1]

# Set seed for reproducibility 
set.seed(17)

# Generate indices for random sampling
indices <- sample(rep(1:k, ceiling(n/k))[1:n])

#----
# Calculate AUC values for pm
all.response.pm <- all.predictor.pm <- aucs.pm <- c()
for (i in 1:k) {
  test.pm = data.pm[indices==i,]
  learn.pm = data.pm[indices!=i,]
  model.pm <- glmer(rl.cat~pm+rpm+rs+gl+wa+(1|or), family=binomial, data=learn.pm)
  model.pred.pm <- predict(model.pm, newdata=test.pm, type="response", allow.new.levels=TRUE)
  aucs.pm <- c(aucs.pm, roc(test.pm$rl.cat, model.pred.pm, quiet=TRUE)$auc)
  all.response.pm <- c(all.response.pm, test.pm$rl.cat)
  all.predictor.pm <- c(all.predictor.pm, model.pred.pm)
}

# Convert AUC values into a data frame
hc.pm.auc.values<-as.data.frame(aucs.pm)

# Calculate mean and standard error
hc.pm.auc.values.model <- lm(aucs.pm ~ 1, hc.pm.auc.values)
coef.hc.pm.auc.values.model<-data.frame(coef(summary(hc.pm.auc.values.model)))

# Calculate confidence interval
confint.hc.pm.auc.values.model<-as.data.frame(confint(hc.pm.auc.values.model))

# Create data frame with mean AUC value and confidence interval
auc.pm<-data.frame(auc=coef.hc.pm.auc.values.model[,1], ci_lower=confint.hc.pm.auc.values.model$`2.5 %`, ci_upper=confint.hc.pm.auc.values.model$`97.5 %`)

#----
# Calculate AUC values for cm
all.response.cm <- all.predictor.cm <- aucs.cm <- c()
for (i in 1:k) {
  test.cm = data.cm[indices==i,]
  learn.cm = data.cm[indices!=i,]
  model.cm <- glmer(rl.cat~cm+rcm+rs+gl+wa+(1|or), family=binomial, data=learn.cm)
  model.pred.cm <- predict(model.cm, newdata=test.cm, type="response", allow.new.levels=TRUE)
  aucs.cm <- c(aucs.cm, roc(test.cm$rl.cat, model.pred.cm, quiet=TRUE)$auc)
  all.response.cm <- c(all.response.cm, test.cm$rl.cat)
  all.predictor.cm <- c(all.predictor.cm, model.pred.cm)
}

# Convert AUC values into a data frame
hc.cm.auc.values<-as.data.frame(aucs.cm)

# Calculate mean and standard error
hc.cm.auc.values.model <- lm(aucs.cm~1, hc.cm.auc.values)
coef.hc.cm.auc.values.model<-data.frame(coef(summary(hc.cm.auc.values.model)))

# Calculate confidence interval
confint.hc.cm.auc.values.model<-as.data.frame(confint(hc.cm.auc.values.model))

# Create data frame with mean AUC value and confidence interval
auc.cm<-data.frame(auc=coef.hc.cm.auc.values.model[,1], ci_lower=confint.hc.cm.auc.values.model$`2.5 %`, ci_upper=confint.hc.cm.auc.values.model$`97.5 %`)

#----
# Calculate AUC values for hm
all.response.hm <- all.predictor.hm <- aucs.hm <- c()
for (i in 1:k) {
  test.hm = data.hm[indices==i,]
  learn.hm = data.hm[indices!=i,]
  model.hm <- glmer(rl.cat~hm+rhm+rs+gl+wa+(1|or), family=binomial, data=learn.hm)
  model.pred.hm <- predict(model.hm, newdata=test.hm, type="response", allow.new.levels=TRUE)
  aucs.hm <- c(aucs.hm, roc(test.hm$rl.cat, model.pred.hm, quiet=TRUE)$auc)
  all.response.hm <- c(all.response.hm, test.hm$rl.cat)
  all.predictor.hm <- c(all.predictor.hm, model.pred.hm)
}

# Convert AUC values into a data frame
hc.hm.auc.values<-as.data.frame(aucs.hm)

# Calculate mean and standard error
hc.hm.auc.values.model <- lm(aucs.hm~1, hc.hm.auc.values)
coef.hc.hm.auc.values.model<-data.frame(coef(summary(hc.hm.auc.values.model)))

# Calculate confidence interval
confint.hc.hm.auc.values.model<-as.data.frame(confint(hc.hm.auc.values.model))

# Create data frame with mean AUC value and confidence interval
auc.hm<-data.frame(auc=coef.hc.hm.auc.values.model[,1], ci_lower=confint.hc.hm.auc.values.model$`2.5 %`, ci_upper=confint.hc.hm.auc.values.model$`97.5 %`)

#----
# Combine data frames with mean AUC values and confidence intervals together
auc.pmcmhm<-rbind(auc.pm, auc.cm, auc.hm)

# Assign habitat model and habitat breadth
auc.pmcmhm$parameter<-c("pm", "cm", "hm")
habitat_models<-c("Patch-matrix", "Continuum", "Hybrid")
auc.pmcmhm$habitat_model<-habitat_models
auc.pmcmhm$habitat_breadth <- "Specialists"

# Save AUC values and confidence intervals
write.table(auc.pmcmhm, file="auc_10-fold_cv_pmcmhm_specialists_threatened_criterionB_excluded.csv", sep="\t", row.names=F)
