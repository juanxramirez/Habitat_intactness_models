# Install required packages
if(!requireNamespace("lme4", quietly=TRUE))
  install.packages("lme4", quiet=TRUE, dependencies=TRUE)
if(!requireNamespace("dplyr", quietly=TRUE))
  install.packages("dplyr", quiet=TRUE, dependencies=TRUE)
if(!requireNamespace("ggplot2", quietly=TRUE))
  install.packages("ggplot2", quiet=TRUE, dependencies=TRUE)

# Load required packages
library(lme4)
library(dplyr)
library(ggplot2)

# Set working directory
setwd("C:/Data")

# Import data
highHFP.drivers<-read.delim("species_data.txt")

# Extract data for generalists
generalists<-highHFP.drivers %>% filter(habitat_breadth>1)

# Recode red list categories as numeric
generalists[,7] <- ifelse(generalists[,7] == "LC", 0,
                           ifelse(generalists[,7] == "NT", 0,
                                  ifelse(generalists[,7] == "VU", 1,
                                         ifelse(generalists[,7] == "EN", 1,
                                                ifelse(generalists[,7] == "CR", 1, NA)))))

# Convert categorical variables to factor
generalists$order<-as.factor(generalists$order)
generalists$family<-as.factor(generalists$family)
generalists$genus<-as.factor(generalists$genus)
generalists$rl_cat<-as.factor(generalists$rl_cat)

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
generalists.na.omit<-na.omit(generalists[,vars])

# Create data frame for extinction risk (ER) models
data.generalists<-data.frame(or=generalists.na.omit$order,
                         fam=generalists.na.omit$family,
                         gen=generalists.na.omit$genus,     
                         cp5=generalists.na.omit$continuum_p5,
                         cp10=generalists.na.omit$continuum_p10,
                         cp50=generalists.na.omit$continuum_p50,
                         cp90=generalists.na.omit$continuum_p90,
                         cp95=generalists.na.omit$continuum_p95,
                         hp5=generalists.na.omit$hybrid_p5,
                         hp10=generalists.na.omit$hybrid_p10,
                         hp50=generalists.na.omit$hybrid_p50,
                         hp90=generalists.na.omit$hybrid_p90,
                         hp95=generalists.na.omit$hybrid_p95,
						             rs=generalists.na.omit$range_size,
						             gl=generalists.na.omit$gestation_length,
						             wa=generalists.na.omit$weaning_age,
                         rl.cat=generalists.na.omit$rl_cat)

# Scale the numerical variables
num_vars <- sapply(data.generalists, is.numeric)
data.generalists.scaled <- data.generalists
data.generalists.scaled[, num_vars] <- scale(data.generalists[, num_vars])

#----
# ER ~ continuum (c) + ...
cp5.glmer<-glmer(rl.cat~cp5+rs+gl+wa+(1|fam/gen), family=binomial, data=data.generalists.scaled, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
cp10.glmer<-glmer(rl.cat~cp10+rs+gl+wa+(1|fam/gen), family=binomial, data=data.generalists.scaled, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
cp50.glmer<-glmer(rl.cat~cp50+rs+gl+wa+(1|fam/gen), family=binomial, data=data.generalists.scaled, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
cp90.glmer<-glmer(rl.cat~cp90+rs+gl+wa+(1|fam/gen), family=binomial, data=data.generalists.scaled, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
cp95.glmer<-glmer(rl.cat~cp95+rs+gl+wa+(1|fam/gen), family=binomial, data=data.generalists.scaled, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

# Extract coefficients for c while holding other predictors constant at their mean
coef.cp5<-fixef(cp5.glmer)["cp5"]
coef.cp10<-fixef(cp10.glmer)["cp10"]
coef.cp50<-fixef(cp50.glmer)["cp50"]
coef.cp90<-fixef(cp90.glmer)["cp90"]
coef.cp95<-fixef(cp95.glmer)["cp95"]

# Extract the confidence interval for the estimated coefficient of the predictor of interest
coef.cp5.ci<-confint(cp5.glmer, method="Wald")["cp5",]
coef.cp10.ci<-confint(cp10.glmer, method="Wald")["cp10",]
coef.cp50.ci<-confint(cp50.glmer, method="Wald")["cp50",]
coef.cp90.ci<-confint(cp90.glmer, method="Wald")["cp90",]
coef.cp95.ci<-confint(cp95.glmer, method="Wald")["cp95",]

# Create a new data frame with a single row and column
cp5.effectsize.table <- data.frame(parameter = "5th", std_coefficient=coef.cp5[[1]], ci_lower=coef.cp5.ci[[1]], ci_upper=coef.cp5.ci[[2]])
cp10.effectsize.table <- data.frame(parameter = "10th", std_coefficient=coef.cp10[[1]], ci_lower=coef.cp10.ci[[1]], ci_upper=coef.cp10.ci[[2]])
cp50.effectsize.table <- data.frame(parameter = "50th", std_coefficient=coef.cp50[[1]], ci_lower=coef.cp50.ci[[1]], ci_upper=coef.cp50.ci[[2]])
cp90.effectsize.table <- data.frame(parameter = "90th", std_coefficient=coef.cp90[[1]], ci_lower=coef.cp90.ci[[1]], ci_upper=coef.cp90.ci[[2]])
cp95.effectsize.table <- data.frame(parameter = "95th", std_coefficient=coef.cp95[[1]], ci_lower=coef.cp95.ci[[1]], ci_upper=coef.cp95.ci[[2]])

# Combine the multiple group of rows together
c.effectsizes.table<-rbind(cp5.effectsize.table, cp10.effectsize.table, cp50.effectsize.table, cp90.effectsize.table, cp95.effectsize.table)

# Assign group, habitat model and habitat breadth
c.effectsizes.table$group <- "c"
c.effectsizes.table$habitat_model <- "Continuum"
c.effectsizes.table$habitat_breadth <- "Generalists"

#----
# ER ~ hybrid (h) + ...
hp5.glmer<-glmer(rl.cat~hp5+rs+gl+wa+(1|fam/gen), family=binomial, data=data.generalists.scaled, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hp10.glmer<-glmer(rl.cat~hp10+rs+gl+wa+(1|fam/gen), family=binomial, data=data.generalists.scaled, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hp50.glmer<-glmer(rl.cat~hp50+rs+gl+wa+(1|fam/gen), family=binomial, data=data.generalists.scaled, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hp90.glmer<-glmer(rl.cat~hp90+rs+gl+wa+(1|fam/gen), family=binomial, data=data.generalists.scaled, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
hp95.glmer<-glmer(rl.cat~hp95+rs+gl+wa+(1|fam/gen), family=binomial, data=data.generalists.scaled, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

# Extract coefficients for hybrid while holding other predictors constant at their mean
coef.hp5<-fixef(hp5.glmer)["hp5"]
coef.hp10<-fixef(hp10.glmer)["hp10"]
coef.hp50<-fixef(hp50.glmer)["hp50"]
coef.hp90<-fixef(hp90.glmer)["hp90"]
coef.hp95<-fixef(hp95.glmer)["hp95"]

# Extract the confidence interval for the estimated coefficient of the predictor of interest
coef.hp5.ci<-confint(hp5.glmer, method="Wald")["hp5",]
coef.hp10.ci<-confint(hp10.glmer, method="Wald")["hp10",]
coef.hp50.ci<-confint(hp50.glmer, method="Wald")["hp50",]
coef.hp90.ci<-confint(hp90.glmer, method="Wald")["hp90",]
coef.hp95.ci<-confint(hp95.glmer, method="Wald")["hp95",]

# Create a new data frame with a single row and column
hp5.effectsize.table <- data.frame(parameter = "5th", std_coefficient=coef.hp5[[1]], ci_lower=coef.hp5.ci[[1]], ci_upper=coef.hp5.ci[[2]])
hp10.effectsize.table <- data.frame(parameter = "10th", std_coefficient=coef.hp10[[1]], ci_lower=coef.hp10.ci[[1]], ci_upper=coef.hp10.ci[[2]])
hp50.effectsize.table <- data.frame(parameter = "50th", std_coefficient=coef.hp50[[1]], ci_lower=coef.hp50.ci[[1]], ci_upper=coef.hp50.ci[[2]])
hp90.effectsize.table <- data.frame(parameter = "90th", std_coefficient=coef.hp90[[1]], ci_lower=coef.hp90.ci[[1]], ci_upper=coef.hp90.ci[[2]])
hp95.effectsize.table <- data.frame(parameter = "95th", std_coefficient=coef.hp95[[1]], ci_lower=coef.hp95.ci[[1]], ci_upper=coef.hp95.ci[[2]])

# Combine the multiple group of rows together
h.effectsizes.table<-rbind(hp5.effectsize.table, hp10.effectsize.table, hp50.effectsize.table, hp90.effectsize.table, hp95.effectsize.table)

# Assign group, habitat model and habitat breadth
h.effectsizes.table$group <- "h"
h.effectsizes.table$habitat_model <- "Hybrid"
h.effectsizes.table$habitat_breadth <- "Generalists"

# Combine effect sizes for continuum and hybrid together
hc.effectsize.table<-rbind(c.effectsizes.table, h.effectsizes.table)

# Save effect sizes
write.table(hc.effectsize.table, file="effectsize_cmhm_generalists.csv", sep="\t", row.names=F)

