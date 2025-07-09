# Install required packages
if(!requireNamespace("dplyr", quietly=TRUE))
  install.packages("dplyr", quiet=TRUE, dependencies=TRUE)
if(!requireNamespace("ggplot2", quietly=TRUE))
  install.packages("ggplot2", quiet=TRUE, dependencies=TRUE)
if(!requireNamespace("ggpubr", quietly=TRUE))
  install.packages("ggpubr", quiet=TRUE, dependencies=TRUE)
if(!requireNamespace("ggcorrplot", quietly=TRUE))
  install.packages("ggcorrplot", quiet=TRUE, dependencies=TRUE)

# Load required packages
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggcorrplot)

# Set working directory
setwd("C:/Data")

# Import data
highHFP.drivers<-read.delim("species_data.txt")

# Recode red list categories as numeric
highHFP.drivers[,7] <- ifelse(highHFP.drivers[,7] == "LC", 0,
                              ifelse(highHFP.drivers[,7] == "NT", 0,
                                     ifelse(highHFP.drivers[,7] == "VU", 1,
                                            ifelse(highHFP.drivers[,7] == "EN", 1,
                                                   ifelse(highHFP.drivers[,7] == "CR", 1, NA)))))

# Convert categorical variables to factor
highHFP.drivers$order<-as.factor(highHFP.drivers$order)
highHFP.drivers$family<-as.factor(highHFP.drivers$family)
highHFP.drivers$genus<-as.factor(highHFP.drivers$genus)
highHFP.drivers$rl_cat<-as.factor(highHFP.drivers$rl_cat)

# Select variables from dataset
vars<-c("order",
        "family",
        "genus",
        "habitat_breadth",
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

# Remove species with missing values
highHFP.drivers.na.omit<-na.omit(highHFP.drivers[,vars])

# Extract data for generalist species
generalists<-highHFP.drivers.na.omit %>% filter(habitat_breadth>1)

# Extract data for specialist species
specialists<-highHFP.drivers.na.omit %>% filter(habitat_breadth==1)

# Create data frame for generalists
data.generalists<-data.frame(Patch_matrix=scale(generalists$patch_matrix),
                             Continuum=scale(generalists$continuum_p95),
                             Hybrid=scale(generalists$hybrid_p95),
                             Range_size=scale(generalists$range_size),
                             Gestation_length=scale(generalists$gestation_length),
                             Weaning_age=scale(generalists$weaning_age))

# Create data frame for specialists
data.specialists<-data.frame(Patch_matrix=scale(specialists$patch_matrix),
                             Continuum=scale(specialists$continuum_p50),
                             Hybrid=scale(specialists$hybrid_p95),
                             Range_size=scale(specialists$range_size),
                             Gestation_length=scale(specialists$gestation_length),
                             Weaging_age=scale(specialists$weaning_age))

# Visual inspection of the data normality using Q-Q plots (quantile-quantile plots)
# Q-Q plots for patch-matrix
ggqqplot(data.generalists$Patch_matrix, ylab = "Patch-matrix")
ggqqplot(data.specialists$Patch_matrix, ylab = "Patch-matrix")

# Q-Q plots for continuum
ggqqplot(data.generalists$Continuum, ylab = "Continuum")
ggqqplot(data.specialists$Continuum, ylab = "Continuum")

# Q-Q plots for hybrid
ggqqplot(data.generalists$Hybrid, ylab = "Hybrid")
ggqqplot(data.specialists$Hybrid, ylab = "Hybrid")

# Compute correlation matrix
corr.generalists <- cor(data.generalists,  method="spearman")
corr.specialists <- cor(data.specialists,  method="spearman")

# Save correlation matrix
write.table(corr.generalists, file="corr_generalists.csv", sep="\t", row.names=F)
write.table(corr.specialists, file="corr_specialists.csv", sep="\t", row.names=F)

