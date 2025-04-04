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
setwd("My_working_directory")

# Import data
highHFP.drivers<-read.delim("species_data.txt")

# Recode red list categories as numeric
highHFP.drivers[,32] <- ifelse(highHFP.drivers[,32] == "LC", 0,
                              ifelse(highHFP.drivers[,32] == "NT", 0,
                                     ifelse(highHFP.drivers[,32] == "VU", 1,
                                            ifelse(highHFP.drivers[,32] == "EN", 1,
                                                   ifelse(highHFP.drivers[,32] == "CR", 1, NA)))))

# Convert categorical variables to factor
highHFP.drivers$order<-as.factor(highHFP.drivers$order)
highHFP.drivers$rl_cat<-as.factor(highHFP.drivers$rl_cat)

# Select variables from dataset
vars<-c("order",
        "habitat_breadth",
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
highHFP.drivers.gain.excluded <- highHFP.drivers %>%
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

# Remove species with missing values
highHFP.drivers.gain.excluded.na.omit<-na.omit(highHFP.drivers.gain.excluded[,vars])

# Extract data for generalist species
generalists<-highHFP.drivers.gain.excluded.na.omit %>% filter(habitat_breadth>1)

# Extract data for specialist species
specialists<-highHFP.drivers.gain.excluded.na.omit %>% filter(habitat_breadth==1)

# Create data frame for generalists
data.generalists<-data.frame(PMM=scale(generalists$patch_matrix),
                             RPMM=scale(generalists$change_patch_matrix),
                             CM=scale(generalists$continuum_p95),
                             RCM=scale(generalists$change_continuum_p95),
                             HM=scale(generalists$hybrid_p95),
                             RHM=scale(generalists$change_hybrid_p95),
                             RS=scale(generalists$range_size),
                             GL=scale(generalists$gestation_length),
                             WA=scale(generalists$weaning_age))

# Create data frame for specialists
data.specialists<-data.frame(PMM=scale(specialists$patch_matrix),
                             RPMM=scale(specialists$change_patch_matrix),
                             CM=scale(specialists$continuum_p95),
                             RCM=scale(specialists$change_continuum_p95),
                             HM=scale(specialists$hybrid_p95),
                             RHM=scale(specialists$change_hybrid_p95),
                             RS=scale(specialists$range_size),
                             GL=scale(specialists$gestation_length),
                             WA=scale(specialists$weaning_age))

# Visual inspection of the data normality using Q-Q plots (quantile-quantile plots)
# Q-Q plots for patch-matrix
ggqqplot(data.generalists$PM, ylab = "Patch-matrix")
ggqqplot(data.specialists$PM, ylab = "Patch-matrix")

# Q-Q plots for continuum
ggqqplot(data.generalists$CM, ylab = "Continuum")
ggqqplot(data.specialists$CM, ylab = "Continuum")

# Q-Q plots for hybrid
ggqqplot(data.generalists$HM, ylab = "Hybrid")
ggqqplot(data.specialists$HM, ylab = "Hybrid")

# Compute correlation matrix
corr.generalists <- cor(data.generalists,  method="spearman")
corr.specialists <- cor(data.specialists,  method="spearman")

# Save correlation matrix
write.table(corr.generalists, file="corr_generalists.csv", sep="\t", row.names=F)
write.table(corr.specialists, file="corr_specialists.csv", sep="\t", row.names=F)
