# Install required packages
if(!requireNamespace("ape", quietly=TRUE))
  install.packages("ape", dependencies=TRUE)
if(!requireNamespace("lme4", quietly=TRUE))
  install.packages("lme4", dependencies=TRUE)
if(!requireNamespace("phytools", quietly=TRUE))
  install.packages("phytools", dependencies=TRUE)
if(!requireNamespace("dplyr", quietly=TRUE))
  install.packages("dplyr", dependencies=TRUE)

# Load necessary packages
library(ape)
library(lme4)
library(phytools)
library(dplyr)

# Set working directory
setwd("C:/Data")

# Load dataset
ext.risk <- read.delim("species_data.txt")
ext.risk.mammals <- ext.risk %>% filter(habitat_breadth > 1)

# Load mammal supertree
phylo <- read.tree("mammal_supertree.tre")

# Adjust tree tip labels
phylo$tip.label <- sapply(strsplit(phylo$tip.label, "_"), function(x) paste(x[1], x[2], sep="_"))

# Adjust dataset species names to match the tree
ext.risk.mammals$speciesName_fixed <- gsub(" ", "_", ext.risk.mammals$speciesName)

# Match species between tree and dataset
matched_species <- intersect(ext.risk.mammals$speciesName_fixed, phylo$tip.label)
pruned_tree <- drop.tip(phylo, setdiff(phylo$tip.label, matched_species))

# Subset and reorder data exactly matching the pruned tree
phylo_data <- ext.risk.mammals %>% 
  filter(speciesName_fixed %in% pruned_tree$tip.label)
phylo_data <- phylo_data[match(pruned_tree$tip.label, phylo_data$speciesName_fixed), ]

# Select and rename variables
vars <- c("order", "family", "genus", "hybrid_p95", 
          "range_size", "gestation_length", "weaning_age", "rl_cat")
phylo_data_model <- phylo_data[, vars]
colnames(phylo_data_model) <- c("or", "fam", "gen", "hm", "rs", "gl", "wa", "rl.cat")

# Recode rl.cat into numeric binary (0/1)
phylo_data_model$rl.cat <- ifelse(phylo_data_model$rl.cat %in% c("LC", "NT"), 0,
                                  ifelse(phylo_data_model$rl.cat %in% c("VU", "EN", "CR"), 1, NA))

# Store species names before removing NAs
phylo_data_model$speciesName_fixed <- phylo_data$speciesName_fixed

# Remove rows with any NA values
phylo_data_model_noNA <- na.omit(phylo_data_model)

# Prune the tree again to match the final dataset
final_species_names <- phylo_data_model_noNA$speciesName_fixed
pruned_tree_final <- drop.tip(pruned_tree, setdiff(pruned_tree$tip.label, final_species_names))

# Reorder the final dataset to match the final pruned tree
phylo_data_model_noNA <- phylo_data_model_noNA[match(pruned_tree_final$tip.label, phylo_data_model_noNA$speciesName_fixed), ]

# Scale only numeric predictors (exclude response 'rl.cat')
vars_to_scale <- c("hm", "rs", "gl", "wa")
phylo_data_model_noNA[vars_to_scale] <- scale(phylo_data_model_noNA[vars_to_scale])

# Run logistic mixed-effects model
hm.glmer <- glmer(rl.cat ~ hm + rs + gl + wa + (1|or/fam/gen), 
                  family = binomial, 
                  data = phylo_data_model_noNA,
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
head(phylo_data_model_noNA)

# Extract residuals to a separate vector
residuals_vector <- residuals(hm.glmer, type="response")

# Assign names clearly to the residuals_vector
names(residuals_vector) <- pruned_tree_final$tip.label

# Confirm names assigned correctly
head(names(residuals_vector))
head(pruned_tree_final$tip.label)

# Assign equal branch lengths again
pruned_tree_final$edge.length <- rep(1, nrow(pruned_tree_final$edge))

# Run phylosig() using the named residual vector
phylo_signal_result <- phylosig(pruned_tree_final, 
                                residuals_vector, 
                                method="lambda", 
                                test=TRUE)

# Print result
print(phylo_signal_result)
