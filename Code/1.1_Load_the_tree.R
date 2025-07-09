# Install required packages
if(!requireNamespace("ape", quietly=TRUE))
  install.packages("ape", quiet=TRUE, dependencies=TRUE)

# Load necessary packages
library(ape)

#Set working directory
setwd("C:/phy")

getwd()

# Check that the file exists in current directory
file.exists("MamPhy_fullPosterior_BDvr_Completed_5911sp_topoCons_FBDasZhouEtAl_all10k_v2_nexus.trees") 

# NOTE: The .trees file used here was downloaded from:
# Upham, N. S., Esselstyn, J. A., & Jetz, W. (2019). Inferring the mammal tree: 
# Species-level sets of phylogenies for questions in ecology, evolution, and conservation. 
# PLOS Biology, 17(12), e3000494. https://doi.org/10.1371/journal.pbio.3000494
# File: MamPhy_fullPosterior_BDvr_Completed_5911sp_topoCons_FBDasZhouEtAl_all10k_v2_nexus.trees

# Load the first 100 trees (for quick consensus)
trees <- read.nexus("MamPhy_fullPosterior_BDvr_Completed_5911sp_topoCons_FBDasZhouEtAl_all10k_v2_nexus.trees")[1:100]

# Create a majority-rule consensus tree
consensus_tree <- consensus(trees, p=0.5)

# Save consensus tree
write.tree(consensus_tree, "mammal_supertree.tre")
