# Install required packages
if(!requireNamespace("raster", quietly=TRUE))
  install.packages("raster", quiet=TRUE, dependencies=TRUE)

# Load necessary packages
library(raster)

# Set working directory
setwd("C:/Intactness")

# Load Human Footprint raster
hfp_raster <- raster("hfp2013_merisINT.tif")

# NOTE: The .tif file used here was obtained from:
# Williams, B. A., Venter, O., Allan, J. R., Atkinson, S. C., Rehbein, J. A., Ward, M., 
# Di Marco, M., Grantham, H. S., Ervin, J., Goetz, S. J., Hansen, A. J., Jantz, P., 
# Pillay, R., Rodríguez-Buriticá, S., Supples, C., Virnig, A. L. S., & Watson, J. E. M. (2020). 
# Change in terrestrial human footprint drives continued loss of intact ecosystems. 
# One Earth, 3(3), 371–382. https://doi.org/10.1016/j.oneear.2020.08.009

# Set HFP values <0 or >50 as NA to maintain data integrity
hfp_raster[hfp_raster < 0 | hfp_raster > 50] <- NA

# Cell resolution (in km)
cell_res <- res(hfp_raster)[1]
if (cell_res > 100) cell_res <- cell_res / 1000
cat("Cell resolution (km):", cell_res, "\n")

# Parameters from Beyer's methodology
z <- 0.5
beta <- 0.2
gamma <- 0.80471895621705  # Habitat Quality = 20% at HFP > 3

# Calculate habitat quality 
quality <- exp(-gamma * hfp_raster)

# Kernel radius set at 26.5 km as per Beyer's global analysis
rad <- 26.5
kernel_radius <- ceiling(rad / cell_res)
kernel_size <- kernel_radius * 2 + 1
center <- kernel_radius + 1

# Construct exponential focal kernel for weights (Beyer's method)
foc_kernel_weights <- matrix(0, kernel_size, kernel_size)
for (i in 1:kernel_size) {
  for (j in 1:kernel_size) {
    d <- sqrt((i - center)^2 + (j - center)^2) * cell_res
    if (d <= rad) {
      foc_kernel_weights[i, j] <- exp(-beta * d)
    }
  }
}

# Normalize kernel weights to ensure they sum to 1
foc_kernel_weights <- foc_kernel_weights / sum(foc_kernel_weights)

# Convert kernel weights to vector for use in focal()
dvec <- as.vector(foc_kernel_weights)

# Construct circular kernel to define the shape only
# This kernel sets cells within the radius = 1 and outside = NA.
# The actual weighting is handled separately by dvec.
foc_kernel_circular <- foc_kernel_weights
foc_kernel_circular[foc_kernel_circular > 0] <- 1

# Visualization of kernel weights for verification
image(foc_kernel_weights, main="Exponential Kernel Weights (Beyer's Method)")

# qprime function aligned with Beyer's formula:
# Q'_i = [Σ(wi * wj)^z * exp(-βdij)] / [Σexp(-βdij)]
qprime <- function(x) {
  center <- ceiling(length(x) / 2)
  
  # Return NA if the center value is missing
  if (is.na(x[center])) return(NA)
  
  # Identify valid (non-NA) pixels
  valid <- which(!is.na(x))
  
  # Ensure weights match valid pixels
  weights <- dvec[valid] / sum(dvec[valid])
  
  # Quality product matches (wi * wj)^z component
  quality_product <- (x[center] * x[valid])^z
  
  # Calculate intactness
  intactness <- sum(quality_product * weights, na.rm = TRUE)
  
  # Return NA if intactness calculation is invalid
  if (is.nan(intactness) || intactness <= 0) return(NA)
  
  return(intactness)
}

# Calculate habitat intactness raster:
# - foc_kernel_circular defines shape only (binary: inside=1, outside=NA).
# - Actual weights defined via dvec.
intactness <- focal(quality, 
                    w = foc_kernel_circular, 
                    fun = qprime, 
                    pad = TRUE, 
                    dvec = dvec)

# Align and mask intactness raster with original HFP raster for precise alignment
intactness <- mask(intactness, hfp_raster)

# Verification of raster alignment
if (!compareRaster(hfp_raster, intactness, extent=TRUE, rowcol=TRUE, crs=TRUE, res=TRUE, stopiffalse=FALSE)) {
  stop("Raster alignment failed: verify your inputs.")
} else {
  cat("✅ Raster alignment confirmed.\n")
}

# Match NA pattern of intactness raster to the original HFP raster
intactness[is.na(hfp_raster)] <- NA

# Normalization of intactness raster to a 0–1 range for ecological interpretability
min_val <- cellStats(intactness, "min", na.rm=TRUE)
max_val <- cellStats(intactness, "max", na.rm=TRUE)

if (!is.finite(min_val) || !is.finite(max_val) || max_val <= min_val) {
  stop("Normalization failed: verify intactness raster values.")
}

intactness_norm <- (intactness - min_val) / (max_val - min_val)

# Plot normalized intactness raster for visual verification
plot(intactness_norm, col=rev(terrain.colors(100)),
     main="Normalized Habitat Intactness (Aligned with HFP)")

# Save aligned and normalized intactness raster to file
writeRaster(intactness_norm, "intactness_middle_2013.tif", overwrite=TRUE)
