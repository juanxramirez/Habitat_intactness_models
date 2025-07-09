## Habitat_intactness_models #



This repository includes data and `R` code to reproduce the analyses shown in the article:

**Global comparison of habitat intactness models for predicting extinction risk in terrestrial mammals**

_by J.P. Ramírez-Delgado, M. Di Marco, C.J. Johnson, J.E.M. Watson, H.L. Beyer, L. De Assis Barros, R. Pillay, and O. Venter_

In this article, we compare the ability of patch-matrix, continuum, and hybrid models of habitat intactness to explain the risk of extinction for terrestrial mammals on a global scale.

Users should have `R` version 4.2.3 or higher to execute the scripts.

`R` scripts have been tested on `RStudio` version 2023.03.0+386.

## Data ##

The `species_data.txt` serves as input for scripts 1 to 15 and includes the following variables:
- taxon_id: UCN taxon identifier for each species.
- speciesName: Scientific name of each species.
- order: Taxonomic order to which each species belongs.
- family: Taxonomic family to which each species belongs.
- genus: Taxonomic genus to which each species belongs.
- patch_matrix: Proportion of each species’ range overlapping with low human footprint levels (HFP values <3).
- continuum_p5: 5th percentile of continuum model values within a species’ range.
- continuum_p10: 10th percentile of continuum model values within a species’ range.
- continuum_p50: 50th percentile of continuum model values within a species’ range.
- continuum_p90: 90th percentile of continuum model values within a species’ range.
- continuum_p95: 95th percentile of continuum model values within a species’ range.
- hybrid_p5: 5th percentile of continuum model values within relatively intact habitat patches (i.e., HFP values <3) within a species’ range.
- hybrid_p10: 10th percentile of continuum model values within relatively intact habitat patches (i.e., HFP values <3) within a species’ range.
- hybrid_p50: 50th percentile of continuum model values within relatively intact habitat patches (i.e., HFP values <3) within a species’ range.
- hybrid_p90: 90th percentile of continuum model values within relatively intact habitat patches (i.e., HFP values <3) within a species’ range.
- hybrid_p95: 95th percentile of continuum model values within relatively intact habitat patches (i.e., HFP values <3) within a species’ range.
- habitat_breadth: Number of habitat types used as suitable habitat by a species.
- range_size: Area in square kilometres of a species’ distribution.
- gestation_length: Length of time of fetal growth in days as a proxy for a species’ reproductive output.
- weaning age: Age at which primary nutritional dependency on the mother ends and independent foraging begins in days as a proxy for a species’ reproductive onset.
- realm: Biogeographic realm in which a species has over 50% of its distribution.
- criterionB: Binary variable indicating whether a species was assessed exclusively under IUCN Red List Criterion B, where ‘1’ represents species assessed solely under Criterion B, and ‘0’ represents species assessed under other or multiple criteria.
- rl_cat: IUCN Red List category assigned to a species.
- threatened_status: Threatened status of a species.
