# Analysis of vegetation indices

Vegetation indices are calculated by the software [drone2report](https://github.com/ne1s0n/drone2report) 
from the orthomosaics collected during the drone phenotyping experiment of the [polyploidbreeding project](https://polyploidbreeding.ibba.cnr.it/) (UAV imagery)

## Open questions

1. **thresholding**: do we want to use thresholding when calculating the vegetation indices? If so, which threshold on which indices?
    - a separate threshold for each index?
    - a fixed threshold on one index for all other indices?
    - no thresholds at all?
2. **data matching**: do we have complete correspondence between indices/plots one one hand, and genotypes/phenotypes on the other? What if we have a few mismatches? Why?
3. **thermal sensor**: what do we do with temperatures? Shall we use the difference from the temperature of the day, as measured by the weather service?
4. **DEM file**: how do we treat heights and volumes?

## Operational workflow

### Input data
#### Barley
- phenotypes: `Pheno_Fiorenzuola_2024_12_19_WB_semina 2023.csv`
- cross reference: `Fiorenzuola_gid_accession.csv`
- vegetation indices: indexes_RGB_F_complete.csv, indexes_multi_complete_da_0_1_F.csv, indexes_term_F.csv, indexes_F_Dem.csv

#### Wheat
- phenotypes: `Phenotypic_data_SIS_Crop_season23-24_unito.xlsx`
- cross reference:
- vegetation indices: 

### Scripts
1. [merge_indices.r](veg_indices/prep_data/merge_indices.r): this creates a combined file with all indices (from all sensors, from all flights) per species
2. 
