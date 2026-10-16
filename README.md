Repository for the code developed in the **Polyploidbreeding project**

### Vegetation Indices

The folder `veg_indices` contains the scripts to process and analyse vegetation index data obtained from the orthomosaics (UAV imagery) through `drone2report`: 

 - within species, merge all indices in a common file (plot, date, indexes)
 - correlate vegetation indexes with phenotypes
 - predict phenotypes based on vegetation indexes
 - assess the effect of thresholding

1. [DONE] split wheat index data into two subsets: i) *durum wheat*, 2) *bread wheat* (this is done for phenotypes, while indices are extracted on the fly)
2. [DONE] merge index datasets from different sensors
3. [DONE] integrate phenotypic data
4. [DONE] correlations
5. [TODO] predictions
