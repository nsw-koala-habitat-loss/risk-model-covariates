# Data processing pipeline for covariates in the deforestation risk model

Replicable data processing pipeline that converts input layers into a consistent set of raster files with the common grid.

## Requirements
* R (terra, sf, tidyverse)
* RStudio (for batch processing using RStudio API)

## Steps
1. Check the directory of the "code" folder in the `parameters.R`
2. Run `run_all_bg.R` to batch process all covariates using the RStudio API background jobs (each covariate will be processed in a separate instance of R)
3. Check whether the covariates align (and rename covariates) in the `combine_rast.R` file