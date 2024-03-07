library(tidyverse)

source('code/R/spatial_functions.R')

dirname <- "H://risk-model-covariates/code"

## Landuse shp
nsw_landuse <- file.path(dirname, "../data/landuse/landnswlanduse2007/NSWLanduse_2007shp.shp")

output <- nsw_landuse %>%
  projectShp(name = "landuse") %>%
  shpToRast(name = "landuse", field_name = "LU_ALUM7Co") %>%
  resampleRast(name = "landuse") %>%
  clipRast(name = "landuse", to_output = TRUE)