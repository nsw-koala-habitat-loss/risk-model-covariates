library(tidyverse)
library(terra)
library(sf)

source('code/R/spatial_functions.R')

dirname <- "H://risk-model-covariates/code"

## Landuse shp
nsw_landuse_2013 <- file.path(dirname, "../data/landuse/landnswlanduse2013/Land_NSW_Landuse_2013/Data/Shapefile/NSW_Landuse_2013.shp")

# Superimpose Sydney region of 2017 to 2007 dataset
output <- nsw_landuse_2013 %>%
  projectShp(name = "landuse") %>%
  shpToRast(name = "landuse", field_name = "TertiaryAL", overwrite=T) %>%
  resampleRast(name = "landuse", overwrite=T) %>%
  clipRast(name = "landuse", overwrite=T, to_output = TRUE)