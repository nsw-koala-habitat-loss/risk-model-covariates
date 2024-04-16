library(terra)
library(tidyverse)

source('code/R/spatial_functions.R')

soil_type_lyr <- "data/soil_fert/soiltypeascv45nsw/SoilType_ASC_NSW_v4_5_210429.shp"

soil_type_lyr %>%
  projectShp(name = "soil_type") %>%
  shpToRast(name = "soil_type", field_name = "ASC_code", overwrite=T) %>%
  resampleRast(name = "soil_type", overwrite=T) %>%
  clipRast(name = "soil_type", to_output = TRUE, overwrite=T)