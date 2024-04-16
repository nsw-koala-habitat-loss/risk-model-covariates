library(terra)
library(tidyverse)

source('code/R/spatial_functions.R')

soil_fert_lyr <- "data/soil_fert/inherentsoilfertility_nsw_v4_5_211020/Fertility_NSW_v4_5_211020.shp"

soil_fert_lyr %>%
  projectShp(name = "soil_fert") %>%
  shpToRast(name = "soil_fert", field_name = "Fert_code", overwrite=T) %>%
  resampleRast(name = "soil_fert", overwrite=T) %>%
  clipRast(name = "soil_fert", to_output = TRUE, overwrite=T)