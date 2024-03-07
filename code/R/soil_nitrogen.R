library(terra)
library(tidyverse)

dirname <- "H://risk-model-covariates"
soil_nitrogen_lyr <- file.path(dirname, "data/soil_fert/Soil_nitrogen_0_5/NTO_000_005_05_N_P_AU_NAT_C_20140801.tif")

source('code/R/spatial_functions.R')

soil_nitrogen_lyr %>%
  projectRast(name = "soil_nitrogen", overwrite = TRUE) %>%
  clipRast(name = "soil_nitrogen", apply_mask = FALSE, to_output = FALSE, overwrite = TRUE) %>%
  resampleRast(name = "soil_nitrogen", overwrite = TRUE) %>%
  clipRast(name = "soil_nitrogen", to_output = TRUE, overwrite = TRUE)