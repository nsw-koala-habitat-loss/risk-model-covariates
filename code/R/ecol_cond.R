library(terra)
library(tidyverse)

dirname <- "H://risk-model-covariates"
ecol_cond <- file.path(dirname, r"{data\ecol_cond\biodiversityecologicalconditionofterrestrialhabitat\bba_31a_cond_V2_nsw_raw.tif}")

source('code/R/spatial_functions.R')

ecol_cond %>%
  projectRast(name = "ecol_cond", overwrite = TRUE) %>%
  clipRast(name = "ecol_cond", apply_mask = FALSE, to_output = FALSE, overwrite = TRUE) %>%
  resampleRast(name = "ecol_cond", overwrite = TRUE) %>%
  clipRast(name = "ecol_cond", to_output = TRUE, overwrite = TRUE)