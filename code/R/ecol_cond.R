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

### Reprocess missing data###
Woody <- rast("Input/woody_nsw.tif")

ecol_cond <- rast("Input/biodiversityecologicalconditionofterrestrialhabitat/bba_31a_cond_V2_nsw_raw.tif") %>% 
  project(crs(Woody), thread = TRUE) %>% 
  resample(Woody, method = "bilinear", thread = TRUE) %>% 
  crop(Woody, snap = "out" , mask = TRUE)
names(ecol_cond) <- "EcolCond"

writeRaster(ecol_cond, "Output/Raster/EcolCond.tif", overwrite = TRUE)
