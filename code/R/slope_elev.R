library(terra)
library(tidyverse)

source('code/R/parameters')
elev <- file.path(dirname, "../data/slope/srtm-1sec-dem-v1-COG.tif")

source('code/R/spatial_functions.R')

elev_clipped <- elev %>%
  projectRast(name = "elev", overwrite = TRUE) %>%
  clipRast(name = "elev", apply_mask = FALSE, to_output = FALSE, overwrite = TRUE)

slope_name <- "intermediate_data/slope.tif"
slope <- terrain(rast(elev_clipped), filename= slope_name, overwrite=TRUE)

slope_name %>%
  resampleRast(name = "slope", overwrite = TRUE) %>%
  clipRast(name = "slope", to_output = TRUE, overwrite = TRUE)

elev_clipped %>%
  resampleRast(name = "elev", overwrite = TRUE) %>%
  clipRast(name = "elev", to_output = TRUE, overwrite = TRUE)