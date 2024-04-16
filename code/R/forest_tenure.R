library(tidyverse)
library(terra)
library(foreign)
library(doParallel)
library(foreach)

source('code/R/spatial_functions.R')

## Landuse shp
forest_tenure_lyr <- "data/forest_tenure/aus_forten18_geotiff/aus_forten18_geotiff/aus_forten18.tif"
forest_tenure_dbf <- read.dbf("data/forest_tenure/aus_forten18_geotiff/aus_forten18_geotiff/aus_forten18.tif.vat.dbf")

forest_tenure_rast <- terra::rast('output/forest_tenure.tif')

output <- forest_tenure_lyr %>%
  projectRast(name = "forest_tenure", overwrite = T) %>%
  clipRast(name = "forest_tenure", apply_mask = FALSE, to_output = FALSE, overwrite = T) %>%
  resampleRast(name = "forest_tenure", overwrite = T) %>%
  clipRast(name = "forest_tenure", to_output = FALSE, overwrite = T)

forest_tenure_rast <- rast(output)
activeCat(forest_tenure_rast) <- 1

forest_tenure_rast %>% plot()

# Remap the output to values in the DBF
vals <- terra::values(rast(output)) # Index numbers
colnames_dbf <- c(
  forest_tenure_type = "TEN_TYPE",
  forest_tenure = "FOR_TEN",
  forest_code = "FOR_CODE")

for (i in 1:length(colnames_dbf)) {
  name = names(colnames_dbf[i])
  column = as.character(colnames_dbf[i])
  rcl = forest_tenure_dbf[,c("VALUE", column)]
  filename = file.path("output", paste0(name, ".tif"))
  output_rast <- rast(output)
  set.cats(output_rast, layer = 1, value = rcl)
  writeRaster(output_rast, filename, overwrite=T)
}
