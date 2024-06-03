library(terra)
library(sf)

dirname <- "H://risk-model-covariates/code"

ref_raster <- terra::rast(file.path(dirname,  "../data/risk_analysis/clearing_spatial_units_data_prep/woody_veg_loss_prep/veg_loss/agr_loss_1119.tif"))

nsw = vect(file.path(dirname, "../data/Admin_boundaries/NSW_LGA_GDA2020/NSW_LGA_GDA2020.shp")) %>%
  project(y= ref_raster) %>%
  crop(ref_raster)
writeVector(nsw, filename = file.path(dirname, '../data/Admin_boundaries/nsw_lga.shp'), overwrite=T)

nsw_lga <- st_read(file.path(dirname, '../data/Admin_boundaries/nsw_lga.shp')) %>%
  st_union()

st_write(nsw_lga, file.path(dirname, '../data/Admin_boundaries/nsw_lga.shp'), append=F)



### Part 2 ####
# Purpose: To process create a raster template.
# In the template, raster cell value with Woody data will be filled with zero(0) and the rest will be filled with NA.

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

# Load Libraries
library(terra)

# load data
# load woody raster as template
Woody <- rast("Input/woody_nsw.tif")
Woody_template <- ifel(not.na(Woody), 9999, NA)
names(Woody_template) <- "EXT"
writeRaster(Woody_template, "Input/Woody_template.tif", overwrite = TRUE)
