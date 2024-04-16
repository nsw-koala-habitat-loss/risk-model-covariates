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
