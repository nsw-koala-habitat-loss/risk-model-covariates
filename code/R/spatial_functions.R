
library(terra)

dirname <- "code"
ref_raster <- terra::rast(file.path(dirname,  "../data/risk_analysis/clearing_spatial_units_data_prep/woody_veg_loss_prep/veg_loss/agr_loss_1119.tif"))
nsw = terra::vect(file.path(dirname, '../data/Admin_boundaries/nsw_lga.shp'))

loadShp <- function(name, input_shp) {
  return(terra::vect(input_shp))
}

projectShp <- function(input_shp, name = NA) {
  print(paste("Start projecting shapefile", name))
  if (is.na(name)) {
    stop("Name must be specified")
  }
  fname = file.path(dirname, "../intermediate_data", paste0(name, "_proj.shp"))
  projected_shp <- terra::vect(input_shp) %>%
    project(y = ref_raster)
  return(projected_shp)
}

projectRast <- function(input_raster, name = NA, overwrite = F) {
  print(paste("Start projecting raster", name))
  if (is.na(name)) stop("Name must be specified")
  fname = file.path(dirname, "../intermediate_data", paste0(name, "_proj.tif"))
  
  if (!overwrite & file.exists(fname)) {
    return(fname)
  }
  
  projected_rast <- terra::rast(input_raster) %>%
    project(y = ref_raster, filename = fname, threads = TRUE, overwrite = overwrite)
  return(fname)
}

shpToRast <- function(input_shp, name = NA, field_name = "", to_output = FALSE, overwrite = FALSE) {
  print(paste("Start converting Shp to Rast for", name))
  if (is.na(name)) stop("Name must be specified")
  if (to_output) {
    fname = file.path(dirname, "../output", paste0(name, ".tif"))
  } else {
    fname = file.path(dirname, "../intermediate_data", paste0(name, "_rasterised.tif"))
  }
  
  if (!overwrite & file.exists(fname)) {
    return(fname)
  }
  rasterised_shp <- terra::rasterize(x = input_shp, y = ref_raster, field = field_name, filename = fname, overwrite = overwrite)
  return(fname)
}

clipRast <- function(input_raster, name = NA, to_output = TRUE, overwrite = FALSE, apply_mask = TRUE) {
  print(paste("Start clipping raster", name))
  if (is.na(name)) stop("Name must be specified")
  if (to_output) {
    fname = file.path(dirname, "../output", paste0(name, ".tif"))
  } else {
    fname = file.path(dirname, "../intermediate_data", paste0(name, "_clip.tif"))
  }
  
  if (!overwrite & file.exists(fname)) {
    return(fname)
  }
  clipped_rast <- terra::rast(input_raster) %>%
    terra::crop(y = nsw, mask = apply_mask, filename = fname, overwrite = overwrite)
  return(fname)
}

resampleRast <- function(input_raster, name = NA, to_output = FALSE, overwrite = FALSE) {
  print(paste("Start resampling raster", name))
  if (is.na(name)) stop("Name must be specified")
  if (to_output) {
    fname = file.path(dirname, "../output", paste0(name, ".tif"))
  } else {
    fname = file.path(dirname, "../intermediate_data", paste0(name, "_resampled.tif"))
  }
  
  if (!overwrite & file.exists(fname)) {
    return(fname)
  }
  resample_rast <- terra::rast(input_raster) %>%
    terra::resample(y = ref_raster, filename = fname, overwrite = overwrite)
  return(fname)
}
