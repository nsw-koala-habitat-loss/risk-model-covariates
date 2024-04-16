# Test outputs
library(terra)
library(stringr)
library(sf)
library(ggplot2)

output_dir <- "output"
nsw <- file.path('data/Admin_boundaries/nsw_lga.shp') %>%
  vect()

load_covariates <- function(output_dir, idx = NULL) {
  output_files <- list.files(output_dir, "*.tif$", full.names = T)
  lyr_names <- list.files(output_dir, "*.tif$") %>% tools::file_path_sans_ext()
  
  if (is.null(idx)) {
    idx <- 1:length(lyr_names)
  }
  
  comb_rast <- rast(output_files[idx])
  names(comb_rast) <- lyr_names[idx]
  return(comb_rast)
}

comb_rast <- load_covariates(output_dir)

# Generate plots
categorical_vars <- c('drought', 'fire', 'forest_code', 'forest_tenure', 'forest_tenure_type', 'remoteness', 'landuse', 'soil_fert', 'soil_type')
for (i in 1:length(names(comb_rast))){
  name <- names(comb_rast)[i]
  png(paste0("plots/", name, ".png"), width = 1000, height = 800, units = 'px',pointsize = 24)
  if(name %in% categorical_vars) {
    col_scheme <- grDevices::hcl.colors(10, 'dynamic')
  } else {
    col_scheme = rev(grDevices::terrain.colors(50))
  }
  plot(comb_rast[[i]], col = col_scheme)
  lines(nsw)
  dev.off()
}

# Write a single multidimensional raster if appropriate
#writeRaster(comb_rast, file.path(output_dir, 'comb_rast.tif'), overwrite = T)
