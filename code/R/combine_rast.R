# Test outputs

output_dir <- "output"

output_files <- list.files(output_dir, "*.tif$", full.names = T)

terra::rast(output_files)
