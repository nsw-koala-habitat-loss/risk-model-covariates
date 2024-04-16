library(dplyr)

if (!exists('job_id')) {
  job_id <- as.numeric(commandArgs(trailingOnly = T)[1])
} else {
  job_id <- 1
}

rast_files <- list.files('data/woody/woodyv3_geotiff', '*.tif$', full.names = T)
rast_names <- list.files('data/woody/woodyv3_geotiff', '*.tif$', full.names = F) %>% 
  tools::file_path_sans_ext()

extract_woody <- function(obj) {
  source('code/R/spatial_functions.R')
  path <- obj$path
  year <- obj$year
  
  name <-  paste0("woody_veg_", year)
  path %>%
    projectRast(name = name, overwrite = TRUE) %>%
    clipRast(name = name, apply_mask = FALSE, to_output = FALSE, overwrite = TRUE) %>%
    resampleRast(name = name, overwrite = TRUE) %>%
    clipRast(name = name, to_output = TRUE, overwrite = TRUE)
}

r <- list()
for (i in 1:length(rast_files)) {
  r[[i]] <- list(path = rast_files[i], year = substr(rast_names[i], 6,9))
}

extract_woody(r[[job_id]])
#plan(multisession, gc = TRUE, workers = 8)
#future_lapply(r, extract_woody, future.seed = TRUE)
