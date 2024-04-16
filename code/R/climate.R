# climate
library(terra)
library(tidyverse)

args <- commandArgs(trailingOnly=T)
run_single_year <- !is.na(args[1]) & !exists('yr')
print(args[1])
if (run_single_year) {
  years <- sum(1988+as.numeric(args[1]))
} else {
  if (exists('yr')) {
    years <- yr
  } else {
    years <- 2011:2019
  }
}
print(years)

source('code/R/spatial_functions.R')

# precipitation
prec_list <- c()
for (i in years) {
  # get download URLs and file names
  URL1 <- paste0("http://opendap.bom.gov.au:8080/thredds/fileServer/agcd/precip/total/r005/12month/", as.character(i),
                 "/precip_total_r005_", as.character(i), "0101_", as.character(i), "1231.nc")
  File1 <- paste0("data/prec", "/precip_total_r005_", as.character(i), "0101_", as.character(i), "1231.nc")

  # download files
  if (!file.exists(File1)) {
    download.file(URL1, File1, mode = "wb")
  }
  prec_list <- c(prec_list, File1)
}

# mean mean temperature
tmean_list <- c()
for (i in years) {
  # get download URLs and file names
  URL1 <- paste0("http://opendap.bom.gov.au:8080/thredds/fileServer/agcd/tmean/mean/r005/12month/", as.character(i),
                 "/tmean_mean_r005_", as.character(i), "0101_", as.character(i), "1231.nc")
  File1 <- paste0("data/prec", "/tmean_mean_r005_", as.character(i), "0101_", as.character(i), "1231.nc")
  
  # download files
  if (!file.exists(File1)) {
    download.file(URL1, File1, mode = "wb")
  }
  tmean_list <- c(tmean_list, File1)
}


averageRaster <- function(rast_list) {
  res <- lapply(rast_list, function(r) {
    terra::rast(r) %>%
      project(crs(nsw))
  })
  return(terra::mean(rast(res)))
}


if (run_single_year) {
  prec_name = paste0('prec_', years)
  temp_name = paste0("temp_", years)
} else {
  prec_name = 'prec'
  temp_name = 'temp'
}

prec_path <- paste0("intermediate_data/", prec_name, ".tif")
prec_list %>%
  averageRaster() %>%
  terra::writeRaster(filename = prec_path, overwrite = T)

tmean_path <- paste0("intermediate_data/", temp_name, ".tif")
tmean_list %>%
  averageRaster() %>%
  terra::writeRaster(tmean_path, overwrite = T)

## Process using pipeline
  
prec_output <- prec_path %>%
  clipRast(name = prec_name, apply_mask = FALSE, to_output = FALSE, overwrite = TRUE) %>%
  projectRast(name = prec_name, overwrite = TRUE) %>%
  resampleRast(name = prec_name, overwrite = TRUE) %>%
  clipRast(name = prec_name, to_output = TRUE, overwrite = TRUE)

temp_output <- tmean_path %>%
  clipRast(name = temp_name, apply_mask = FALSE, to_output = FALSE, overwrite = TRUE) %>%
  projectRast(name = temp_name, overwrite = TRUE) %>%
  resampleRast(name = temp_name, overwrite = TRUE) %>%
  clipRast(name = temp_name, to_output = TRUE, overwrite = TRUE)
