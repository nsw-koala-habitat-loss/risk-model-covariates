# # read in woody vegetation extent data for 2011
# woody <- rast(paste(getwd(), "/input/woody_cover/woody_nsw_2011.tif", sep = "")) %>% round()
# woody_mask <- woody %>% classify(cbind(c(0, 1), c(NA, 1)))
# 
# # read in koala habitat data and reclassify so 1 = koala habitat, 0 = non-habitat then aggregate, reproject, and snap to woody cover layer
# # then aggregate, reproject, and snap to woody cover layer
# khab <- rast(paste(getwd(), "/input/koala_habitat/KoalaHabitatSuitabilityModelClasses.tif", sep = "")) %>% aggregate(5, fun = "modal") %>% round() %>% classify(cbind(c(1, 2, 3, 4, 5, 6), c(0, 0, 0, 1, 1, 1))) %>% project(woody)
# khab <- khab * woody
# 
# # then calculate zonal using exact_extracxt
# zonal(khab, sum)/zonal(woody, sum)


# Purpose: To process koala habitat suitability 

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

# Load Libraries
library(tidyverse)
library(tidyterra)
library(terra)
library(sf)
library(stringr)
library(qs)
library(Rcpp)
library(exactextractr)
library(factoextra)
library(ggpubr)

INPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Input"
OUTPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Output"

# load woody raster as template
Woody <- rast(file.path(INPUT_DIR, "woody_nsw.tif"))
Woody_template <- rast(file.path(INPUT_DIR, "Woody_template.tif"))


# load koala habitat raster
khab_5m <- rast(file.path(INPUT_DIR, "Koala habitat suitability/KoalaHabitatSuitabilityModelClasses_v1p1/KoalaHabitatSuitabilityModelClasses.tif"))

Woody_khab <- project(Woody, crs(khab_5m), thread = TRUE)  

khab <- resample(khab_5m, Woody_khab, method = "mode", thread = TRUE) %>% 
  project(Woody, thread = TRUE) %>%
  round() %>%
  classify(cbind(c(1, 2, 3, 4, 5, 6), c(0, 0, 0, 1, 1, 1))) %>%
  crop(Woody, snap = "out", mask = TRUE)
plot(khab)
plot(Woody)
khab <- khab * Woody

writeRaster(khab, file.path(OUTPUT_DIR , "Raster/KoalaHabitatSuitability.tif"), overwrite = TRUE)
# writeRaster(khab, "../risk-model/input/covariates/KoalaHabitatSuitability.tif", overwrite = TRUE)
