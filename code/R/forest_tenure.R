## Updated forest tenure in the second part of this script. ####

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


#### Forest Tenure 2 ####
## Corrected the errors in previous development ##

# Purpose: To process forest tenure data

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
library(readxl)


# load data
# load woody raster as template
Woody <- rast("Input/woody_nsw.tif")
Woody_template <- rast("Input/Woody_template.tif")

Aus_forten18 <- rast("Input/aus_forten18_geotiff/aus_forten18.tif")

# load forest tenure data
forest_tenure_dbf <- read.dbf("Input/aus_forten18_geotiff/aus_forten18.tif.vat.dbf") %>% 
  mutate(FOR_CODE = ifelse(STATE == "NSW", FOR_CODE, NA),
         TEN_TYPE = if_else(STATE == "NSW", TEN_TYPE, NA),
         FOR_TEN = if_else(STATE == "NSW", FOR_TEN, NA))

# Produce a lookup table
forest_tenure_dbf_NSW <- forest_tenure_dbf %>% 
  drop_na(FOR_CODE) %>%
  distinct(FOR_CODE, .keep_all = TRUE) %>%
  arrange(FOR_CODE) %>% 
  write.csv("Input/aus_forten18_geotiff/NSW_forten18.csv", row.names = FALSE)

# Look up table further developed in excel to produce standardised numeric CODE 
# Load the updated lookup table

NSW_forten18_xl <- read_xlsx("covariate_description.xlsx", sheet = "NSW_forten18") %>%
  select(FOR_CODE, FOR_TYPE1_Code, FOR_TEN_CODE) %>% 
  right_join(forest_tenure_dbf, by = join_by("FOR_CODE" == "FOR_CODE")) %>%
  select(VALUE, STATE, FOR_CODE, FOR_TYPE1_Code, FOR_TEN_CODE) %>% 
  mutate(FOR_CODE = ifelse(STATE == "NSW", FOR_CODE, NA),
         FOR_TYPE1_Code = if_else(STATE == "NSW", FOR_TYPE1_Code, NA),
         FOR_TEN_CODE = if_else(STATE == "NSW", FOR_TEN_CODE, NA))


forest_tenure_dbf %>% tidyr::drop_na(FOR_CODE) %>% distinct(FOR_TYPE, .keep_all = TRUE) %>% arrange(FOR_TYPE)


Aus_forten18_GDALamB <- project(Aus_forten18, crs(Woody), threads=TRUE)
levels(Aus_forten18_GDALamB) <- NULL

NSW_forten18_ForCode <- classify(Aus_forten18_GDALamB, cbind(forest_tenure_dbf$VALUE, forest_tenure_dbf$FOR_CODE)) %>% 
  crop(Woody, snap = "out") %>% 
  resample(Woody, method = "mode", thread = TRUE) %>% 
  mask(Woody)
names(NSW_forten18_ForCode) <- "ForCode"
writeRaster(NSW_forten18_ForCode, "Output/Raster/NSW_forten18_ForCode.tif", overwrite=TRUE)

NSW_forten18_ForType <- classify(Aus_forten18_GDALamB, cbind(NSW_forten18_xl$VALUE, NSW_forten18_xl$FOR_TYPE1_Code)) %>% 
  crop(Woody, snap = "out") %>% 
  resample(Woody, method = "mode", thread = TRUE) %>% 
  mask(Woody)
names(NSW_forten18_ForType) <- "ForType"
NSW_forten18_ForType <- ifel(not.na(NSW_forten18_ForType$ForType), NSW_forten18_ForType$ForType, Woody_template$EXT)
names(NSW_forten18_ForType) <- "ForType"
writeRaster(NSW_forten18_ForType, "Output/Raster/NSW_forten18_ForType.tif", overwrite=TRUE)

NSW_forten18_ForTen <- classify(Aus_forten18_GDALamB, cbind(NSW_forten18_xl$VALUE, NSW_forten18_xl$FOR_TEN_CODE)) %>% 
  crop(Woody, snap = "out") %>% 
  resample(Woody, method = "mode", thread = TRUE) %>% 
  mask(Woody)
names(NSW_forten18_ForTen) <- "ForTen"
NSW_forten18_ForTen <- ifel(not.na(NSW_forten18_ForTen$ForTen),NSW_forten18_ForTen$ForTen, Woody_template$EXT)
names(NSW_forten18_ForTen) <- "ForTen"
writeRaster(NSW_forten18_ForTen, "Output/Raster/NSW_forten18_ForTen.tif", overwrite=TRUE)

# plot(NSW_forten18_ForTen)
# plot(NSW_forten18_ForType)


