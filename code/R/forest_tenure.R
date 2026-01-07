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
library(foreign)

# load data
INPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Input"
OUTPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Output"
# load woody raster as template
Woody <- rast(file.path(INPUT_DIR, "woody_nsw.tif"))
Woody_template <- rast(file.path(INPUT_DIR, "Woody_template.tif"))

Aus_forten18 <- rast(file.path(INPUT_DIR, "aus_forten18_geotiff/aus_forten18.tif"))
names(Aus_forten18)

# load forest tenure data
forest_tenure_dbf <- read.dbf(file.path(INPUT_DIR, "aus_forten18_geotiff/aus_forten18.tif.vat.dbf"))

# NFI forest type
FOR_TYPE_LUT <- forest_tenure_dbf %>% 
  select(FOR_TYPE) %>% 
  distinct() %>% arrange(FOR_TYPE) %>% 
  mutate(FOR_TYPE1 = case_when(
    str_detect(FOR_TYPE, "Eucalypt") ~ "Eucalypt",
    str_detect(FOR_TYPE, "plantation") ~ "plantation",
    .default = FOR_TYPE
  ),
  FOR_TYPE1_Code = as.integer(factor(FOR_TYPE1, levels = unique(FOR_TYPE1[FOR_TYPE1 != "Non forest"]))),
  FOR_TYPE1_Code = if_else(FOR_TYPE1 == "Non forest", 0, FOR_TYPE1_Code))
sort(unique(FOR_TYPE_LUT$FOR_TYPE1_Code))

# NFI forest tenure type/class. Shows tenure for both forest and non-forest land
# Reclassify the forest tenure type to a binary classification.
# This is for downstream filtering SUs that are privately managed (LEASE and PRIV).
Ten_Type_LUT <- forest_tenure_dbf %>% 
  select(TEN_TYPE) %>% 
  distinct() %>% arrange(TEN_TYPE) %>% 
  mutate(TenType = case_when(TEN_TYPE %in% c("LEASE", "PRIV") ~ as.integer(1),
                             TEN_TYPE %in% c("MUF", "NCR", "ND", "OCL") ~ as.integer(0),
                             TEN_TYPE == "NULL" ~ NA_integer_))
sort(unique(FOR_TEN_LUT$FOR_TEN_CODE))
str(Ten_Type_LUT)
Ten_Type_LUT

# NFI forest tenure type/class. 
# Shows tenure of forest land only.
# forest tenure type combined (intersected) with the forest cover extent from the NFI Forests of Australia (2018) dataset. 
NSW_forten_LUT <- forest_tenure_dbf %>% 
  select(VALUE, STATE, FOR_TYPE, FOR_TEN, TEN_TYPE) %>% 
  left_join(FOR_TYPE_LUT, by = join_by("FOR_TYPE" == "FOR_TYPE")) %>%
  left_join(FOR_TEN_LUT, by = join_by("FOR_TEN" == "FOR_TEN")) %>% 
  left_join(Ten_Type_LUT, by = join_by("TEN_TYPE" == "TEN_TYPE")) %>%
  mutate(FOR_TYPE1_Code = if_else(STATE == "NSW", FOR_TYPE1_Code, NA),
         FOR_TEN_CODE = if_else(STATE == "NSW", FOR_TEN_CODE, NA),
         TenType = if_else(STATE == "NSW", TenType, NA))
# sort(unique(NSW_forten_LUT$FOR_TYPE1_Code))

write.csv(NSW_forten_LUT, file.path(INPUT_DIR, "aus_forten18_geotiff/NSW_forten18.csv", row.names = FALSE))

Aus_forten18_GDALamB <- project(Aus_forten18, crs(Woody), method = "mode", threads=TRUE)
levels(Aus_forten18_GDALamB) <- NULL

# NSW_forten18_ForCode <- classify(Aus_forten18_GDALamB, cbind(forest_tenure_dbf$VALUE, forest_tenure_dbf$FOR_CODE)) %>% 
#   crop(Woody, snap = "out") %>% 
#   resample(Woody, method = "mode", thread = TRUE) %>% 
#   mask(Woody)
# names(NSW_forten18_ForCode) <- "ForCode"
# writeRaster(NSW_forten18_ForCode, file.path(OUTPUT_DIR, "Raster/NSW_forten18_ForCode.tif"), overwrite=TRUE)

NSW_forten18_ForType <- classify(Aus_forten18_GDALamB, cbind(NSW_forten_LUT$VALUE, NSW_forten_LUT$FOR_TYPE1_Code)) %>% 
  crop(ext(Woody), snap = "out") %>% 
  resample(Woody, method = "mode", thread = TRUE) %>% 
  crop(Woody, mask = TRUE, snap = "out")

names(NSW_forten18_ForType) <- "ForType"
plot(NSW_forten18_ForType)
  # NSW_forten18_ForType <- ifel(not.na(NSW_forten18_ForType$ForType), NSW_forten18_ForType$ForType, Woody_template$EXT)
# names(NSW_forten18_ForType) <- "ForType"
writeRaster(NSW_forten18_ForType, file.path(OUTPUT_DIR, "Raster/NSW_forten18_ForType.tif"), overwrite=TRUE)

NSW_forten18_TenType <- classify(Aus_forten18_GDALamB, cbind(NSW_forten_LUT$VALUE, NSW_forten_LUT$TenType)) %>% 
  crop(ext(Woody), snap = "out") %>% 
  resample(Woody, method = "mode", thread = TRUE) %>% 
  crop(Woody, mask = TRUE, snap = "out")
plot(NSW_forten18_TenType)
names(NSW_forten18_TenType) <- "TenType"
unique(NSW_forten18_TenType$TenType)
# NSW_forten18_ForTen <- ifel(not.na(NSW_forten18_ForTen$ForTen),NSW_forten18_ForTen$ForTen, Woody_template$EXT)
# names(NSW_forten18_ForTen) <- "ForTen"
writeRaster(NSW_forten18_TenType, file.path(OUTPUT_DIR, "Raster/NSW_forten18_TenType.tif"), overwrite=TRUE)

NSW_forten18_ForTen <- classify(Aus_forten18_GDALamB, cbind(NSW_forten_LUT$VALUE, NSW_forten_LUT$FOR_TEN_CODE)) %>% 
  crop(ext(Woody), snap = "out") %>% 
  resample(Woody, method = "mode", thread = TRUE) %>% 
  crop(Woody, mask = TRUE, snap = "out")
plot(NSW_forten18_ForTen)
names(NSW_forten18_ForTen) <- "ForTen"
unique(NSW_forten18_ForTen$ForTen)
# NSW_forten18_ForTen <- ifel(not.na(NSW_forten18_ForTen$ForTen),NSW_forten18_ForTen$ForTen, Woody_template$EXT)
# names(NSW_forten18_ForTen) <- "ForTen"
writeRaster(NSW_forten18_ForTen, file.path(OUTPUT_DIR, "Raster/NSW_forten18_ForTen.tif"), overwrite=TRUE)

# plot(NSW_forten18_ForTen)
# plot(NSW_forten18_ForType)


