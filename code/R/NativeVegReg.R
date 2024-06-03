# Purpose: To process native vegetation regulation data 

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

# Load Libraries
library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(stringr)
library(qs)

# load data
# load woody raster as template
Woody <- rast("Input/woody_nsw.tif")
Woody_template <- rast("Input/Woody_template.tif")

# load boundary

KMRs <- vect("Input/biodiversity_nsw_koala_modelling_regions_v1p1/NSW_Koala_Modelling_Regions_v1.1.shp") %>%  aggregate()
plot(KMRs)
ext(KMRs)
NSWbound <- vect(ext(KMRs), crs = crs(KMRs))
plot(NSWbound)

# load native vegetation regulation data

## Refer to 
### 1. Native_Vegetation_Regulatory_Map_Datapack_Version11_20240522.docx; 
### 2. https://www.lmbc.nsw.gov.au/Maps/index.html?viewer=NVRMap

## There are two categories that are not mapped on the transitional NVR map. These are:
### category 1-exempt land (as described in section 60H of the LLS Act), and
### category 2-regulated land (as described in section 60I of the LLS Act).

## naluma_nsw_2017_abkl0 layer contains 3 map display classes: 
### 3 category 2-vulnerable regulated land 
### 4 category 2-sensitive regulated land 
### 6 category 2-sensitive and vulnerable regulated lands areas of overlap
naluma_nsw_2017_abkl0 <- rast("Input/native_vegetation_regulatory_map_datapack_version11_20240522/naluma_nsw_2017_abkl0_c20240514_u11.tif")

## naluma_nsw_2017_abel0 layer contains 1 map display class
### 5 Land excluded from the LLS Act
naluma_nsw_2017_abel0 <- rast("Input/native_vegetation_regulatory_map_datapack_version11_20240522/naluma_nsw_2017_abel0_c20240514_u11.tif")

naluma_nsw_2017_rast <- rast(list(naluma_nsw_2017_abel0, naluma_nsw_2017_abkl0))
names(naluma_nsw_2017_rast) <- c("Excluded" , "Cat2")

NatVegReg_rast <- ifel(not.na(naluma_nsw_2017_rast$Excluded), naluma_nsw_2017_rast$Excluded, naluma_nsw_2017_rast$Cat2)

NatVegReg_rast <- resample(NatVegReg_rast, Woody, method = "mode", threads = TRUE) %>% 
  crop(Woody,snap="out", mask = TRUE, filename = "Output/NatVegReg.tif", overwrite = TRUE)

NatVegReg_rast <- sum(NatVegReg_rast$Cat2, Woody_template$EXT, na.rm = TRUE)
writeRaster(NatVegReg_rast, "Output/NatVegReg.tif", overwrite = TRUE)
