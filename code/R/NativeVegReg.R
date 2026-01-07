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
library(exactextractr)

# load data
INPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Input"
OUTPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Output"
# load woody raster as template
Woody <- rast(file.path(INPUT_DIR, "woody_nsw.tif"))
Woody_template <- rast(file.path(INPUT_DIR, "Woody_template.tif"))
# plot(Woody)
# Lots <- st_read("D:/Data/NSW_Deforestation/risk-model-covariates/Input/LLS_properties/Treated_proper_Export.shp")

# load boundary

KMRs <- vect(file.path(INPUT_DIR, "biodiversity_nsw_koala_modelling_regions_v1p1/NSW_Koala_Modelling_Regions_v1.1.shp")) %>%  
  aggregate()
# plot(KMRs)
# ext(KMRs)
NSWbound <- vect(ext(KMRs), crs = crs(KMRs))
# plot(NSWbound)

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

# Version: 20240514----
naluma_nsw_2017_abkl0 <- rast(file.path(INPUT_DIR, "native_vegetation_regulatory_map_datapack_version11_20240522/naluma_nsw_2017_abkl0_c20240514_u11.tif"))

## naluma_nsw_2017_abel0 layer contains 1 map display class
### 5 Land excluded from the LLS Act
naluma_nsw_2017_abel0 <- rast(file.path(INPUT_DIR, "native_vegetation_regulatory_map_datapack_version11_20240522/naluma_nsw_2017_abel0_c20240514_u11.tif"))

# Version: 20221212----
## naluma_nsw_2017_abel0 layer contains 1 map display class
naluma_nsw_2017_abkl0 <- rast(file.path(INPUT_DIR, "native_vegetation_regulatory_map_datapack_version4_20221212/naluma_nsw_2017_abkl0_c20221212_u9.tif")) 
### 5 Land excluded from the LLS Act
naluma_nsw_2017_abel0 <- rast(file.path(INPUT_DIR, "native_vegetation_regulatory_map_datapack_version4_20221212/naluma_nsw_2017_abel0_c20221212_u9.tif"))

Woody_NVR <- project(Woody, crs(naluma_nsw_2017_abkl0), thread = TRUE)

NVR_Cat2_pNVR <- resample(naluma_nsw_2017_abkl0, Woody_NVR, method = "mode", threads = TRUE)
NVR_Excluded_pNVR <- resample(naluma_nsw_2017_abel0, Woody_NVR, method = "mode", threads = TRUE)

NVR_2lyr_pNVR <- rast(list(NVR_Cat2_pNVR, NVR_Excluded_pNVR))
names(NVR_2lyr_pNVR) <- c("Cat2", "Excluded")

NVR_pNVR <- ifel(not.na(NVR_2lyr_pNVR$Excluded), NVR_2lyr_pNVR$Excluded, NVR_2lyr_pNVR$Cat2)
NVR <- project(NVR_pNVR, crs(Woody), thread = TRUE)
unique(NVR)
  
NatVegReg <- ifel(not.na(NVR$Cat2), NVR$Cat2, Woody_template$EXT)
names(NatVegReg) <- "NatVegReg"
NatVegReg <- classify(NatVegReg, cbind(c(0,1,3,4,6), c(1,0,2,2,2)))
plot(NatVegReg)
NatVegReg <- as.factor(NatVegReg)
writeRaster(NatVegReg, file.path(OUTPUT_DIR, "Raster/NatVegReg.tif"), overwrite = TRUE)

# Extract values
Lots_NVR <- exact_extract(NatVegReg, Lots, fun = "frac", max_cells_in_memory = 3.5e+08)
Lots_Woody <- exact_extract(Woody, Lots, fun = "sum", max_cells_in_memory = 3.5e+08)

Lots_NVR_sf <- cbind(Lots, Woody = Lots_Woody, Lots_NVR) %>% 
  mutate(across(c(Woody, starts_with("frac")), \(x) round(x, 1))) %>% 
  select(cat_label, Woody, frac_1, frac_3, frac_4, frac_6, frac_9999)

Lots_NVR_sf_1 <- cbind(Lots, Woody = Lots_Woody, Lots_NVR) %>% 
  select(cat_label, Woody, frac_1, frac_3, frac_4, frac_6, frac_9999) %>% 
  mutate(across(c(Woody, starts_with("frac")), \(x) round(x, 1))) %>% 
  # filter(cat_label == "no_veg") %>% 
  st_drop_geometry(Lots_NVR_sf) %>% unique(.)

Lots_NVR_sf_2 <- Lots_NVR_sf %>% 
  mutate(NVR = case_when(Woody == 0 ~ "no_veg",
                         Woody > 0 & sum(frac1, frac_9999) == 1 ~ "onlynon"
                         Woody > 0 & sum(frac_3, frac_4, frac_6) > 0  & sum(frac1, frac_9999) == 0 ~ "onlyreg",
                         Woody > 0 & sum(frac_3, frac_4, frac_6) == 0 ~ "onlynon",
                         Woody > 0 & sum(frac_3, frac_4, frac_6) > 0 & frac_1 >0 ~ "mixed"
  ))


# This part is not used for this analysis but kept for record ####
##########################################################################################################################################
# Part 2----
# Purpose: to process the draft native vegetation# Purpose: To process native vegetation regulation data 

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

# Load Libraries
library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(stringr)
library(qs)
library(exactextractr)

# load data
INPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Input"
OUTPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Output"
# load woody raster as template
Woody <- rast(file.path(INPUT_DIR, "woody_nsw.tif"))
Woody_template <- rast(file.path(INPUT_DIR, "Woody_template.tif"))

NVR <- rast(file.path(INPUT_DIR , "naluma_nsw_2017_abpl0_c20240321_u11/naluma_nsw_2017_abpl0_c20240321_u11.tif"))

Woody_NVR <- project(Woody, crs(NatVegReg_DraftMap), thread = TRUE)

NVR_pNVR <- resample(NatVegReg_DraftMap, Woody_NVR, method = "mode", threads = TRUE)

NVR <- project(NVR_pNVR, crs(Woody), method="near", thread = TRUE) %>% 
  crop(Woody, snap = "out", mask = TRUE)

unique(NVR)

NVR <- classify(NVR, cbind(c(1,2,3,4,5,6), c(1,2,3,3,0,3)))
plot(NVR)
names(NVR) <- "NVR"
writeRaster(NVR, file.path(OUTPUT_DIR, "Raster/NVR.tif"), overwrite = TRUE)
