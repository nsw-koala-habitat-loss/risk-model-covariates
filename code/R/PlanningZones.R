# Purpose: To generate planning zone

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

# Load Libraries
library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(stringr)
library(qs)
library(readxl)

# load data
# load woody raster as template
Woody <- rast("Input/woody_nsw.tif")
Woody_template <- rast("Input/Woody_template.tif")

# load recode look up
LandZone_code <- read_xlsx("covariate_description.xlsx", sheet = "LandZone")

Cat2Num <- read_xlsx("covariate_description.xlsx", sheet = "Cat2Num")

# load EPI Land zoning data
EPI_lyr <- st_layers("Input/NSW_EPI_land_zones/All_EPI_Data_Geodatabase_GDA94_22122022/EnvironmentalPlanningInstruments.gdb/")

LandZone <- vect("Input/NSW_EPI_land_zones/All_EPI_Data_Shapefile_GDA94_22122022/EPI_Land_Zoning.shp")%>%
  project(crs(Woody)) %>% 
  tidyterra::left_join(LandZone_code, by = join_by("SYM_CODE" == "Abbreviation")) %>%
  tidyterra::filter(LandZone1 != "REMOVED_LORD HOWE", LandZone1 != "REMOVED_Overlapped", LGA_NAME != "LORD HOWE ISLAND - UNINCORPORATED AREA") %>%
  tidyterra::mutate(LandZone1 = case_when(LAY_CLASS == "Business Zone - Commercial Core" ~ "B3", # Business Zone - Commercial Core
                                          LAY_CLASS == "Environment" ~ "OTHERS",
                                          .default = as.character(SYM_CODE))) %>% 
  tidyterra::left_join(Cat2Num, by = join_by("LandZone2" == "Character")) %>% 
  tidyterra::mutate(Number = as.numeric(Number)) %>% 
  tidyterra::select(-c(Remarks, Cov_Catogory, 'Covariate Code', 'Covariate Description'))


# Potentially exclude SYM_CODE == "DR" (Drainage)
writeVector(LandZone, "Output/LandZone.shp", overwrite = TRUE)
LandZone <- vect("Output/LandZone.shp")


# rasterize
LandZone_rast <- rasterize(LandZone, Woody, field = "Number") %>% 
  crop(Woody, snap = "out", mask = TRUE)
LandZone_rast <- sum(LandZone_rast, Woody_template, na.rm = TRUE)
names(LandZone_rast) <- "LandZone2"

# export
writeRaster(LandZone_rast, "Output/LandZone.tif", overwrite = TRUE)

