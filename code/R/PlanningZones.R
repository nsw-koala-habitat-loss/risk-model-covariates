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
PlanZone_code <- read_xlsx("covariate_description.xlsx", sheet = "PlanZone")

Cat2Num <- read_xlsx("covariate_description.xlsx", sheet = "Cat2Num")

# load EPI Land zoning data
EPI_lyr <- st_layers("Input/NSW_EPI_land_zones/All_EPI_Data_Geodatabase_GDA94_22122022/EnvironmentalPlanningInstruments.gdb/")

PlanZone <- vect("Input/NSW_EPI_land_zones/All_EPI_Data_Shapefile_GDA94_22122022/EPI_Land_Zoning.shp")%>%
  project(crs(Woody)) %>% 
  tidyterra::left_join(PlanZone_code, by = join_by("SYM_CODE" == "Abbreviation")) %>%
  tidyterra::filter(LandZone1 != "REMOVED_LORD HOWE", LandZone1 != "REMOVED_Overlapped", LGA_NAME != "LORD HOWE ISLAND - UNINCORPORATED AREA") %>%
  tidyterra::mutate(LandZone1 = case_when(LAY_CLASS == "Business Zone - Commercial Core" ~ "B3", # Business Zone - Commercial Core
                                          LAY_CLASS == "Environment" ~ "OTHERS",
                                          .default = as.character(SYM_CODE))) %>% 
  tidyterra::left_join(Cat2Num, by = join_by("PlanZone2" == "Character")) %>% 
  tidyterra::mutate(Code = as.numeric(Code)) %>% 
  tidyterra::select(-c(Remarks, Cov_Catogory, 'Covariate Code', 'Covariate Description'))


# Potentially exclude SYM_CODE == "DR" (Drainage)
writeVector(PlanZone, "Output/Shapefile/PlanZone.shp", overwrite = TRUE)
PlanZone <- vect("Output/Shapefile/PlanZone.shp")
names(PlanZone)

# rasterize
PlanZone_rast <- rasterize(PlanZone, Woody, field = "Code") %>% 
  crop(Woody, snap = "out", mask = TRUE)
PlanZone_rast <- ifel(not.na(PlanZone_rast$Code), PlanZone_rast$Code, Woody_template$EXT)
names(PlanZone_rast) <- "PlanZone"

# export
writeRaster(PlanZone_rast, "Output/Raster/PlanZone.tif", overwrite = TRUE)
PlanZone_rast <- rast("Output/Raster/PlanZone.tif")
PlanZone <- rast("Output/Raster/PlanZone.tif")
plot(PlanZone)
