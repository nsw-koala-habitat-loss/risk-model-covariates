# Purpose: To prduce distance to road and city layer

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

# Load Libraries
library(tidyverse)
library(tidyterra)
library(terra)
library(sf)
library(stringr)
library(qs)
library(exactextractr)


# load data
## Define directories
INPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Input"
OUTPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Output"
## load woody raster as template
Woody <- rast(file.path(INPUT_DIR, "woody_nsw.tif"))
Woody_template <- rast(file.path(INPUT_DIR, "Woody_template.tif"))

##########################################################################################################################################
# Part 1: Distance to Urban Centre and Locality (UCL) based on ABS 2016 SA1 ----
SA1_UCL <- vect(file.path(INPUT_DIR, "2016_GCP_SA1_for_NSW_short-header/ucl_2016_aust_shape/UCL_2016_AUST.shp")) %>% 
  tidyterra::filter(SOS_NAME16 == "Major Urban" | SOS_NAME16 == "Other Urban") %>% 
  project(Woody)

# Crop the extent to 1.5 times NSW state extent to reduce the processing time but capture city outside NSW
SA_ext <- ext(Woody)

New_ext <- ext(
  SA_ext[1] - (SA_ext[2] - ((SA_ext[1] + SA_ext[2])/2)),
  SA_ext[2], # no need to extent xmax into towards the sea
  SA_ext[3] - abs(SA_ext[4] - ((SA_ext[3] + SA_ext[4])/2)),
  SA_ext[4] + abs(SA_ext[4] - ((SA_ext[3] + SA_ext[4])/2)))
SA1_UCL_NSWex <- crop(SA1_UCL, SA_ext)

# Rasterised UCL layer for 1.5x NSW extent
SA1_UCL_NSWex_r <- rasterize(SA1_UCL_NSWex, Woody, field = "SSR_CODE16")

# Calculate distance to UCL for 1.5x NSW extent
DistCityEx <- terra::distance(SA1_UCL_NSWex_r, filename = file.path(OUTPUT_DIR, "DistCityEx.tif"), overwrite=TRUE)

# Chck the distance raster
# ggplot()+ geom_spatraster(data = DistCityEx, aes(fill = SSR_CODE16 )) + scale_fill_viridis_c()+ geom_spatvector(data=SA1_UCL_NSWex, aes(fill = NULL)) + theme_minimal()
names(DistCity) <- "DistCity"

# Crop distance to UCL to NSW extent
DistCity <- crop(DistCityEx, Woody, snap = "out", mask = TRUE, filename = file.path(OUTPUT_DIR, "Raster/DistCity.tif"), overwrite=TRUE)

# DistCity <- rast(file.path(OUTPUT_DIR, "Raster/DistCity.tif"))

##########################################################################################################################################
# Part2: Distance to road ----

# Load road data

st_layers(file.path(INPUT_DIR, "National_Roads_Apr24/National_Roads_Apr24.gdb/"))

# Load NSW road data (this is not used, use the australian national road data instead)
RoadNSW <- vect(file.path(INPUT_DIR, "RoadSegment_EPSG4283/RoadSegment_EPSG4283.gdb/"), layer = "RoadSegment") 
RoadNSW_type1 <- RoadNSW %>% tidyterra::filter(surface == "1")

RoadAus <- vect(file.path(INPUT_DIR, "National_Roads_Apr24/National_Roads_Apr24.gdb/"), layer = "National_Roads")

# Crop the extent to 1.5 times NSW state extent to reduce the processing time but capture road outside NSW
Woody_GDA20 <- project(Woody, crs(RoadAus), threads=TRUE)
SA_ext <- ext(Woody_GDA20)

New_ext <- ext(
  SA_ext[1] - (SA_ext[2] - ((SA_ext[1] + SA_ext[2])/2)),
  SA_ext[2], # no need to extent xmax into towards the sea
  SA_ext[3] - abs(SA_ext[4] - ((SA_ext[3] + SA_ext[4])/2)),
  SA_ext[4] + abs(SA_ext[4] - ((SA_ext[3] + SA_ext[4])/2)))


RoadNSW_1.5x_all <- terra::crop(RoadAus, New_ext) %>% 
  project(crs(Woody))

# Select major roads that is still operational and sealed
RoadNSW_1.5x <- RoadNSW_1.5x_all %>% 
  tidyterra::filter(hierarchy == "ACCESS ROAD" | hierarchy == "ARTERIAL ROAD" | hierarchy == "COLLECTOR ROAD" | 
                      hierarchy == "NATIONAL OR STATE HIGHWAY" | hierarchy == "SUB-ARTERIAL ROAD" | hierarchy == "VEHICLE TRACK") %>% 
  tidyterra::filter(status == "OPERATIONAL") %>% 
  tidyterra::filter(surface == "SEALED") %>% 
  tidyterra::mutate(ForRast = as.integer(1))

# Export the road shapefile for 1.5x NSW extent
writeVector(RoadNSW_1.5x, file.path(OUTPUT_DIR, "Shapefile/RoadNSW_1.5x.shp"), overwrite=TRUE)
RoadNSW_1.5x <- vect(file.path(OUTPUT_DIR, "Shapefile/RoadNSW_1.5x.shp"))

# Rasterised road layer for 1.5x NSW extent
RoadNSW_1.5x_rast <- rasterize(RoadNSW_1.5x, Woody, field = "ForRast", fun = "max")

# Calculate distance to road for 1.5x NSW extent
DistRoadEx <- terra::distance(RoadNSW_1.5x_rast, filename = file.path(OUTPUT_DIR, "DistRoadEx.tif"), overwrite=TRUE)

names(DistRoad) <- "DistRoad"

DistRoad <- crop(DistRoadEx, Woody, snap = "out", mask = TRUE, filename = file.path(OUTPUT_DIR, "DistRoad.tif"), overwrite=TRUE)

##################################################################################################################################################