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
# load woody raster as template
Woody <- rast("Input/woody_nsw.tif")
Woody_template <- rast("Input/Woody_template.tif")

# Part 1: Distance to major and other uban area in SA1
SA1_UCL <- vect("Input/2016_GCP_SA1_for_NSW_short-header/ucl_2016_aust_shape/UCL_2016_AUST.shp") %>% 
  tidyterra::filter(SOS_NAME16 == "Major Urban" | SOS_NAME16 == "Other Urban") %>% 
  # tidyterra::filter(STE_NAME16 == "New South Wales") %>%
  project(Woody)


#Test 
SA1_UCL_CC <- SA1_UCL %>% 
  tidyterra::filter(UCL_NAME16 == "Central Coast")
SA1_UCL2_ext <- ext(buffer(SA1_UCL_CC, width = 50000))
SA1_UCL2 <- crop(SA1_UCL, SA1_UCL2_ext)
Woody_template2 <- crop(Woody_template, SA1_UCL2_ext)
DistCity <- terra::distance(Woody_template2, SA1_UCL2)
ggplot()+ geom_spatraster(data = DistCity, aes(fill = layer)) + scale_fill_viridis_c()+ geom_spatvector(data=SA1_UCL2, aes(fill = NULL)) + theme_minimal()

# Woody_GDA94 <- project(Woody, crs(SA1_UCL), threads=TRUE, filename="Input/Woody_GDA94.tif")
# 

SA1_UCL_rast <- rasterize(SA1_UCL, Woody, field = "SSR_CODE16")

DistCity <- terra::distance(SA1_UCL_rast, filename = "Output/DistCity.tif", overwrite=TRUE)
ggplot()+ geom_spatraster(data = DistCity, aes(fill = SSR_CODE16 )) + scale_fill_viridis_c()+ geom_spatvector(data=SA1_UCL, aes(fill = NULL)) + theme_minimal()

# Crop the extent to 1.5 times NSW state extent to reduce the processing time but capture city outside NSW
SA_ext <- ext(Woody)

New_ext <- ext(
  SA_ext[1] - (SA_ext[2] - ((SA_ext[1] + SA_ext[2])/2)),
  SA_ext[2], # no need to extent xmax into towards the sea
  SA_ext[3] - abs(SA_ext[4] - ((SA_ext[3] + SA_ext[4])/2)),
  SA_ext[4] + abs(SA_ext[4] - ((SA_ext[3] + SA_ext[4])/2)))

SA1_UCL_NSWex <- crop(SA1_UCL, SA_ext)
SA1_UCL_NSWex_r <- rasterize(SA1_UCL_NSWex, Woody, field = "SSR_CODE16")
DistCityEx <- terra::distance(SA1_UCL_NSWex_r, filename = "Output/DistCityEx.tif", overwrite=TRUE)
ggplot()+ geom_spatraster(data = DistCityEx, aes(fill = SSR_CODE16 )) + scale_fill_viridis_c()+ geom_spatvector(data=SA1_UCL_NSWex, aes(fill = NULL)) + theme_minimal()
names(DistCity) <- "DistCity"

DistCity <- crop(DistCityEx, Woody, snap = "out", mask = TRUE, filename = "Output/Raster/DistCity.tif", overwrite=TRUE)

DistCity <- rast("Output/Raster/DistCity.tif")
# Part2: Distance to road ----

# Load road data

st_layers("Input/National_Roads_Apr24/National_Roads_Apr24.gdb/")

RoadNSW <- vect("Input/RoadSegment_EPSG4283/RoadSegment_EPSG4283.gdb/", layer = "RoadSegment") 
names(RoadNSW)
RoadNSW_type1 <- RoadNSW %>% 
  tidyterra::filter(surface == "1")

RoadAus <- vect("Input/National_Roads_Apr24/National_Roads_Apr24.gdb/", layer = "National_Roads")

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

RoadNSW_1.5x <- RoadNSW_1.5x_all %>% 
  tidyterra::filter(hierarchy == "ACCESS ROAD" | hierarchy == "ARTERIAL ROAD" | hierarchy == "COLLECTOR ROAD" | 
                      hierarchy == "NATIONAL OR STATE HIGHWAY" | hierarchy == "SUB-ARTERIAL ROAD" | hierarchy == "VEHICLE TRACK") %>% 
  tidyterra::filter(status == "OPERATIONAL") %>% 
  tidyterra::filter(surface == "SEALED") %>% 
  tidyterra::mutate(ForRast = as.integer(1))

writeVector(RoadNSW_1.5x, "Output/Shapefile/RoadNSW_1.5x.shp", overwrite=TRUE)
RoadNSW_1.5x <- vect("Output/Shapefile/RoadNSW_1.5x.shp")

RoadNSW_1.5x_rast <- rasterize(RoadNSW_1.5x, Woody, field = "ForRast", fun = "max")

DistRoadEx <- terra::distance(RoadNSW_1.5x_rast, filename = "Output/DistRoadEx.tif", overwrite=TRUE)

names(DistRoad) <- "DistRoad"

DistRoad <- crop(DistRoadEx, Woody, snap = "out", mask = TRUE, filename = "Output/DistRoad.tif", overwrite=TRUE)
