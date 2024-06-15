library(tidyverse)
library(terra)
library(sf)

source('code/R/spatial_functions.R')

dirname <- "H://risk-model-covariates/code"

## Landuse shp
nsw_landuse_2013 <- file.path(dirname, "../data/landuse/landnswlanduse2013/Land_NSW_Landuse_2013/Data/Shapefile/NSW_Landuse_2013.shp")

# Superimpose Sydney region of 2017 to 2007 dataset
output <- nsw_landuse_2013 %>%
  projectShp(name = "landuse") %>%
  shpToRast(name = "landuse", field_name = "TertiaryAL", overwrite=T) %>%
  resampleRast(name = "landuse", overwrite=T) %>%
  clipRast(name = "landuse", overwrite=T, to_output = TRUE)


## Land Use 2017 ####
# Purpose: To produce land use data for 2017 datasets 

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
Woody <- rast("D:/Data/NSW_Deforestation/risk-model-covariates/Input/woody_nsw.tif")
Woody_template <- rast("D:/Data/NSW_Deforestation/risk-model-covariates/Input/Woody_template.tif")

landuse2017 <- vect("D:/Data/NSW_Deforestation/risk-model-covariates/Input/land_nswlanduse2017v1p5/NSWLanduse2017_Ver1_5_20230921.shp") %>% 
  project(crs(Woody))

landuse2017 <- landuse2017 %>% 
  tidyterra::mutate(PrimaryC = as.factor(SecondaryA %/% 100))

# Generate a look up table for the landuse codes in NSW
landuse2017_lut <- as.data.frame(landuse2017) %>%
  select(SecondaryCode = SecondaryA ,SecondaryDes = Secondary, TertiaryCode = TertiaryAL , TertiaryDes = Tertiary)%>% 
  distinct() %>% 
  arrange(TertiaryCode) %>% 
  mutate(Retain = if_else(SecondaryCode %in% c(110, 120, 130, 510, 520, 530, 540, 550, 560, 570, 580, 590, 610, 620, 630, 640, 650, 660), 0, 1))
write.csv(landuse2017_lut, "Output/landuse2017_lut.csv")

# landuse2017_sf <- st_as_sf(landuse2017) %>% 
#   select(SecondaryCode = SecondaryA) %>% 
#   mutate(SecondaryCode = as.factor(SecondaryCode))

landuse2017_r <- terra::rasterize(landuse2017, Woody, field = "PrimaryC")
names(landuse2017_r) <- "LandUse"
plot(landuse2017_r)

names(landuse2017_r) <- "LandUse"
writeRaster(landuse2017_r, "Output/Raster/LandUse.tif", overwrite=TRUE)
