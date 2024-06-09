# Purpose: To process land tenure data 

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
library(foreign)
library(ggpubr)

# load raster template

Woody <- rast("Input/woody_nsw.tif")
Woody_template <- rast("Input/Woody_template.tif")

NSW_vect <- vect("Input/STE_2021_AUST_SHP_GDA2020/STE_2021_AUST_GDA2020.shp") %>% 
  tidyterra::filter(STE_NAME21 == "New South Wales")

#Load land tenure 

AusLandTen <- rast("Input/land_tenure_of_australia_2010_11_to_2015_16_20210929/AUSTEN_250m_2015_16_alb/AUSTEN_250m_2015_16/AUSTEN_250m_2015_16_alb.tif")
NPWS_Land <- vect("Input/npws_allmanagedland/NPWS_AllManagedLand.shp")

NSW_vect_Albers <- project(NSW_vect, crs(AusLandTen))

NPWS_Land_df <- as.data.frame(NPWS_Land)
NPWS_Land_df %>% distinct(IUCN , TENURETYPE)
plot(NPWS_Land)

NSWLandTen <- crop(AusLandTen, NSW_vect_Albers, snap = "out", mask = TRUE) %>% 
  project(crs(Woody))

NSWLandTen_lut <- as.data.frame(cats(NSWLandTen))[,c(4:5)] %>% 
  distinct()
write.csv(NSWLandTen_lut, "Output/NSWLandTen_lut.csv")

NSWLandTen_L2 <- catalyze(NSWLandTen) %>% 
  subset("L2N") %>% 
  resample(y = Woody, thread = TRUE) %>% 
  crop(Woody, snap = "out", mask = TRUE)

names(NSWLandTen_L2) <- "LandTen"

writeRaster(NSWLandTen_L2, "Output/Raster/LandTen.tif", overwrite = TRUE)

LandTen <- rast("Output/Raster/LandTen.tif")

plot(LandTen)
