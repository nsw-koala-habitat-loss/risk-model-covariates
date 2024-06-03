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
NSWLandTen_cats <- as.data.frame(cats(NSWLandTen))[,c(1,4)]
NSWLandTen_L2 <-  categories(NSWLandTen, layer =1 , NSWLandTen_cats, active = 1) %>% 
  resample(y = Woody, thread = TRUE) %>% 
  crop(Woody, snap = "out", mask = TRUE)
writeRaster(NSWLandTen_L2, "Output/Raster/NSWLandTen.tif", overwrite = TRUE)

NSWLandTen <- rast("Output/Raster/NSWLandTen.tif")

plot(NSWLandTen)
