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
INPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Input"
OUTPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Output"

Woody <- rast(file.path(INPUT_DIR, "woody_nsw.tif"))
Woody_template <- rast(file.path(INPUT_DIR, "Woody_template.tif"))

NSW_vect <- vect(file.path(INPUT_DIR, "STE_2021_AUST_SHP_GDA2020/STE_2021_AUST_GDA2020.shp")) %>% 
  tidyterra::filter(STE_NAME21 == "New South Wales")

#Load land tenure 

AusLandTen <- rast(file.path(INPUT_DIR, "land_tenure_of_australia_2010_11_to_2015_16_20210929/AUSTEN_250m_2015_16_alb/AUSTEN_250m_2015_16/AUSTEN_250m_2015_16_alb.tif"))
# NPWS_Land <- vect((file.path(INPUT_DIR, "npws_allmanagedland/NPWS_AllManagedLand.shp")

# AusLandTen <- project(AusLandTen, crs(Woody), method = "mode", thread = TRUE)

# NPWS_Land_df <- as.data.frame(NPWS_Land)
# NPWS_Land_df %>% distinct(IUCN , TENURETYPE)
# plot(NPWS_Land)

NSWLandTen <- AusLandTen %>% 
  catalyze() %>% 
  subset("L2N") %>% 
  project(crs(Woody), method = "mode", thread = TRUE)%>% 
  resample(y = Woody, method = "mode", thread = TRUE) %>% 
  crop(Woody, snap = "out", mask = TRUE)

names(NSWLandTen) <- "LandTen"
NSWLandTen[NSWLandTen == 0] <- NA
plot(NSWLandTen)

# NSWLandTen <- ifel(NSWLandTen$LandTen == 0, NSWLandTen$LandTen, Woody_template$EXT)

NSWLandTen <- NSWLandTen %>% classify(cbind(c(10, 21, 22,23), c(1, 2, 3, 4)))
NSWLandTen <- as.factor(NSWLandTen)
names(NSWLandTen) <- "LandTen"
plot(NSWLandTen)
writeRaster(NSWLandTen, file.path(OUTPUT_DIR, "Raster/LandTen.tif"), overwrite = TRUE)

NSWLandTen <- rast(file.path(OUTPUT_DIR, "Raster/LandTen.tif"))

# plot(NSWLandTen)

