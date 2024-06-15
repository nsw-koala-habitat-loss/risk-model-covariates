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

Woody <- rast("D:/Data/NSW_Deforestation/risk-model-covariates/Input/woody_nsw.tif")
Woody_template <- rast("D:/Data/NSW_Deforestation/risk-model-covariates/Input/Woody_template.tif")

NSW_vect <- vect("D:/Data/NSW_Deforestation/risk-model-covariates/Input/STE_2021_AUST_SHP_GDA2020/STE_2021_AUST_GDA2020.shp") %>% 
  tidyterra::filter(STE_NAME21 == "New South Wales")

#Load land tenure 

AusLandTen <- rast("D:/Data/NSW_Deforestation/risk-model-covariates/Input/land_tenure_of_australia_2010_11_to_2015_16_20210929/AUSTEN_250m_2015_16_alb/AUSTEN_250m_2015_16/AUSTEN_250m_2015_16_alb.tif")
# NPWS_Land <- vect("D:/Data/NSW_Deforestation/risk-model-covariates/Input/npws_allmanagedland/NPWS_AllManagedLand.shp")

AusLandTen <- project(AusLandTen, crs(Woody), method = "mode", thread = TRUE)

NPWS_Land_df <- as.data.frame(NPWS_Land)
NPWS_Land_df %>% distinct(IUCN , TENURETYPE)
plot(NPWS_Land)

NSWLandTen <- AusLandTen %>% 
  catalyze() %>% 
  subset("L2N") %>% 
  project(crs(Woody), method = "mode", thread = TRUE)%>% 
  resample(y = Woody, method = "mode", thread = TRUE) %>% 
  crop(Woody, snap = "out", mask = TRUE)

names(NSWLandTen) <- "LandTen"

NSWLandTen <- ifel(not.na(NSWLandTen$LandTen), NSWLandTen$LandTen, Woody_template$EXT)
NSWLandTen <- NSWLandTen %>% classify(cbind(c(0, 10, 21, 22,23), c(0, 1, 2, 3, 4)))
names(NSWLandTen) <- "LandTen"
plot(NSWLandTen)
writeRaster(NSWLandTen, "Output/Raster/LandTen.tif", overwrite = TRUE)

NSWLandTen <- rast("Output/Raster/LandTen.tif")

NSWLandTen_lut <- as.data.frame(cats(NSWLandTen))[,c(4:5)] %>% 
  distinct()
write.csv(NSWLandTen_lut, "Output/NSWLandTen_lut.csv")

plot(NSWLandTen)

