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
# Use the NSW specific land tenure data
st_layers(file.path(INPUT_DIR,"nswlandtenure_dec2024_v2_seed.gdb"))
NSWTEN <- st_read(file.path(INPUT_DIR,"nswlandtenure_dec2024_v2_seed.gdb"), layer = "NSW_LandTenure_DPI2024_v02")
unique(NSWTEN$TenureClass)
NSWTEN_2 <- NSWTEN %>% 
  mutate(LandTen = case_when(TenureClass == "Private" ~ 1,
                             TenureClass == "Crownland-Leasehold" ~ 2,
                             TenureClass == "Crownland-Other" ~ 4,
                             TenureClass == "Indigenous Owned" ~ 4,
                             TenureClass == "National Park" ~ 3,
                             TenureClass == "State Forest" ~ 3,
                            .default = NA))

NSWTEN_vect <- vect(NSWTEN_2)
NSWTEN_rast <- rasterize(NSWTEN_vect, Woody, field = "LandTen", fun = "min") 

plot(NSWTEN_rast)

writeRaster(NSWTEN_rast, file.path(OUTPUT_DIR, "Raster/NSW_LandTenure.tif"), overwrite = TRUE)

