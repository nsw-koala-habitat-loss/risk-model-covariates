# Remoteness

library(tidyverse)
library(terra)
library(sf)
library(stringr)

source('code/R/spatial_functions.R')
source('code/R/parameters')

## Landuse shp
remoteness <- file.path(dirname, "../data/remoteness/RA_2021_AUST_GDA94/RA_2021_AUST_GDA94.shp")
remoteness_shp <- st_read(remoteness)
remoteness_classified <- remoteness_shp %>%
  filter(substr(as.character(RA_CODE21),0,1)==1) %>% # Filter only NSW
  mutate(remoteness = as.numeric(substr(as.character(RA_CODE21),2,2))) %>%
  select(remoteness)

remoteness_path <- file.path(dirname, "../intermediate_data/remoteness.shp")
st_write(remoteness_classified, remoteness_path, append=F)

output <- remoteness_path %>%
  projectShp(name = "remoteness") %>%
  shpToRast(name = "remoteness", field_name = "remoteness", overwrite=T) %>%
  resampleRast(name = "remoteness", overwrite=T) %>%
  clipRast(name = "remoteness", to_output = TRUE, overwrite=T)

#### Remoteness 2016 ####
## Updated to remoteness data for year 2016 ##
# Purpose: To process create a raster template.
# In the template, raster cell value with Woody data will be filled with zero(0) and the rest will be filled with NA.

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

# Load Libraries
library(terra)

# load data
# load woody raster as template
Woody <- rast("Input/woody_nsw.tif")
Woody_template <- ifel(not.na(Woody), 9999, NA)
names(Woody_template) <- "EXT"
writeRaster(Woody_template, "Input/Woody_template.tif", overwrite = TRUE)
