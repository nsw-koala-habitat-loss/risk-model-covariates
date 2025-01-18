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
library(tidyverse)
library(tidyterra)
library(terra)
library(sf)
library(stringr)
library(qs)
library(Rcpp)
library(exactextractr)
library(factoextra)
library(ggpubr)

# load data
## Define directories
INPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Input"
OUTPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Output"
## load woody raster as template
Woody <- rast(file.path(INPUT_DIR, "woody_nsw.tif"))
Woody_template <- rast(file.path(INPUT_DIR, "Woody_template.tif"))

# load remoteness raster
Remoteness <- vect(file.path(INPUT_DIR, "ABS_ASGS_Volume 5_Remoteness Structure 2016/RA_2016_AUST.shp")) %>% 
  tidyterra::filter(STE_NAME16 == "New South Wales") %>% 
  project(crs(Woody)) %>% 
  crop(Woody) %>% 
  tidyterra::mutate(Remoteness = as.integer(substr(RA_CODE16, 2, 2))) %>% 
  tidyterra::select(Remoteness, RemoteDcpt = RA_NAME16)

writeVector(Remoteness, file.path(OUTPUT_DIR, "Shapefile/remote2016.shp"), overwrite = TRUE)
Remoteness_LUT <- as.data.frame(Remoteness)
write.csv(Remoteness_LUT, file.path(INPUT_DIR, "remote2016_LUT.csv"), row.names = FALSE)

ggplot()+geom_spatvector(data = Remoteness, aes(fill = Remoteness))+theme_minimal()

Remoteness_rast <- rasterize(Remoteness, Woody_template, field = "Remoteness") %>% 
  crop(Woody, snap = "out", mask = TRUE)
plot(Remoteness_rast)
writeRaster(Remoteness_rast, file.path(OUTPUT_DIR, "Raster/remote2016.tif"), overwrite = TRUE)
