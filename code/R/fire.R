library(tidyverse)
library(terra)
library(sf)
library(stringr)
library(tidyterra)

source('code/R/spatial_functions.R')

dirname <- "H://risk-model-covariates/code"

## Landuse shp
nsw_fire <- file.path(dirname, "../data/fire/fire_npwsfirehistory/NPWSFireHistory.shp")
nsw_fire_shp <- st_read(nsw_fire)

years <- 2011:2019

# Select fires within the period of analysis
nsw_fire_cleaned <- nsw_fire_shp %>%
  mutate(StartYear = substr(StartDate, 0, 4), EndYear = substr(EndDate, 0, 4)) %>%
  filter(StartYear %in% years | EndYear %in% years) %>%
  mutate(FireType = ifelse(str_detect(Label, "Prescribed Burn"), 1, 
                           ifelse(str_detect(Label, "Wildfire"), 2, 0))) %>%
  select(FireType)
cleaned_file <- file.path(dirname, '../intermediate_data/nsw_fire_cleaned.shp')
st_write(nsw_fire_cleaned, cleaned_file, append=F)

output <- cleaned_file %>%
  projectShp(name = "fire") %>%
  shpToRast(name = "fire", field_name = "FireType", overwrite=T) %>%
  resampleRast(name = "fire", overwrite=T) %>%
  clipRast(name = "fire", to_output = TRUE, overwrite=T)

### Re-process to include no data area.
library(terra)
library(tidyterra)

INPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Input"
OUTPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Output"

Woody_template <- rast(file.path(INPUT_DIR, "Woody_template.tif"))

NSW_Fire <- vect(file.path(INPUT_DIR, "/fire_npwsfirehistory/NPWSFireHistory.shp")) %>% 
  tidyterra::mutate(StartYear = substr(StartDate, 0, 4), EndYear = substr(EndDate, 0, 4)) %>%
  filter(StartYear %in% 2011:2019 | EndYear %in% 2011:2019) %>%
  mutate(Fire = ifelse(str_detect(Label, "Prescribed Burn") | str_detect(Label, "Wildfire"), 1, NA)) %>% 
  select(Fire)
ggplot()+geom_spatvector(data = NSW_Fire, aes(fill = Fire)) + theme_minimal()
writeVector(NSW_Fire, file.path(OUTPUT_DIR, "Shapefile/fire.shp"), overwrite = TRUE)

NSW_Fire <- vect(file.path(OUTPUT_DIR, "Shapefile/fire.shp")) %>% 
  project(crs(Woody_template)) %>% 
  crop(Woody_template) %>% 
  rasterize(Woody_template, fun = "max", field = "Fire") %>%
  resample(Woody_template, method = "mode") %>% 
  crop(Woody_template, snap = "out", mask = TRUE)

NSW_Fire <- ifel(not.na(NSW_Fire$Fire), NSW_Fire$Fire, Woody_template$EXT)
names(NSW_Fire) <- "Fire"
NSW_Fire <- as.factor(NSW_Fire)
writeRaster(NSW_Fire, file.path(OUTPUT_DIR, "/Raster/Fire.tif"), overwrite = TRUE)
plot(NSW_Fire)
