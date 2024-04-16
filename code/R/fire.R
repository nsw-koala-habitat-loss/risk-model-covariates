library(tidyverse)
library(terra)
library(sf)
library(stringr)

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