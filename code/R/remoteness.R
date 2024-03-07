# Remoteness

library(tidyverse)
library(terra)
library(sf)
library(stringr)

source('code/R/spatial_functions.R')

dirname <- "H://risk-model-covariates/code"

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
  shpToRast(name = "remoteness", field_name = "remoteness") %>%
  resampleRast(name = "remoteness") %>%
  clipRast(name = "remoteness", to_output = TRUE)