library(tidyverse)
library(terra)
library(sf)
library(stringr)

source('code/R/spatial_functions.R')

dirname <- "H://risk-model-covariates/code"

## Income shp
census_path <- file.path(dirname, "../data/pop_den/2016_GCP_SA1_for_NSW_short-header/2016 Census GCP Statistical Area 1 for NSW/2016Census_G01_NSW_SA1.csv")
sa1 <-  file.path(dirname, "../data/pop_den/2016_SA1_shape/SA1_2016_AUST.shp")

# Get population density by dividing number of people and SA1 area
sa1_shp <- st_read(sa1)
census <- read_csv(census_path)
pop_den <- sa1_shp %>%
  mutate(SA1_7DIGIT = as.numeric(SA1_7DIGIT)) %>%
  right_join(census, by = join_by("SA1_7DIGIT" == "SA1_7DIGITCODE_2016")) %>%
  mutate(pop_den = Tot_P_P / AREA_SQKM) %>%
  select(pop_den)
pop_den_path <- file.path(dirname, "../intermediate_data/pop_den_classified.shp")
st_write(pop_den, pop_den_path, append=F)

output <- pop_den_path %>%
  projectShp(name = "pop_den") %>%
  shpToRast(name = "pop_den", field_name = "pop_den", overwrite = T) %>%
  resampleRast(name = "pop_den", overwrite = T) %>%
  clipRast(name = "pop_den", to_output = TRUE, overwrite = T)
