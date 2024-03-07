library(tidyverse)
library(terra)
library(sf)
library(stringr)

source('code/R/spatial_functions.R')

dirname <- "H://risk-model-covariates/code"

## Income shp
census_path <- file.path(dirname, "../data/pop_den/2016_GCP_SA1_for_NSW_short-header/2016 Census GCP Statistical Area 1 for NSW/2016Census_G02_NSW_SA1.csv")
sa1 <-  file.path(dirname, "../data/pop_den/2016_SA1_shape/SA1_2016_AUST.shp")

# Get population density by dividing number of people and SA1 area
sa1_shp <- st_read(sa1)
census <- read_csv(census_path)
income_classified <- sa1_shp %>%
  mutate(SA1_7DIGIT = as.numeric(SA1_7DIGIT)) %>%
  right_join(census, by = join_by("SA1_7DIGIT" == "SA1_7DIGITCODE_2016")) %>%
  mutate(income = Median_tot_hhd_inc_weekly) %>%
  select(income)
income_shp_path <- file.path(dirname, "../intermediate_data/income_classified.shp")
st_write(income_classified, income_shp_path, append=F)

output <- income_shp_path %>%
  projectShp(name = "income") %>%
  shpToRast(name = "income", field_name = "income", overwrite = T) %>%
  resampleRast(name = "income", overwrite = T) %>%
  clipRast(name = "income", to_output = TRUE, overwrite = T)
