library(tidyverse)
library(terra)
library(sf)
library(foreign)

source('code/R/spatial_functions.R')

dirname <- "H://risk-model-covariates/code"

prop_value_ha <- file.path(dirname, "../data/prop_size_value/Prop_value_ha_20230101.dbf") %>%
  read.dbf()

properties <- file.path(dirname, "../data/prop_size_value/Property_EPSG4283.gdb") %>%
  st_read(layer="Property")

properties_joined <- properties %>%
  left_join(prop_value_ha, by = join_by("propid"=="PROPERTY_I"))

output_value <- properties_joined %>%
  projectShp(name = "prop_value") %>%
  shpToRast(name = "prop_value", overwrite=T, field_name = "value_ha") %>%
  resampleRast(name = "prop_value", overwrite=T) %>%
  clipRast(name = "prop_value", overwrite=T, to_output = TRUE)

output_size <- properties_joined %>%
  projectShp(name = "prop_size") %>%
  shpToRast(name = "prop_size", overwrite=T, field_name = "area_ha") %>%
  resampleRast(name = "prop_size", overwrite=T) %>%
  clipRast(name = "prop_size", overwrite=T, to_output = TRUE)