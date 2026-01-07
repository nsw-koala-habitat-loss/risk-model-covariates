rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

# Load Libraries
library(tidyverse)
library(tidyterra)
library(terra)
library(sf)
library(qs)
library(tictoc)
library(exactextractr)
library(foreign)
options("max.print" = 999)
set.seed(2025)

# load raster template
INPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Input"
OUTPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Output"

Woody <- rast(file.path(INPUT_DIR, "woody_nsw.tif"))
Woody_template <- rast(file.path(INPUT_DIR, "Woody_template.tif"))

PropVal_DIR <- file.path(INPUT_DIR, "Property_values")

# Read in Property values for each NSW LGA
PropVal_RAW_CSVs <- list.files(file.path(PropVal_DIR, "LV_20230101"), pattern = "*.csv", full.names = TRUE)
PropVal_RAW_DF <- do.call("rbind", lapply(PropVal_RAW_CSVs, read.csv))

#' Filter useful columns and standardised area in hectares
PropVal_DF <- PropVal_RAW_DF %>% 
  select("PROPERTY.ID", "ZONE.CODE", "AREA", "AREA.TYPE", "LAND.VALUE.1") %>%
  mutate(AREA_HA = NA) %>%
  mutate(AREA_HA = ifelse(AREA.TYPE == "M", AREA / 10000, AREA_HA)) %>%
  mutate(AREA_HA = ifelse(AREA.TYPE == "H", AREA * 1, AREA_HA))

# Read in Property vector file from geodatabase
Prop_SF <- st_read(file.path(INPUT_DIR, "Property_values", "Property_EPSG4283.gdb"), layer = "Property")

#' Join property values with property vector file
Prop_Val_SF <- full_join(Prop_SF, PropVal_DF, by = join_by("propid" == "PROPERTY.ID")) %>% 
    mutate(PID = 1:n()) 

## Calculate areas using sf::st_area() function
Prop_Val_VALID_SF <- Prop_Val_SF %>%
    filter(st_is_valid(.))  %>% 
    mutate(AREA_SF = st_area(.))

# Use the areas calculated using sf::st_area() for missing values in the property value file
Prop_Val_FULL_SF <- left_join(Prop_Val_SF, st_drop_geometry(Prop_Val_VALID_SF) %>% select(PID, AREA_SF), by = "PID")  %>% 
    mutate(AREA_HA = ifelse(is.na(AREA_HA), (as.numeric(AREA_SF)/ 10000), AREA_HA)) %>% 
    mutate(VALUE_HA = LAND.VALUE.1 / AREA_HA) %>%
    select(propid, AREA_HA, LAND_VALUE = LAND.VALUE.1, VALUE_HA) %>% 
    drop_na()

st_write(Prop_Val_FULL_SF, file.path(OUTPUT_DIR, "Prop_value.gdb"), layer = "Property_values", append = FALSE)
st_layers(file.path(OUTPUT_DIR, "Prop_value.gdb"))

# Rasterise the property values
Woody <- rast(file.path(INPUT_DIR, "woody_nsw.tif"))
# Woody_res <- prod(res(rast(file.path(INPUT_DIR, "woody_nsw.tif"))))/10000 # in ha

# Transform the per hectare property values to the same CRS as the woody vegetation raster
Prop_Val_FULL_SF_Projected <- st_transform(Prop_Val_FULL_SF, st_crs(Woody))

# Rasterise the per hectare property values
Prop_Val_FULL_vect <- vect(Prop_Val_FULL_SF_Projected)
Prop_Val_RAST <- rasterize(Prop_Val_FULL_vect, Woody, fun = "mean", field = "VALUE_HA", na.rm = TRUE, background = NA)
writeRaster(Prop_Val_RAST, file.path(OUTPUT_DIR, "prop_value.tif"), overwrite = TRUE)
