# Purpose: To extract political preference data 

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

# Load Libraries
library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(stringr)
library(qs)

# load data
# load woody raster as template
Woody <- rast("Input/woody_nsw.tif")

## Federal Election 2022 ----

# Load 2022 federal election division boundary
ElecBnd <- vect("Input/AEC_political_preferences/nsw-esri-06042016/NSW_electoral_boundaries_25-02-2016.shp") %>% 
  project(crs(Woody))

# load federal first preference votes data, filter for party with maximum votes in each division and joined with electoral boundaries
Fed_FPV <- read_csv("Input/AEC_political_preferences/current-data-first-prefs-03-03.csv") %>% 
  filter(State == "NSW") %>%
  group_by(DivisionName) %>%
  filter(Votes == max(Votes)) %>%
  arrange(DivisionId) 

# join with electoral boundaries
Fed_FPV_Elec <- ElecBnd %>% 
  tidyterra::left_join(Fed_FPV, by = join_by("Elect_div" == "DivisionName")) %>% 
  select(PartyAb)

# rasterize
Fed_FPV_rast <- rasterize(Fed_FPV_Elec, Woody, field = "PartyAb")
names(Fed_FPV_rast) <- "PolPref"
# export
writeRaster(Fed_FPV_rast, "Output/PolPref.tif", overwrite = TRUE)


## State General Election 2015 ----
# load data

# load woody raster as template
Woody <- rast("Input/woody_nsw.tif")

# Load 2022 federal election division boundary
SteElcBnd <- vect("Input/NSW Electoral Com/DeterminedBoundaries2013/DeterminedBoundaries2013.MID") %>% 
  project(crs(Woody))

# Load 2015 state election Legislative Assembly results
Ste_LA <- read_csv("Input/NSW Electoral Com/NSW STATE ELECTION RESULTS Legislative Assembly.csv") %>% 
  select(-'Total Formal') %>% 
  pivot_longer(cols = ACP:IND, names_to = "PolPar", values_to = "FVotes") %>% 
  group_by(District) %>%
  filter(FVotes == max(FVotes, na.rm = TRUE)) %>%
  arrange(District) 

# join with electoral boundaries
Ste_LA_Elec <- SteElcBnd %>% 
  tidyterra::left_join(Ste_LA, by = join_by("Name" == "District"))
Ste_LA_Elec_df <- as.data.frame(Ste_LA_Elec)
ggplot()+geom_spatvector(data = Ste_LA_Elec, aes(fill = PolPar))

Ste_LA_rast <- rasterize(Ste_LA_Elec, Woody, field = "PolPar") %>% 
  crop(Woody, snap = "out", mask = TRUE)
names(Ste_LA_rast) <- "PolPref"
# export
writeRaster(Ste_LA_rast, "Output/Raster/PolPref.tif", overwrite = TRUE)

