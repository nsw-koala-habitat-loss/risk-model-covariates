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
library(readxl)

# load data
## Define directories
INPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Input"
OUTPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Output"
## load woody raster as template
Woody <- rast(file.path(INPUT_DIR, "woody_nsw.tif"))
Woody_template <- rast(file.path(INPUT_DIR, "Woody_template.tif"))

## Federal Election 2022 ----

# Load 2022 federal election division boundary
ElecBnd <- vect(file.path(INPUT_DIR, "AEC_political_preferences/nsw-esri-06042016/NSW_electoral_boundaries_25-02-2016.shp")) %>% 
  project(crs(Woody))

# load federal first preference votes data, filter for party with maximum votes in each division and joined with electoral boundaries
Fed_FPV <- read_csv(file.path(INPUT_DIR, "AEC_political_preferences/current-data-first-prefs-03-03.csv")) %>% 
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

# Load 2022 federal election division boundary
SteElcBnd <- vect(file.path(INPUT_DIR, "NSW Electoral Com/DeterminedBoundaries2013/DeterminedBoundaries2013.MID")) %>% 
  project(crs(Woody))

# Load 2015 state election Legislative Assembly results
## Legislative Assembly - Formal Vote by Representation
## Formal First Preference (FP) Vote for each Registered Political Party or Independent Candidate for Each District
Ste_LA <- read_csv(file.path(INPUT_DIR, "NSW Electoral Com/NSW STATE ELECTION RESULTS Legislative Assembly.csv")) %>% 
  select(-'Total Formal') %>% 
  pivot_longer(cols = ACP:IND, names_to = "PolPar", values_to = "FVotes") %>% 
  group_by(District) %>%
  filter(FVotes == max(FVotes, na.rm = TRUE)) %>%
  arrange(District) 

# join with electoral boundaries
Ste_LA_Elec <- SteElcBnd %>% 
  tidyterra::left_join(Ste_LA, by = join_by("Name" == "District")) %>% 
  tidyterra::mutate(PolPar= if_else(PolPar %in% c("NP", "LIB", "CLP"), "COA", PolPar),
                    PolParN = as.integer(as.ordered(PolPar)))
Ste_LA_Elec_df <- as.data.frame(Ste_LA_Elec)
Ste_LA_Elec_df %>% distinct(PolPar) %>% mutate(PolParN = as.numeric(as.ordered(PolPar)))
ggplot()+geom_spatvector(data = Ste_LA_Elec, aes(fill = PolPar))

Ste_LA_rast <- rasterize(Ste_LA_Elec, Woody, field = "PolParN") %>% 
  crop(Woody, snap = "out", mask = TRUE)
names(Ste_LA_rast) <- "PolPref"
Ste_LA_rast <- ifel(not.na(Ste_LA_rast$PolPref), Ste_LA_rast$PolPref, Woody_template$EXT)
# export
writeRaster(Ste_LA_rast, file.path(OUTPUT_DIR, "Raster/PolPref.tif"), overwrite = TRUE)
plot(Ste_LA_rast)



# Legislative Assembly Two Party Preferred Results
Ste_TPP_COA <- read_xlsx(file.path(INPUT_DIR, "NSW Electoral Com/NSW STATE ELECTION RESULTS Legislative Assembly.xlsx"), sheet = "TwoPartyPref") %>%
  select(1:5) %>% mutate(ParAff = "COA")
colnames(Ste_TPP_COA) <- c("District", "Party", "Cand", "Cand_Vote", "Pct", "ParAff")
Ste_TPP_LAB <- read_xlsx(file.path(INPUT_DIR, "NSW Electoral Com/NSW STATE ELECTION RESULTS Legislative Assembly.xlsx"), sheet = "TwoPartyPref") %>%
  select(1, 6, 7, 8, 9) %>%  mutate(ParAff = "LAB")
colnames(Ste_TPP_LAB) <- c("District", "Party", "Cand", "Cand_Vote", "Pct", "ParAff")
Ste_TPP <- rbind(Ste_TPP_COA, Ste_TPP_LAB) %>% 
  group_by(District) %>%
  filter(Pct == max(Pct, na.rm = TRUE)) %>%
  arrange(District)


# join with electoral boundaries
Ste_TPP_Elec <- SteElcBnd %>% 
  tidyterra::left_join(Ste_TPP, by = join_by("Name" == "District")) %>% 
  tidyterra::mutate(ParAffn = as.factor(as.integer(as.ordered(ParAff))))
Ste_LA_Elec_df <- as.data.frame(Ste_TPP_Elec)
Ste_LA_Elec_df %>% distinct(ParAff) %>% mutate(ParAffN = as.factor(as.integer(as.ordered(ParAff))))
ggplot()+geom_spatvector(data = Ste_TPP_Elec, aes(fill = ParAffn))
writeVector(Ste_TPP_Elec, file.path(OUTPUT_DIR, "Shapefile/Ste_TPP_Elec.shp"))

Ste_TPP_rast <- rasterize(Ste_TPP_Elec, Woody, field = "ParAffn") %>% 
  crop(Woody, snap = "out", mask = TRUE)
names(Ste_TPP_rast) <- "PolPref"
# Ste_LA_rast <- ifel(not.na(Ste_LA_rast$PolPref), Ste_LA_rast$PolPref, Woody_template$EXT)
# export
writeRaster(Ste_TPP_rast, file.path(OUTPUT_DIR, "Raster/PolPref.tif"), overwrite = TRUE)
Ste_TPP_rast <- rast(file.path(INPUT_DIR, "Raster/PolPref.tif"))
unique(Ste_TPP_rast)
levels(Ste_TPP_rast) <- c(1,2)
# plot(Ste_TPP_rast_f)
