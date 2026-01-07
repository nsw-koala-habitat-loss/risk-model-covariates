# Purpose: To generate planning zone

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

# Cat2Num <- read_xlsx("covariate_description.xlsx", sheet = "Cat2Num")

# load EPI Land zoning data
EpiPlanZone <- vect(file.path(INPUT_DIR, "NSW_EPI_land_zones/All_EPI_Data_Shapefile_GDA94_22122022/EPI_Land_Zoning.shp")) %>% 
  project(crs(Woody)) %>% 
  tidyterra::filter(LGA_NAME != "LORD HOWE ISLAND - UNINCORPORATED AREA")

# Create planning zone code based on SYM_CODE and description
## Reduced classification to 5 classes
PlanZone_code <- as.data.frame(EpiPlanZone) %>% 
  distinct(SYM_CODE, .keep_all = TRUE) %>% 
  select(LAY_CLASS, LABEL, SYM_CODE) %>% 
  arrange(SYM_CODE) %>% 
  mutate(PzCode2 = case_when(str_detect(SYM_CODE, "^(A|2\\(a\\)|R1|R2|R3|R4|R5)$") ~ "R",
                             str_detect(SYM_CODE, "^(AGB|B|B1|B2|B3|B4|B5|B6|B7|B8|C|CA|D|E|E2|ENT|MU|REZ|SP3|SP4|UD|IN1|IN2|IN3|IN4|W3|Yellow)$") ~ "B",
                             str_detect(SYM_CODE, "^(C1|C2|C3|C4|H|I|ENZ|RE1|RE2|W1|W2)$") ~ "C",
                             str_detect(SYM_CODE, "^(E1|RAC|RAZ|RU1|RU2|RU3|RU4|RU5|RU6)$") ~ "RU",
                             .default = "O" ),
         PzCode_des = case_when(PzCode2 == "R" ~ "Residential",
                                 PzCode2 == "B" ~ "Business",
                                 PzCode2 == "C" ~ "Environment",
                                 PzCode2 == "RU" ~ "Rural",
                                 PzCode2 == "O" ~ "Others"),
         PlanZone = as.integer(factor(PzCode , levels = unique(PzCode)))-1)

PlanZone <- EpiPlanZone %>%
  tidyterra::select(LABEL , SYM_CODE, LAY_CLASS) %>%
  tidyterra::left_join(PlanZone_code, by = join_by("SYM_CODE" == "SYM_CODE")) %>%
  tidyterra::mutate(PlanZone = case_when(LAY_CLASS.x == "Business Zone - Commercial Core" ~ 0, # Business Zone - Commercial Core
                                          LAY_CLASS.x == "Environment" ~ 1,
                                          .default = PlanZone),
                    PlanZone = as.integer(PlanZone))

# Potentially exclude SYM_CODE == "DR" (Drainage)
writeVector(PlanZone, file.path(OUTPUT_DIR, "Shapefile/PlanZone.shp"), overwrite = TRUE)

PlanZone <- vect(file.path(OUTPUT_DIR, "Shapefile/PlanZone.shp"))
names(PlanZone)
PlanZone_df <- as.data.frame(PlanZone)

# rasterize
PlanZone_rast <- rasterize(PlanZone, Woody, field = "PlanZone") %>% 
  crop(Woody, snap = "out", mask = TRUE)
plot(Woody_template)
plot(PlanZone_rast, add=TRUE)
unique(PlanZone_rast$PlanZone)


writeRaster(PlanZone_rast, file.path(OUTPUT_DIR, "Raster/PlanZone.tif"), overwrite = TRUE)
PlanZone_rast <- rast(file.path(OUTPUT_DIR, "Raster/PlanZone.tif"))
PlanZone <- rast(file.path(OUTPUT_DIR, "Raster/PlanZone.tif"))
plot(PlanZone)
