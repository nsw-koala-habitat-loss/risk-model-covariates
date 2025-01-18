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

PlanZone_type_df <- as.data.frame(EpiPlanZone) %>% 
  distinct(SYM_CODE, .keep_all = TRUE) %>% 
  select(LAY_CLASS, LABEL, SYM_CODE) %>% 
  arrange(SYM_CODE)
write.csv(PlanZone_type_df, file.path(INPUT_DIR, "NSW_EPI_land_zones/PlanZone_type.csv") , row.names = FALSE)

# load recode look up
PlanZone_code <- read_csv(file.path(INPUT_DIR, "NSW_EPI_land_zones/PlanZone_type.csv")) %>% 
  mutate(PlanZone = as.integer(factor(PzCode , levels = unique(PzCode)))-1)

unique(PlanZone_code[5:6])

# PlanZone_type_code_df <- PlanZone_type_df %>% 
#   left_join(PlanZone_code, by = c("SYM_CODE" = "Abbreviation")) %>% 
#   select(LAY_CLASS, LABEL, SYM_CODE, "LandZone1 description", LandZone1, PlanZone2) %>% 
#   arrange(PlanZone2) %>% 
#   mutate(PlanZoneC = as.factor(as.numeric(factor(PlanZone2, levels = unique(PlanZone2)))))
# PlanZone_type_code_df %>% select(PlanZone2, PlanZoneC) %>% distinct()
# # 
# PlanZone_ori <- vect("D:/Data/NSW_Deforestation/risk-model-covariates/Input/NSW_EPI_land_zones/All_EPI_Data_Shapefile_GDA94_22122022/EPI_Land_Zoning.shp")%>%
#   project(crs(Woody)) %>% 
#   tidyterra::filter(LGA_NAME != "LORD HOWE ISLAND - UNINCORPORATED AREA")
# 
# PlanZone_type_df <- as.data.frame(PlanZone_ori) %>% 
#   distinct(SYM_CODE, .keep_all = TRUE) %>% 
#   arrange(SYM_CODE)
# 
# PZ <- full_join(PlanZone_type_df, PlanZone_code, by = c("SYM_CODE" = "Abbreviation"), keep = TRUE) %>% 
#   select(LAY_CLASS, LABEL, SYM_CODE, Abbreviation, "LandZone1 description", LandZone1, PlanZone2)
# 
# write.csv(PlanZone_type_df, "D:/Data/NSW_Deforestation/risk-model-covariates/Input/NSW_EPI_land_zones/PlanZone_type.csv", row.names = FALSE)
# 
# PlanZone_type_df <- PlanZone_ori %>% 
#   tidyterra::select(LABEL , SYM_CODE) %>%
#   as.data.frame() %>% 
#   distinct(SYM_CODE, .keep_all = TRUE)
# write.csv(PlanZone_type_df, "Output/PlanZone_type.csv", row.names = FALSE)

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

# PlanZone_rast <- ifel(not.na(PlanZone_rast$PlanZoneC), PlanZone_rast$PlanZoneC, Woody_template$EXT)
# plot(PlanZone_rast)
# names(PlanZone_rast) <- "PlanZone"
# unique(PlanZone_rast$PlanZone)
# export
writeRaster(PlanZone_rast, file.path(OUTPUT_DIR, "Raster/PlanZone.tif"), overwrite = TRUE)
PlanZone_rast <- rast(file.path(OUTPUT_DIR, "Raster/PlanZone.tif"))
PlanZone <- rast(file.path(OUTPUT_DIR, "Raster/PlanZone.tif"))
plot(PlanZone)
