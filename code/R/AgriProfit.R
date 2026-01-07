# Purpose: To process Agriculture profit data 

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

# Load Libraries
library(tidyverse)
library(tidyterra)
library(terra)
library(sf)
library(stringr)
library(qs)
library(Rcpp)
library(foreign)
library(ggpubr)


# load data
# load woody raster as template
INPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Input"
OUTPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Output"
Woody <- rast(file.path(INPUT_DIR, "woody_nsw.tif"))
Woody_template <- rast(file.path(INPUT_DIR, "Woody_template.tif"))

# ABS Agricultural census 2016
# The Value of Agricultural Commodities Produced GeoPackage container present estimates using the 2011 edition of Australian Statistical Geography Standard (ASGS).

# load SA2 shapefile for NSW
SA2_2011 <- vect(file.path(INPUT_DIR, "2011_BCP_SA1_for_NSW_short-header/1270055001_sa2_2011_aust_shape/SA2_2011_AUST.shp")) %>% 
  tidyterra::filter(STE_NAME11 == "New South Wales") %>% 
  mutate(SA2_Area_HA = ALBERS_SQM/1E4) %>%
  select(SA2_MAIN = SA2_MAIN11, SA2_Area_HA)

# load agricultural commodity value data
## Then filter to get total agri value for each SA2 region
## The region code for SA2 in NSW starts with '1'
AgCommVal <- read_csv(file.path(INPUT_DIR, "ABS_Agricultural Commodities 2015-16/75030DO005_201516_Value of Agricultural Commodities.csv", skip = 4)) %>% 
  select(Reg_Code = 'Region code', Commodity = 'Commodity description', Value = 'Local value ($)') %>%
  filter(str_detect(Reg_Code, "^1\\d{8}$"), str_detect(Commodity, "Total")) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  pivot_wider(names_from = Commodity, values_from = Value, names_expand = TRUE) %>% 
  select(Reg_Code,
         Total_agri = "Total agriculture",
         )

# Calculate agricultural profit per hectare of agricultural land within each SA2 region
# load land use data to get agricultural land area
# land use data to get agricultural land area
landuse2017 <- vect(file.path(INPUT_DIR, "land_nswlanduse2017v1p5/NSWLanduse2017_Ver1_5_20230921.shp")) %>% 
  tidyterra::mutate(AGRI_Land = ifelse(SecondaryA %in% c(210, 320, 330, 340, 350, 420, 430, 440, 450, 510, 520, 530, 540), 1 , 0),
                    AGRI_Land = ifelse(TertiaryAL %in% c(515, 528, 530, 531, 532, 533, 536, 537, 538, 540, 541, 543, 544), 0 , AGRI_Land)) %>% 
  tidyterra::select(SecondaryA, TertiaryAL, AGRI_Land) %>% 
  tidyterra::filter(AGRI_Land == 1)

# intersect agricultural land with SA2 regions to get agricultural land area within each SA2 region
SA2_Agri <- terra::intersect(landuse2017, SA2_2011) %>% 
  tidyterra::mutate(AREA_Agri_ha = ((terra::expanse(.))/1E4)) %>% 
  tidyterra::select(SA2_MAIN, AREA_Agri_ha) %>%
  terra::aggregate(by = "SA2_MAIN", dissolve=TRUE, fun = "sum")
SA2_Agri_df <- as.data.frame(SA2_Agri)

# join agricultural commodity value data with agricultural land area data to calculate agricultural profit per hectare
SA2_Agri_CommVal <- SA2_2011 %>%
  tidyterra::left_join(AgCommVal, by = join_by("SA2_MAIN" == "Reg_Code")) %>% 
  tidyterra::select(SA2_MAIN, Total_agri, SA2_Area_HA) %>% 
  tidyterra::left_join(SA2_Agri_df, by = join_by("SA2_MAIN" == "SA2_MAIN")) %>% 
  tidyterra::mutate(Agri_per_ha = case_when(Total_agri > 0 & sum_AREA_Agri_ha > 0 ~ Total_agri/sum_AREA_Agri_ha , 
                                            Total_agri > 0 & sum_AREA_Agri_ha == 0 ~ NA,
                                            Total_agri > 0 & is.na(sum_AREA_Agri_ha) ~ NA,
                                            Total_agri == 0| is.na(Total_agri) ~ 0,
                                            .default = 999999999999)) %>%  # to capture any other cases (none here)
  project(crs(Woody))

# finalize the data
SA2_Agri_CommVal <- SA2_Agri_CommVal %>% 
  tidyterra::select(SA2_MAIN, Ag_profit = Total_agri, Ag_Area_ha = sum_AREA_Agri_ha , AgProf_ha = Agri_per_ha)

writeVector(SA2_Agri_CommVal, "Output/Shapefile/SA2_Agri_CommVal.shp", overwrite = TRUE)

SA2_Agri_CommVal_r <- rasterize(SA2_Agri_CommVal, Woody_template, field = "AgProf_ha", fun = "mean")
writeRaster(SA2_Agri_CommVal_r, "Output/Raster/AgProf.tif", overwrite=TRUE)
