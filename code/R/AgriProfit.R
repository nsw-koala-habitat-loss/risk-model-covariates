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
Woody <- rast("Input/woody_nsw.tif")
Woody_template <- rast("Input/Woody_template.tif")

# load profit data
AgriProfit <- rast("Input/Agricultural profit map for Australia for 2010-2011/pfe201011.tif") %>% 
  project(Woody, thread = TRUE) %>% 
  crop(Woody, snap = "out", mask = TRUE)

AUS_AgriProfit <- rast("Input/Agricultural profit map for Australia for 2010-2011/pfe201011.tif")
  
plot(AgriProfit)
plot(AUS_AgriProfit)

# ABS Agricultural census 2016
# The Value of Agricultural Commodities Produced GeoPackage container present estimates using the 2011 edition of Australian Statistical Geography Standard (ASGS).

SA2_2011 <- vect("Input/2011_BCP_SA1_for_NSW_short-header/1270055001_sa2_2011_aust_shape/SA2_2011_AUST.shp") %>% 
  tidyterra::filter(STE_NAME11 == "New South Wales") %>% 
  mutate(SA2_Area_HA = ALBERS_SQM/1E4) %>%
  select(SA2_MAIN = SA2_MAIN11, SA2_Area_HA)

AgCommVal <- read_csv("Input/ABS_Agricultural Commodities 2015-16/75030DO005_201516_Value of Agricultural Commodities.csv", skip = 4) %>% 
  select(Reg_Code = 'Region code', Commodity = 'Commodity description', Value = 'Local value ($)') %>%
  filter(str_detect(Reg_Code, "^1\\d{8}$"), str_detect(Commodity, "Total")) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  pivot_wider(names_from = Commodity, values_from = Value, names_expand = TRUE) %>% 
  select(Reg_Code,
         Total_agri = "Total agriculture",
         # Broadacre_crops = "Broadacre crops - Total",
         # Fruit_nuts_xGrape = "Fruit and nuts (excluding grapes) - Total",
         # Fruit_nuts_Grapes = "Fruit and nuts - Grapes - Total",
         # Hay = "Hay - Total",
         # Lstock_prod = "Livestock products - Total",
         # Lstock_other = "Livestock slaughtered and other disposals - Total",
         # Nurseries_cut_flowers_cultivated_turf = "Nurseries, cut flowers or cultivated turf - Total",
         # Total_crops = "Total value of crops",
         # Veg_Total = "Vegetables for human consumption - Total"
         )

landuse2017 <- vect("Input/land_nswlanduse2017v1p5/NSWLanduse2017_Ver1_5_20230921.shp") %>% 
  tidyterra::mutate(AGRI_Land = ifelse(SecondaryA %in% c(210, 320, 330, 340, 350, 420, 430, 440, 450, 510, 520, 530, 540), 1 , 0),
                    AGRI_Land = ifelse(TertiaryAL %in% c(515, 528, 530, 531, 532, 533, 536, 537, 538, 540, 541, 543, 544), 0 , AGRI_Land)) %>% 
  tidyterra::select(SecondaryA, TertiaryAL, AGRI_Land) %>% 
  tidyterra::filter(AGRI_Land == 1)

SA2_Agri <- terra::intersect(landuse2017, SA2_2011) %>% 
  tidyterra::mutate(AREA_Agri_ha = ((terra::expanse(.))/1E4)) %>% 
  tidyterra::select(SA2_MAIN, AREA_Agri_ha) %>%
  terra::aggregate(by = "SA2_MAIN", dissolve=TRUE, fun = "sum")
SA2_Agri_df <- as.data.frame(SA2_Agri)

SA2_Agri_CommVal <- SA2_2011 %>%
  tidyterra::left_join(AgCommVal, by = join_by("SA2_MAIN" == "Reg_Code")) %>% 
  tidyterra::select(SA2_MAIN, Total_agri, SA2_Area_HA) %>% 
  tidyterra::left_join(SA2_Agri_df, by = join_by("SA2_MAIN" == "SA2_MAIN")) %>% 
  tidyterra::mutate(Agri_per_ha = case_when(Total_agri > 0 & sum_AREA_Agri_ha > 0 ~ Total_agri/sum_AREA_Agri_ha , 
                                            Total_agri > 0 & (is.na(sum_AREA_Agri_ha) | sum_AREA_Agri_ha == 0)  ~ Total_agri / SA2_Area_HA,
                                            Total_agri <= 0 ~ 0,
                                            .default = NA)) %>% 
  tidyterra::filter(!is.na(Agri_per_ha)) %>% 
  project(crs(Woody))

writeVector(SA2_Agri_CommVal, "Output/Shapefile/SA2_Agri_CommVal.shp", overwrite = TRUE)

SA2_Agri_CommVal_r <- rasterize(SA2_Agri_CommVal, Woody_template, field = "Agri_per_ha", fun = "mean")
writeRaster(SA2_Agri_CommVal_r, "Output/Raster/AgriVal.tif", overwrite=TRUE)
