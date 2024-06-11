# Purpose: To process soil data and to produce PCA for representing soil fertility and soiltype in NSW

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

library(terra)
library(tidyverse)
library(furrr)
if (!require(qs)) install.packages('qs')
library(qs)
library(mice)
library(ggfortify)
library(ggpubr)
library(tidyterra)
library(ggokabeito)
library(viridis)
library(gg3D)
theme_set(theme_pubr())

# Raw Data processing (can skip)----
# # unzip downloaded soil data from SEED
# # soil organic carbon (SOC), pH, cation exchange capacity, sum-of-bases, available phosphorous, bulk density, clay, silt and sand (total and fine)
# ZIP_files <- list.files("SoilData/",pattern = "\\.zip$", full.names = TRUE, recursive = FALSE)
# for (ZIP_file in ZIP_files){
#   unzip(ZIP_file, exdir = "SoilData/", junkpaths = TRUE, overwrite = TRUE)
# } 
# # Process NSW Soil Data from SEED
# NSWsoil_fl <- list.files("SoilData/",pattern = "\\.tif$", full.names = FALSE, recursive = FALSE)
# NSWsoil_SOC_fl <- list.files("SoilData/", pattern = glob2rx("NSW_SOC*"), full.names = FALSE, recursive = FALSE)

# load Woody data for reference
Woody <- terra::rast("input/woody_nsw.tif")

# Project the soil data to the same CRS as Woody
SOCpc_0_30 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_SOCpc0_30_mean_220901reduced.tif") %>% project(y = crs(Woody), method = "bilinear", threads = TRUE)

# Soil data processing at 3 second arc resolution (raw data resolution) to reduce processing time
# Resample Woody to the same origin and resolution as the soil data and use it as reference layer
Woody_3sec <- terra::resample(Woody, SOCpc_0_30, method = "mode", threads = TRUE) %>% writeRaster("SoilData/Woody_3sec.tif", overwrite = TRUE)
# SoilFert_3sec <- terra::resample(SoilFert, SOCpc_0_30, method = "mode", threads = TRUE) %>%  crop(y=SOCpc_0_30, snap = "out", mask = TRUE, threads = TRUE) %>%
#   writeRaster("SoilData/SoilFert_3sec.tif", overwrite = TRUE)

Woody_3sec <- rast("Input/SoilData/Woody_3sec.tif")
Woody_3sec_df <- as.data.frame(Woody_3sec,xy =TRUE)
gc()

SOC_0_30 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_SOCpc0_30_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_SOCpc0_30.tif", overwrite = TRUE)
SOC_30_60 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_SOCpc30_60_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_SOCpc30_60.tif", overwrite = TRUE)
SOC_60_100 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_SOCpc60_100_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE)%>% writeRaster("Output/SoilData/NSW_SOCpc60_100.tif", overwrite = TRUE)
gc()
PH_0_30 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_PH0_30_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_PH0_30.tif", overwrite = TRUE)
PH_30_60 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_PH30_60_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_PH30_60.tif", overwrite = TRUE)
PH_60_100 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_PH60_100_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_PH60_100.tif", overwrite = TRUE)
PH_100_200 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_PH100_200_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_PH100_200.tif", overwrite = TRUE)
gc()
CEC_0_30 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_CEC0_30_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE)  %>% writeRaster("Output/SoilData/NSW_CEC0_30.tif", overwrite = TRUE)
CEC_30_60 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_CEC30_60_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_CEC30_60.tif", overwrite = TRUE)
CEC_60_100 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_CEC60_100_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_CEC60_100.tif", overwrite = TRUE)
CEC_100_200 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_CEC100_200_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_CEC100_200.tif", overwrite = TRUE)
gc()
SOB_0_30 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_SumBase0_30_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_SumBase0_30.tif", overwrite = TRUE)
SOB_30_60 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_SumBase30_60_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_SumBase30_60.tif", overwrite = TRUE)
SOB_60_100 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_SumBase60_100_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_SumBase60_100.tif", overwrite = TRUE)
SOB_100_200 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_SumBase100_200_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_SumBase100_200.tif", overwrite = TRUE)
gc()
AVP_0_30 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_Pbray0_30_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_Pbray0_30.tif", overwrite = TRUE)
AVP_30_60 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_Pbray30_60_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_Pbray30_60.tif", overwrite = TRUE)
AVP_60_100 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_Pbray60_100_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_Pbray60_100.tif", overwrite = TRUE)
AVP_100_200 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_Pbray100_200_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_Pbray100_200.tif")
gc()
BD_0_30 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_BD0_30_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_BD0_30.tif")
gc()
clay_0_30 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_Clay0_30_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_Clay0_30.tif")
clay_30_60 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_Clay30_60_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_Clay30_60.tif")
clay_60_100 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_Clay60_100_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_Clay60_100.tif")
clay_100_200 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_Clay100_200_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_Clay100_200.tif")
gc()
Silt_0_30 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_Silt0_30_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_Silt0_30.tif")
Silt_30_60 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_Silt30_60_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_Silt30_60.tif")
Silt_60_100 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_Silt60_100_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_Silt60_100.tif")
Silt_100_200 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_Silt100_200_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_Silt100_200.tif")
gc()
Sand_0_30 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_Sand0_30_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_Sand0_30.tif")
Sand_30_60 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_Sand30_60_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_Sand30_60.tif")
Sand_60_100 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_Sand60_100_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_Sand60_100.tif")
Sand_100_200 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_Sand100_200_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_Sand100_200.tif")
gc()
FSand_0_30 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_FSand0_30_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_FSand0_30.tif")
FSand_30_60 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_FSand30_60_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_FSand30_60.tif")
FSand_60_100 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_FSand60_100_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_FSand60_100.tif")
FSand_100_200 <- rast("Input/SoilData/Raw_download/DSM_NSW/NSW_FSand100_200_mean_220901reduced.tif")%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("Output/SoilData/NSW_FSand100_200.tif")
gc()

# SoilList <- list(SOC_0_30, SOC_30_60, SOC_60_100, CEC_0_30, CEC_30_60, CEC_60_100, CEC_100_200, BD_0_30, Silt_0_30, Silt_30_60, Silt_60_100, Silt_100_200, Sand_0_30, Sand_30_60, Sand_60_100, Sand_100_200, PH_0_30, PH_30_60, PH_60_100, PH_100_200, SOB_0_30, SOB_30_60, SOB_60_100, SOB_100_200, AVP_0_30, AVP_30_60, AVP_60_100, AVP_100_200, clay_0_30, clay_30_60, clay_60_100, clay_100_200, FSand_0_30, FSand_30_60, FSand_60_100, FSand_100_200)
# lapply(SoilList, function(x){crs(x)})

# SoilStack_NSW <- rast(list(SOC_0_30, SOC_30_60, SOC_60_100, CEC_0_30, CEC_30_60, CEC_60_100, CEC_100_200, BD_0_30, Silt_0_30, Silt_30_60, Silt_60_100, Silt_100_200, Sand_0_30, Sand_30_60, Sand_60_100, Sand_100_200, PH_0_30, PH_30_60, PH_60_100, PH_100_200, SOB_0_30, SOB_30_60, SOB_60_100, SOB_100_200, AVP_0_30, AVP_30_60, AVP_60_100, AVP_100_200, clay_0_30, clay_30_60, clay_60_100, clay_100_200, FSand_0_30, FSand_30_60, FSand_60_100, FSand_100_200))
# writeRaster(SoilStack_NSW, "SoilData/SoilStack_NSW.tif")

# NSW Soil Data from SLGA----
# Download relevant soil data not available in SEED from SLGA
# API key name: DellTower-Tern
# API key prefix: eISugEyuDtEIZCFr
# API key: ZUlTdWdFeXVEdEVJWkNGci45a0g6NmRUby12JzEwSkhLKSJXSGAmYFUoUWs8UmQyU1ZjOUhLZC03WlwkUnV4eVwxZ31KdHNQMGJgRXIubn1q

# Test download 1 raster layer from SLGA 
apikey <- paste0('apikey:', "ZUlTdWdFeXVEdEVJWkNGci45a0g6NmRUby12JzEwSkhLKSJXSGAmYFUoUWs8UmQyU1ZjOUhLZC03WlwkUnV4eVwxZ31KdHNQMGJgRXIubn1q")
AWC_000_005 <- rast(paste0('/vsicurl/https://',apikey,'@data.tern.org.au/landscapes/slga/NationalMaps/SoilAndLandscapeGrid/AWC/AWC_000_005_EV_N_P_AU_TRN_N_20210614.tif'))


# Extract extent of the raster
Woody <- terra::rast("Input/woody_nsw.tif")
Woody_WGS84 <- terra::project(Woody, crs(AWC_000_005), threads=TRUE)
Woody_ext <- rast(ext(Woody_WGS84), res = res(Woody_WGS84), crs = crs(Woody_WGS84))
values(Woody_ext) <-1 
Woody_extw <- wrap(Woody_ext)

# Construct URL based on naming convention (https://esoil.io/TERNLandscapes/Public/Pages/SLGA/MetaData/ASLG_File_Naming_Conventions.html)
# Available Water Capacity (AWC),  Depth of Soil (DES), Total Nitrogen (NTO)
SLGA_Depth <- c("000_005", "005_015", "015_030", "030_060", "060_100", "100_200")
SLGA_Fname <- c(paste0("AWC/AWC_",SLGA_Depth, "_EV_N_P_AU_TRN_N_20210614.tif"),
                paste0("NTO/NTO_",SLGA_Depth, "_EV_N_P_AU_NAT_C_20140801.tif"),
                "DES/DES_000_200_EV_N_P_AU_TRN_C_20190901.tif")

ptm <- proc.time()
plan(multisession, workers = 4)
future_map(SLGA_Fname, function (x){
  rast(paste0('/vsicurl/https://',apikey,'@data.tern.org.au/landscapes/slga/NationalMaps/SoilAndLandscapeGrid/', x)) %>% 
    crop(y = terra::unwrap(Woody_extw), snap = "out") %>% 
    writeRaster(paste0("Input/SoilData/Raw_download/SLGA/", substr(x, start = 5, stop = nchar(x))), overwrite = TRUE)
})
plan(sequential)
proc.time() - ptm

# SLGA_fl <- list.files("SoilData/SLGA/",pattern = "\\.tif$", full.names = TRUE, recursive = FALSE)
# SoilStack_SLGA <- rast(SLGA_fl)
# names(SoilStack_SLGA) <- c(substr(SLGA_Fname, start = 5, stop = 15))
# SoilStack_SLGA <- project(SoilStack_SLGA, crs(SoilFert_3sec), method = "bilinear", threads = TRUE) %>% 
# resample(SoilFert_3sec, method = "bilinear", threads = TRUE) %>% 
# writeRaster("SoilData/SoilStack_SLGA.tif")

Woody_3sec <- rast("SoilData/Woody_3sec.tif")

AWC_0_30 <- mean(
  rast(
    list(
      rast("Input/SoilData/Raw_download/SLGA/AWC_000_005_EV_N_P_AU_TRN_N_20210614.tif"),
      rast("Input/SoilData/Raw_download/SLGA/AWC_005_015_EV_N_P_AU_TRN_N_20210614.tif"), 
      rast("Input/SoilData/Raw_download/SLGA/AWC_015_030_EV_N_P_AU_TRN_N_20210614.tif")))) %>% 
  project(y = Woody_3sec, method = "bilinear", threads = TRUE) %>% 
  resample(y = Woody_3sec, method = "bilinear", threads = TRUE) %>% 
  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% 
  writeRaster("Output/SoilData/AWC_0_30.tif", overwrite = TRUE)

NTO_0_30 <- mean(
  rast(
    list(
      rast("Input/SoilData/Raw_download/SLGA/NTO_000_005_EV_N_P_AU_NAT_C_20140801.tif"),
      rast("Input/SoilData/Raw_download/SLGA/AWC_005_015_EV_N_P_AU_TRN_N_20210614.tif"),
      rast("Input/SoilData/Raw_download/SLGA/AWC_015_030_EV_N_P_AU_TRN_N_20210614.tif")))) %>% 
  project(y = Woody_3sec, method = "bilinear", threads = TRUE) %>% 
  resample(y = Woody_3sec, method = "bilinear", threads = TRUE) %>% 
  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% 
  writeRaster("Output/SoilData/NTO_0_30.tif", overwrite = TRUE)

DES_0_200 <- rast("Input/SoilData/Raw_download/SLGA/DES_000_200_EV_N_P_AU_TRN_C_20190901.tif") %>% 
  project(y = Woody_3sec, method = "bilinear", threads = TRUE) %>% 
  resample(y = Woody_3sec, method = "bilinear", threads = TRUE) %>% 
  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% 
  writeRaster("Output/SoilData/DES_0_200.tif", overwrite = TRUE)

# Incorporate Soil Type and Soil Fertility data
SoilType_3sec <- rast("Input/SoilData/soil_type.tif") %>% resample(Woody_3sec, method = "mode", threads = TRUE) %>% crop(Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("SoilData/SoilType_3sec.tif", overwrite = TRUE)
SoilFert_3sec <- rast("Input/SoilData/soil_fert.tif") %>% resample(Woody_3sec, method = "mode", threads = TRUE) %>% crop(Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("SoilData/SoilFert_3sec.tif", overwrite = TRUE)

# Read in all Raster and create raster stack

SOC_0_30 <- rast("Output/SoilData/NSW_SOCpc0_30.tif")
PH_0_30 <- rast("Output/SoilData/NSW_PH0_30.tif")
CEC_0_30 <- rast("Output/SoilData/NSW_CEC0_30.tif")
SOB_0_30 <- rast("Output/SoilData/NSW_SumBase0_30.tif")
AVP_0_30 <- rast("Output/SoilData/NSW_Pbray0_30.tif")
BD_0_30 <- rast("Output/SoilData/NSW_BD0_30.tif")
AWC_0_30 <- rast("Output/SoilData/AWC_0_30.tif")
NTO_0_30 <- rast("Output/SoilData/NTO_0_30.tif")
DES_0_200 <- rast("Output/SoilData/DES_0_200.tif")
clay_0_30 <- rast("Output/SoilData/NSW_Clay0_30.tif")
Silt_0_30 <- rast("Output/SoilData/NSW_Silt0_30.tif")
Sand_0_30 <- rast("Output/SoilData/NSW_Sand0_30.tif")
Fsand_0_30 <- rast("Output/SoilData/NSW_FSand0_30.tif")
SoilFert_3sec <- rast("Output/SoilData/SoilFert_3sec.tif")
SoilType_3sec <- rast("Output/SoilData/SoilType_3sec.tif")
Woody_3sec <- rast("Input/SoilData/Woody_3sec.tif")


TSoilStack <- rast(list(SOC_0_30, PH_0_30, CEC_0_30, SOB_0_30, AVP_0_30, 
                        BD_0_30, AWC_0_30, NTO_0_30, DES_0_200, 
                        clay_0_30, Silt_0_30, Sand_0_30, Fsand_0_30, 
                        SoilFert_3sec, SoilType_3sec, Woody_3sec))
names(TSoilStack) <- c("SOC_0_30", "PH_0_30", "CEC_0_30", "SOB_0_30", "AVP_0_30", 
                       "BD_0_30", "AWC_0_30", "NTO_0_30", "DES_0_200", 
                       "clay_0_30", "Silt_0_30", "Sand_0_30", "Fsand_0_30",
                       "SoilFert", "SoilType", "Woody")

TSoilStack_3sec_df <- as.data.frame(TSoilStack,  xy = TRUE)
gc()

# Check for NAs
summary(TSoilStack_3sec_df)
sum(is.na(TSoilStack_3sec_df))
map(TSoilStack_df, ~sum(is.na(.)))

TSoilStack_3sec_df <- TSoilStack_3sec_df %>% 
  mutate(SoilFert = as.factor(SoilFert),
         SoilType = as.factor(SoilType)) %>% 
  qsave("Output/SoilData/TSoilStack_3sec_df.qs")

# PCA for Top 30cm soil----

TSoilStack_3sec_df <- qread("Output/SoilData/TSoilStack_3sec_df.qs") %>%
  drop_na()



ptm <- proc.time()
PCA_TSoil_3sec <- prcomp(TSoilStack_3sec_df[,3:15], scale = TRUE)
proc.time() -ptm

qsave(PCA_TSoil_3sec, "Output/SoilData/PCA_TSoil_3sec.qs")
PCA_TSoil_3sec <- qread("Output/SoilData/PCA_TSoil_3sec.qs")
print(PCA_TSoil_3sec)
summary(PCA_TSoil_3sec)
head(PCA_TSoil_3sec$x)
nrow(PCA_TSoil_3sec$x)

# Extract PC axes for plotting
TSoil_3sec_PCval <- TSoilStack_3sec_df %>% 
  select(x, y, SoilFert, SoilType) %>%
  cbind(PCA_TSoil_3sec$x)
qsave(TSoil_3sec_PCval, "Output/SoilData/TSoil_3sec_PCval.qs")

# Extract loadings of the variables
TSoil_3sec_PCload <- rownames_to_column(data.frame(PCA_TSoil_3sec$rotation), "Variable")
rownames(TSoil_3sec_PCload) <- NULL
qsave(TSoil_3sec_PCload, "Output/SoilData/TSoil_3sec_PCload.qs")

#Covert PC1, PC2 & PC3 to raster
TSoilPC_3sec <- rast(TSoil_3sec_PCval[,c(1,2,5,6,7)], type = "xyz", crs = crs(Woody_3sec)) 
writeRaster(TSoilPC_3sec, "Output/SoilData/TSoilPC_3sec.tif", overwrite = TRUE)

Woody_3sec <- rast("SoilData/Woody_3sec.tif")
Woody <- rast("input/woody_cover/woody_nsw.tif")
TSoilPC <- TSoilPC_3sec %>% resample(Woody, method = "bilinear", threads = TRUE) %>% crop(Woody, snap = "out", mask = TRUE)
names(TSoilPC) <- c("Soil_PC1", "Soil_PC2", "Soil_PC3")
writeRaster(TSoilPC, "Output/Raster/TSoilPC.tif", overwrite = TRUE)

# Plotting----
TSoil_3sec_PCval <- qread("Output/SoilData/TSoil_3sec_PCval.qs")
TSoil_3sec_PCload <- qread("Output/SoilData/TSoil_3sec_PCload.qs")

TSoil_Fert_PC12 <- ggplot(TSoil_3sec_PCval, aes(x = PC1, y = PC2, colour = SoilFert)) +
  geom_point(size = 0.1, alpha = 0.05) +
  scale_colour_okabe_ito()+
  stat_ellipse(geom="polygon", level=0.95, alpha=0.01)+
  geom_segment(data = TSoil_3sec_PCload, aes(x = 0, y = 0, xend = (PC1*10), yend = (PC2*10)), arrow = arrow(length = unit(1/2, "picas")), color = "grey30", , alpha = .5) +
  annotate("text", x = (TSoil_3sec_PCload$PC1*11), y = (TSoil_3sec_PCload$PC2*11), label = TSoil_3sec_PCload$Variable,  color = "grey30", alpha = .5)+
  xlab("PC1: 39.67%")+ #x axis label text
  ylab("PC2: 28.87%")+ # y axis label text
  guides(color = guide_legend(override.aes = list(size = 1.5, alpha = 0.1) ) )
TSoil_Fert_PC23 <- ggplot(TSoil_3sec_PCval, aes(x = PC3, y = PC2, colour = SoilFert)) +
  geom_point(size = 0.1, alpha = 0.05) +
  scale_colour_okabe_ito()+
  stat_ellipse(geom="polygon", level=0.95, alpha=0.01)+
  geom_segment(data = TSoil_3sec_PCload, aes(x = 0, y = 0, xend = (PC3*10), yend = (PC2*10)), arrow = arrow(length = unit(1/2, "picas")), color = "grey30", , alpha = .5) +
  annotate("text", x = (TSoil_3sec_PCload$PC3*11), y = (TSoil_3sec_PCload$PC2*11), label = TSoil_3sec_PCload$Variable,  color = "grey30", alpha = .5)+
  xlab("PC3: 14.12")+ #x axis label text
  ylab("PC2: 28.87%")+ # y axis label text
  guides(color = guide_legend(override.aes = list(size = 1.5, alpha = 0.1) ) )
TSoil_Fert_PC123 <- ggarrange(TSoil_Fert_PC12+rremove("ylab"), TSoil_Fert_PC23+rremove("ylab"), 
                              ncol = 2, nrow = 1, common.legend = TRUE, legend = "top", align = "hv")
ggsave(TSoil_Fert_PC123, filename = "Output/SoilData/TSoil_Fert_PC123.png", width = 3000, height = 2000, unit = "px")
ggsave(TSoil_Fert_PCA, filename = "Output/SoilData/TSoil_Fert_PCA.png", width = 3000, height = 2000, unit = "px")


### Not using this part onwards ####
# theta <- 30
# phi <- 0
# 
# qplot(x=0, y=0, z=0, geom="blank") + 
#   axes_3D(theta=theta, phi=phi) +
#   stat_3D(theta=theta, phi=phi) +
#   # axis_labs_3D(theta=theta, phi=phi, size=3, 
#   #              hjust=c(1,1,1.2,1.2,1.2,1.2), 
#   #              vjust=c(-.5,-.5,-.2,-.2,1.2,1.2)) +
#   labs_3D(theta=theta, phi=phi, 
#           hjust=c(1,0,0), vjust=c(1.5,1,-.2),
#           labs=c("PC1: 39.67%", "PC2: 28.87%", "PC3: 14.12"))
# 
# TSoil_Fert_PCA <- ggplot(TSoil_3sec_PCval, aes(x = PC1, y = PC2, z = PC3, colour = SoilFert)) +
#   axes_3D(theta=theta, phi=phi) +
#   stat_3D(theta=theta, phi=phi) +
#   axis_labs_3D(theta=theta, phi=phi, size=3, 
#                hjust=c(1,1,1.2,1.2,1.2,1.2), 
#                vjust=c(-.5,-.5,-.2,-.2,1.2,1.2)) +
#   labs_3D(theta=theta, phi=phi, 
#           hjust=c(1,0,0), vjust=c(1.5,1,-.2),
#           labs=c("PC1: 39.67%", "PC2: 28.87%", "PC3: 14.12")) +
#   geom_point(size = 0.1, alpha = 0.05) +
#   scale_colour_okabe_ito()+
#   stat_ellipse(geom="polygon", level=0.95, alpha=0.01)+
#   geom_segment(data = TSoil_PCload, aes(x = 0, y = 0, xend = (PC1*10), yend = (PC2*10)), arrow = arrow(length = unit(1/2, "picas")), color = "grey30", , alpha = .5) +
#   annotate("text", x = (TSoil_PCload$PC1*11), y = (TSoil_PCload$PC2*11), label = TSoil_PCload$Variable,  color = "grey30", alpha = .5)+
#   guides(color = guide_legend(override.aes = list(size = 1.5, alpha = 0.1) ) )
# 
# TSoil_SType_PCA <- ggplot(TSoil_PCval, aes(x = PC1, y = PC2, colour = SoilType)) +
#   geom_point(size = 0.1, alpha = 0.05) +
#   scale_colour_viridis(discrete = TRUE)+
#   stat_ellipse(geom="polygon", level=0.95, alpha=0.01)+
#   geom_segment(data = TSoil_PCload, aes(x = 0, y = 0, xend = (PC1*10), yend = (PC2*10)), arrow = arrow(length = unit(1/2, "picas")),  color = "grey30", alpha = .5) +
#   annotate("text", x = (TSoil_PCload$PC1*11), y = (TSoil_PCload$PC2*11), label = TSoil_PCload$Variable, color = "grey30", alpha = .5)+
#   xlab("PC1: 39.67%")+ #x axis label text
#   ylab("PC2: 28.87%")+ # y axis label text
#   guides(color = guide_legend(override.aes = list(size = 1.5, alpha = 0.1) ) )
# ggsave(TSoil_SType_PCA, filename = "Output/SoilData/TSoil_SType_PCA.png", width = 3000, height = 2000, unit = "px")
# 
# 
# 
# ZStats_CovsC_LB_cl_tab <- ggtexttable(ZStats_CovsC_LB_cl, theme = ttheme("blank")) %>%
#   tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) %>% 
#   tab_add_hline(at.row = 10:11, row.side = "bottom", linewidth = 1) %>% 
#   tab_add_vline(at.column = c(1), column.side = "right", from.row = 2)
# ggsave("ZStats_CovsC_LB_cl_tab.png", ZStats_CovsC_LB_cl_tab, width = 3000, height = 2000, units = "px", dpi = 300, bg = 'white')
# 
# eig.val<-get_eigenvalue(PCA_TSoil)
# 
# SoilStack_NSW1 <- rast("SoilData/SoilStack_NSW1.tif")
# SoilStack_NSW2 <- rast("SoilData/SoilStack_NSW2.tif")
# SoilStack_NSW <- rast(list(SoilStack_NSW1, SoilStack_NSW2)) %>% writeRaster("SoilData/SoilStack_NSW.tif", overwrite = TRUE)
# SoilStack_NSW <- rast("SoilData/SoilStack_NSW.tif")
# names(SoilStack_NSW)
# SoilStack_Min <- subset(SoilStack_NSW, c("NSW_SOCpc0_30_mean_220901reduced","NSW_CEC0_30_mean_220901reduced", "NSW_SumBase0_30_mean_220901reduced", "NSW_Pbray0_30_mean_220901reduced", "NSW_BD0_30_mean_220901reduced", "NSW_pH0_30_mean_220901reduced"))
# PCA1 <- princomp(values(SoilStack_Min),  fix_sign = TRUE)
# 
# 
# SoilStack_SLGA <- rast("SoilData/SoilStack_SLGA.tif")
# # SoilFert <- terra::rast("input/covariates/soil_fert.tif")
# SoilFert_3sec <- rast("SoilData/SoilFert_3sec.tif")
# SoilFert_3sec_df <- as.data.frame(SoilFert_3sec) %>% mutate(Fert_code = as.factor(Fert_code))
# SoilStack_NSW1_df <- as.data.frame(SoilStack_NSW1)
# # SoilStack_NSW2_df <- as.data.frame(SoilStack_NSW2)
# 
# SoilStack_NSW1_df <- cbind(SoilFert_3sec_df, SoilStack_NSW1_df)
# mod1 <- lm(Fert_code ~ NSW_SOCpc0_30_mean_220901reduced , data = SoilStack_NSW1_df)
# summary(mod1)
# plot(mod1)
# 
# ASoilStack <- rast(
#   list(
#     rast("SoilData/DSM_NSW/NSW_SOCpc0_30.tif"), rast("SoilData/DSM_NSW/NSW_SOCpc30_60.tif"), rast("SoilData/DSM_NSW/NSW_SOCpc60_100.tif"),
#     rast("SoilData/DSM_NSW/NSW_PH0_30.tif"), rast("SoilData/DSM_NSW/NSW_PH30_60.tif"), rast("SoilData/DSM_NSW/NSW_PH60_100.tif"), rast("SoilData/DSM_NSW/NSW_PH100_200.tif"),
#     rast("SoilData/DSM_NSW/NSW_CEC0_30.tif"), rast("SoilData/DSM_NSW/NSW_CEC30_60.tif"), rast("SoilData/DSM_NSW/NSW_CEC60_100.tif"), rast("SoilData/DSM_NSW/NSW_CEC100_200.tif"),
#     rast("SoilData/DSM_NSW/NSW_SumBase0_30.tif"), rast("SoilData/DSM_NSW/NSW_SumBase30_60.tif"), rast("SoilData/DSM_NSW/NSW_SumBase60_100.tif"), rast("SoilData/DSM_NSW/NSW_SumBase100_200.tif"),
#     rast("SoilData/DSM_NSW/NSW_Pbray0_30.tif"), rast("SoilData/DSM_NSW/NSW_Pbray30_60.tif"), rast("SoilData/DSM_NSW/NSW_Pbray60_100.tif"), rast("SoilData/DSM_NSW/NSW_Pbray100_200.tif"),
#     rast("SoilData/DSM_NSW/NSW_BD0_30.tif"), 
#     rast("SoilData/DSM_NSW/NSW_Clay0_30.tif"), rast("SoilData/DSM_NSW/NSW_Clay30_60.tif"), rast("SoilData/DSM_NSW/NSW_Clay60_100.tif"), rast("SoilData/DSM_NSW/NSW_Clay100_200.tif"),
#     rast("SoilData/DSM_NSW/NSW_Silt0_30.tif"), rast("SoilData/DSM_NSW/NSW_Silt30_60.tif"), rast("SoilData/DSM_NSW/NSW_Silt60_100.tif"), rast("SoilData/DSM_NSW/NSW_Silt100_200.tif"),
#     rast("SoilData/DSM_NSW/NSW_Sand0_30.tif"), rast("SoilData/DSM_NSW/NSW_Sand30_60.tif"), rast("SoilData/DSM_NSW/NSW_Sand60_100.tif"), rast("SoilData/DSM_NSW/NSW_Sand100_200.tif"),
#     rast("SoilData/DSM_NSW/NSW_FSand0_30.tif"), rast("SoilData/DSM_NSW/NSW_FSand30_60.tif"), rast("SoilData/DSM_NSW/NSW_FSand60_100.tif"), rast("SoilData/DSM_NSW/NSW_FSand100_200.tif")
#   ))
# 
# names(ASoilStack) <- c("SOC_0_30", "SOC_30_60", "SOC_60_100", 
#                        "PH_0_30", "PH_30_60", "PH_60_100", "PH_100_200",
#                        "CEC_0_30", "CEC_30_60", "CEC_60_100", "CEC_100_200",
#                        "SOB_0_30", "SOB_30_60", "SOB_60_100", "SOB_100_200",
#                        "AVP_0_30", "AVP_30_60", "AVP_60_100", "AVP_100_200",
#                        "BD_0_30",
#                        "Clay_0_30", "Clay_30_60", "Clay_60_100", "Clay_100_200",
#                        "Silt_0_30", "Silt_30_60", "Silt_60_100", "Silt_100_200",
#                        "Sand_0_30", "Sand_30_60", "Sand_60_100", "Sand_100_200",
#                        "FSand_0_30", "FSand_30_60", "FSand_60_100", "FSand_100_200")
# ASoilStack_df <- as.data.frame(ASoilStack,  xy = TRUE)
# qsave(ASoilStack_df, "SoilData/ASoilStack_df.qs")
# SoilFert_3sec <- rast("SoilData/SoilFert_3sec.tif")
# SoilFert_3sec_df <- as.data.frame(rast("SoilData/SoilFert_3sec.tif"), xy = TRUE)
# SoilType_3sec <- rast("input/Covariates/soil_type.tif") %>% resample(SoilFert_3sec, method = "mode", threads = TRUE) %>% crop(SoilFert_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster("SoilData/SoilType_3sec.tif")
# SoilType_3sec_df <- as.data.frame(SoilType_3sec, xy = TRUE)
# ASoilStack_df <- ASoilStack_df %>% left_join(SoilFert_3sec_df, by = c("x", "y")) %>% left_join(SoilType_3sec_df, by = c("x", "y")) %>% drop_na() %>% qsave("SoilData/ASoilStack_df.qs")
# 
# rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
# gc() #free up memrory and report the memory usage.
# 
# ASoilStack_df <- qread("SoilData/ASoilStack_df.qs")
# 
# ptm <- proc.time()
# PCA_ASoil <- prcomp(ASoilStack_df[,3:38], scale = TRUE)
# proc.time() -ptm
# 
# ASoilStack_df$Fert_code <- as.factor(ASoilStack_df$Fert_code)
# ASoilStack_df$ASC_code <- as.factor(ASoilStack_df$ASC_code)
# PCA_ASoil
# summary(PCA_ASoil)
# 
# sum(PCA_ASoil$x[,1])
# 
# # Extract PC axes for plotting
# ASoilStack_PCval <- data.frame(FertCode = ASoilStack_df$Fert_code, SoilCode = ASoilStack_df$ASC_code, PCA_ASoil$x)
# 
# # Extract loadings of the variables
# ASoilStack_PCload <- data.frame(Variables = rownames(PCA_ASoil$rotation), PCA_ASoil$rotation)
# 
# sum(ASoilStack_PCload$PC1)
# 
# # Plot
# PCA_ASoil_plot1 <- ggplot(ASoilStack_PCval, aes(x = PC1, y = PC2, colour = FertCode)) +
#   geom_point(size = 0.1, alpha = 0.05) +
#   ggokabeito::scale_color_okabe_ito()+
#   stat_ellipse(geom="polygon", level=0.95, alpha=0.01)+
#   # geom_segment(data = ASoilStack_PCload, aes(x = 0, y = 0, xend = (PC1*20), yend = (PC2*20)), arrow = arrow(length = unit(1/2, "picas")), color = "red") +
#   # annotate("text", x = (ASoilStack_PCload$PC1*21), y = (ASoilStack_PCload$PC2*21),label = ASoilStack_PCload$Variables)+
#   xlab("PC1: 50.29%")+ #x axis label text
#   ylab("PC2: 25.45%")+ # y axis label text
#   guides(color = guide_legend(override.aes = list(size = 1.5, alpha = 0.1) ) )
# ggsave(PCA_ASoil_plot1, filename = "SoilData/PCA_ASoil_plot11.png", width = 3000, height = 2000, unit = "px")
# 
# PCA_ASoil_plot2 <- ggplot(ASoilStack_PCval, aes(x = PC1, y = PC2, colour = SoilCode)) +
#   geom_point(size = 0.1, alpha = 0.05) +
#   scale_colour_viridis(discrete = TRUE)+
#   stat_ellipse(geom="polygon", level=0.95, alpha=0.01)+
#   # geom_segment(data = ASoilStack_PCload, aes(x = 0, y = 0, xend = (PC1*20), yend = (PC2*20)), arrow = arrow(length = unit(1/2, "picas")), color = "red") +
#   # annotate("text", x = (ASoilStack_PCload$PC1*21), y = (ASoilStack_PCload$PC2*21), label = ASoilStack_PCload$Variables)+
#   xlab("PC1: 50.29%")+ #x axis label text
#   ylab("PC2: 25.45%")+ # y axis label text
#   guides(color = guide_legend(override.aes = list(size = 1.5, alpha = 0.1) ) )
# ggsave(PCA_ASoil_plot2, filename = "SoilData/PCA_ASoil_plot21.png", width = 3000, height = 2000, unit = "px")
# unique(ASoilStack_df$ASC_code)
