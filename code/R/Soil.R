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
library(factoextra)
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

# load data
## Define directories
INPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Input"
OUTPUT_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Output"
OUTPUT_FIG_DIR <- "D:/Data/NSW_Deforestation/risk-model-covariates/Figures"
## load woody raster as template
Woody <- rast(file.path(INPUT_DIR, "woody_nsw.tif"))
Woody_template <- rast(file.path(INPUT_DIR, "Woody_template.tif"))

# Project the soil data to the same CRS as Woody
SOCpc_0_30 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_SOCpc0_30_mean_220901reduced.tif")) %>% 
  project(y = crs(Woody), method = "bilinear", threads = TRUE)

# Soil data processing at 3 second arc resolution (raw data resolution) to reduce processing time
# Resample Woody to the same origin and resolution as the soil data and use it as reference layer
Woody_3sec <- terra::resample(Woody, SOCpc_0_30, method = "mode", threads = TRUE) %>% 
  writeRaster(file.path(INPUT_DIR , "SoilData/Woody_3sec.tif"), overwrite = TRUE)
# SoilFert_3sec <- terra::resample(SoilFert, SOCpc_0_30, method = "mode", threads = TRUE) %>%  crop(y=SOCpc_0_30, snap = "out", mask = TRUE, threads = TRUE) %>%
#   writeRaster("SoilData/SoilFert_3sec.tif", overwrite = TRUE)

Woody_3sec <- rast(file.path(INPUT_DIR , "SoilData/Woody_3sec.tif"))
Woody_3sec_df <- as.data.frame(Woody_3sec,xy =TRUE)
gc()

SOC_0_30 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_SOCpc0_30_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_SOCpc0_30.tif"), overwrite = TRUE)
SOC_30_60 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_SOCpc30_60_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_SOCpc30_60.tif"), overwrite = TRUE)
SOC_60_100 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_SOCpc60_100_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE)%>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_SOCpc60_100.tif"), overwrite = TRUE)
gc()
PH_0_30 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_PH0_30_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_PH0_30.tif"), overwrite = TRUE)
PH_30_60 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_PH30_60_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_PH30_60.tif"), overwrite = TRUE)
PH_60_100 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_PH60_100_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_PH60_100.tif"), overwrite = TRUE)
PH_100_200 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_PH100_200_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_PH100_200.tif"), overwrite = TRUE)
gc()
CEC_0_30 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_CEC0_30_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE)  %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_CEC0_30.tif"), overwrite = TRUE)
CEC_30_60 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_CEC30_60_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_CEC30_60.tif"), overwrite = TRUE)
CEC_60_100 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_CEC60_100_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_CEC60_100.tif"), overwrite = TRUE)
CEC_100_200 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_CEC100_200_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_CEC100_200.tif"), overwrite = TRUE)
gc()
SOB_0_30 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_SumBase0_30_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_SumBase0_30.tif"), overwrite = TRUE)
SOB_30_60 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_SumBase30_60_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_SumBase30_60.tif"), overwrite = TRUE)
SOB_60_100 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_SumBase60_100_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_SumBase60_100.tif"), overwrite = TRUE)
SOB_100_200 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_SumBase100_200_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_SumBase100_200.tif"), overwrite = TRUE)
gc()
AVP_0_30 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_Pbray0_30_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_Pbray0_30.tif"), overwrite = TRUE)
AVP_30_60 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_Pbray30_60_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_Pbray30_60.tif"), overwrite = TRUE)
AVP_60_100 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_Pbray60_100_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_Pbray60_100.tif"), overwrite = TRUE)
AVP_100_200 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_Pbray100_200_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_Pbray100_200.tif"), overwrite = TRUE)
gc()
BD_0_30 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_BD0_30_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_BD0_30.tif"), overwrite = TRUE)
gc()
clay_0_30 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_Clay0_30_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_Clay0_30.tif"), overwrite = TRUE)
clay_30_60 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_Clay30_60_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_Clay30_60.tif"), overwrite = TRUE)
clay_60_100 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_Clay60_100_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_Clay60_100.tif"), overwrite = TRUE)
clay_100_200 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_Clay100_200_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_Clay100_200.tif"), overwrite = TRUE)
gc()
Silt_0_30 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_Silt0_30_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_Silt0_30.tif"), overwrite = TRUE)
Silt_30_60 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_Silt30_60_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_Silt30_60.tif"), overwrite = TRUE)
Silt_60_100 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_Silt60_100_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_Silt60_100.tif"), overwrite = TRUE)
Silt_100_200 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_Silt100_200_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_Silt100_200.tif"), overwrite = TRUE)
gc()
Sand_0_30 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_Sand0_30_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_Sand0_30.tif"), overwrite = TRUE)
Sand_30_60 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_Sand30_60_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_Sand30_60.tif"), overwrite = TRUE)
Sand_60_100 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_Sand60_100_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_Sand60_100.tif"), overwrite = TRUE)
Sand_100_200 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_Sand100_200_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_Sand100_200.tif"), overwrite = TRUE)
gc()
FSand_0_30 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_FSand0_30_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_FSand0_30.tif"), overwrite = TRUE)
FSand_30_60 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_FSand30_60_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_FSand30_60.tif"), overwrite = TRUE)
FSand_60_100 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_FSand60_100_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_FSand60_100.tif"), overwrite = TRUE)
FSand_100_200 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/DSM_NSW/NSW_FSand100_200_mean_220901reduced.tif"))%>% project(y = Woody_3sec, method = "bilinear", threads = TRUE)%>%  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/NSW_FSand100_200.tif"), overwrite = TRUE)
gc()

# SoilList <- list(SOC_0_30, SOC_30_60, SOC_60_100, CEC_0_30, CEC_30_60, CEC_60_100, CEC_100_200, BD_0_30, Silt_0_30, Silt_30_60, Silt_60_100, Silt_100_200, Sand_0_30, Sand_30_60, Sand_60_100, Sand_100_200, PH_0_30, PH_30_60, PH_60_100, PH_100_200, SOB_0_30, SOB_30_60, SOB_60_100, SOB_100_200, AVP_0_30, AVP_30_60, AVP_60_100, AVP_100_200, clay_0_30, clay_30_60, clay_60_100, clay_100_200, FSand_0_30, FSand_30_60, FSand_60_100, FSand_100_200)
# lapply(SoilList, function(x){crs(x)})

# SoilStack_NSW <- rast(list(SOC_0_30, SOC_30_60, SOC_60_100, CEC_0_30, CEC_30_60, CEC_60_100, CEC_100_200, BD_0_30, Silt_0_30, Silt_30_60, Silt_60_100, Silt_100_200, Sand_0_30, Sand_30_60, Sand_60_100, Sand_100_200, PH_0_30, PH_30_60, PH_60_100, PH_100_200, SOB_0_30, SOB_30_60, SOB_60_100, SOB_100_200, AVP_0_30, AVP_30_60, AVP_60_100, AVP_100_200, clay_0_30, clay_30_60, clay_60_100, clay_100_200, FSand_0_30, FSand_30_60, FSand_60_100, FSand_100_200))
# writeRaster(SoilStack_NSW, "SoilData/SoilStack_NSW.tif")

# NSW Soil Data from SLGA----
# Download relevant soil data not available in SEED from SLGA

# SLGA APIKEY
# usethis::edit_r_environ()
TERN_APIkey <- Sys.getenv("TERN_APIkey")
if (TERN_APIkey == "") {
  stop("API key not found. Please assign TERN_APIkey or set TERN_APIkey in .Renviron")
}

apikey <- paste0('apikey:', TERN_APIkey)
AWC_000_005 <- rast(paste0('/vsicurl/https://',apikey,'@data.tern.org.au/landscapes/slga/NationalMaps/SoilAndLandscapeGrid/AWC/AWC_000_005_EV_N_P_AU_TRN_N_20210614.tif'))


# Extract extent of the raster
Woody <- terra::rast(file.path(INPUT_DIR , "woody_nsw.tif"))
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
    writeRaster(paste0(file.path(INPUT_DIR , "SoilData/Raw_download/SLGA/"), substr(x, start = 5, stop = nchar(x))), overwrite = TRUE)
})
plan(sequential)
proc.time() - ptm

# SLGA_fl <- list.files("SoilData/SLGA/",pattern = "\\.tif$", full.names = TRUE, recursive = FALSE)
# SoilStack_SLGA <- rast(SLGA_fl)
# names(SoilStack_SLGA) <- c(substr(SLGA_Fname, start = 5, stop = 15))
# SoilStack_SLGA <- project(SoilStack_SLGA, crs(SoilFert_3sec), method = "bilinear", threads = TRUE) %>% 
# resample(SoilFert_3sec, method = "bilinear", threads = TRUE) %>% 
# writeRaster("SoilData/SoilStack_SLGA.tif")

Woody_3sec <- rast(file.path(INPUT_DIR , "SoilData/Woody_3sec.tif"))

AWC_0_30 <- mean(
  rast(
    list(
      rast(file.path(INPUT_DIR , "SoilData/Raw_download/SLGA/AWC_000_005_EV_N_P_AU_TRN_N_20210614.tif")),
      rast(file.path(INPUT_DIR , "SoilData/Raw_download/SLGA/AWC_005_015_EV_N_P_AU_TRN_N_20210614.tif")), 
      rast(file.path(INPUT_DIR , "SoilData/Raw_download/SLGA/AWC_015_030_EV_N_P_AU_TRN_N_20210614.tif"))))) %>% 
  project(y = Woody_3sec, method = "bilinear", threads = TRUE) %>% 
  resample(y = Woody_3sec, method = "bilinear", threads = TRUE) %>% 
  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% 
  writeRaster(file.path(OUTPUT_DIR, "SoilData/AWC_0_30.tif"), overwrite = TRUE)

NTO_0_30 <- mean(
  rast(
    list(
      rast(file.path(INPUT_DIR , "SoilData/Raw_download/SLGA/NTO_000_005_EV_N_P_AU_NAT_C_20140801.tif")),
      rast(file.path(INPUT_DIR , "SoilData/Raw_download/SLGA/AWC_005_015_EV_N_P_AU_TRN_N_20210614.tif")),
      rast(file.path(INPUT_DIR , "SoilData/Raw_download/SLGA/AWC_015_030_EV_N_P_AU_TRN_N_20210614.tif"))))) %>% 
  project(y = Woody_3sec, method = "bilinear", threads = TRUE) %>% 
  resample(y = Woody_3sec, method = "bilinear", threads = TRUE) %>% 
  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% 
  writeRaster(file.path(OUTPUT_DIR, "SoilData/NTO_0_30.tif"), overwrite = TRUE)

DES_0_200 <- rast(file.path(INPUT_DIR , "SoilData/Raw_download/SLGA/DES_000_200_EV_N_P_AU_TRN_C_20190901.tif")) %>% 
  project(y = Woody_3sec, method = "bilinear", threads = TRUE) %>% 
  resample(y = Woody_3sec, method = "bilinear", threads = TRUE) %>% 
  crop(y=Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% 
  writeRaster(file.path(OUTPUT_DIR, "SoilData/DES_0_200.tif"), overwrite = TRUE)

# Incorporate Soil Type and Soil Fertility data
SoilType_3sec <- rast(file.path(INPUT_DIR , "SoilData/soil_type.tif")) %>% resample(Woody_3sec, method = "mode", threads = TRUE) %>% crop(Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/SoilType_3sec.tif"), overwrite = TRUE)
SoilFert_3sec <- rast(file.path(INPUT_DIR , "SoilData/soil_fert.tif")) %>% resample(Woody_3sec, method = "mode", threads = TRUE) %>% crop(Woody_3sec, snap = "out", mask = TRUE, threads = TRUE) %>% writeRaster(file.path(OUTPUT_DIR, "SoilData/SoilFert_3sec.tif"), overwrite = TRUE)

# Read in all Raster and create raster stack

SOC_0_30 <- rast(file.path(OUTPUT_DIR, "SoilData/NSW_SOCpc0_30.tif"))
PH_0_30 <- rast(file.path(OUTPUT_DIR, "SoilData/NSW_PH0_30.tif"))
CEC_0_30 <- rast(file.path(OUTPUT_DIR, "SoilData/NSW_CEC0_30.tif"))
SOB_0_30 <- rast(file.path(OUTPUT_DIR, "SoilData/NSW_SumBase0_30.tif"))
AVP_0_30 <- rast(file.path(OUTPUT_DIR, "SoilData/NSW_Pbray0_30.tif"))
BD_0_30 <- rast(file.path(OUTPUT_DIR, "SoilData/NSW_BD0_30.tif"))
AWC_0_30 <- rast(file.path(OUTPUT_DIR, "SoilData/AWC_0_30.tif"))
NTO_0_30 <- rast(file.path(OUTPUT_DIR, "SoilData/NTO_0_30.tif"))
DES_0_200 <- rast(file.path(OUTPUT_DIR, "SoilData/DES_0_200.tif"))
clay_0_30 <- rast(file.path(OUTPUT_DIR, "SoilData/NSW_Clay0_30.tif"))
Silt_0_30 <- rast(file.path(OUTPUT_DIR, "SoilData/NSW_Silt0_30.tif"))
Sand_0_30 <- rast(file.path(OUTPUT_DIR, "SoilData/NSW_Sand0_30.tif"))
Fsand_0_30 <- rast(file.path(OUTPUT_DIR, "SoilData/NSW_FSand0_30.tif"))
SoilFert_3sec <- rast(file.path(OUTPUT_DIR, "SoilData/SoilFert_3sec.tif"))
SoilType_3sec <- rast(file.path(OUTPUT_DIR, "SoilData/SoilType_3sec.tif"))
Woody_3sec <- rast(file.path(INPUT_DIR , "SoilData/Woody_3sec.tif"))


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
  qsave(file.path(OUTPUT_DIR, "SoilData/TSoilStack_3sec_df.qs"))

# PCA for Top 30cm soil----

TSoilStack_3sec_df <- qread(file.path(OUTPUT_DIR, "SoilData/TSoilStack_3sec_df.qs")) %>%
  drop_na()



ptm <- proc.time()
PCA_TSoil_3sec <- prcomp(TSoilStack_3sec_df[,3:15], scale = TRUE)
proc.time() -ptm

qsave(PCA_TSoil_3sec, file.path(OUTPUT_DIR, "SoilData/PCA_TSoil_3sec.qs"))

PCA_TSoil_3sec <- qread(file.path(OUTPUT_DIR, "SoilData/PCA_TSoil_3sec.qs"))
print(PCA_TSoil_3sec)
summary(PCA_TSoil_3sec)
head(PCA_TSoil_3sec$x)
nrow(PCA_TSoil_3sec$x)

# Extract PC axes for plotting
TSoil_3sec_PCval <- TSoilStack_3sec_df %>% 
  select(x, y, SoilFert, SoilType) %>%
  cbind(PCA_TSoil_3sec$x)
qsave(TSoil_3sec_PCval, file.path(OUTPUT_DIR, "SoilData/TSoil_3sec_PCval.qs"))

# Extract loadings of the variables
TSoil_3sec_PCload <- rownames_to_column(data.frame(PCA_TSoil_3sec$rotation), "Variable")
rownames(TSoil_3sec_PCload) <- NULL
qsave(TSoil_3sec_PCload, file.path(OUTPUT_DIR, "SoilData/TSoil_3sec_PCload.qs"))
TSoil_3sec_PCload <- qread(file.path(OUTPUT_DIR, "SoilData/TSoil_3sec_PCload.qs"))

#Covert PC1, PC2 & PC3 to raster
TSoilPC_3sec <- rast(TSoil_3sec_PCval[,c(1,2,5,6,7)], type = "xyz", crs = crs(Woody_3sec)) 
writeRaster(TSoilPC_3sec, file.path(OUTPUT_DIR, "SoilData/TSoilPC_3sec.tif", overwrite = TRUE))

Woody_3sec <- rast(file.path(INPUT_DIR, "SoilData/Woody_3sec.tif"))
Woody <- rast(file.path(INPUT_DIR, "woody_nsw.tif"))
TSoilPC <- TSoilPC_3sec %>% resample(Woody, method = "bilinear", threads = TRUE) %>% crop(Woody, snap = "out", mask = TRUE)
names(TSoilPC) <- c("Soil_PC1", "Soil_PC2", "Soil_PC3")
writeRaster(TSoilPC, file.path(OUTPUT_DIR, "Raster/TSoilPC.tif"), overwrite = TRUE)

# Plotting----
TSoil_3sec_PCval <- qread(file.path(OUTPUT_DIR, "SoilData/TSoil_3sec_PCval.qs"))
TSoil_3sec_PCload <- qread(file.path(OUTPUT_DIR, "SoilData/TSoil_3sec_PCload.qs"))

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
ggsave(TSoil_Fert_PC123, filename = file.path(OUTPUT_FIG_DIR, "TSoil_Fert_PC123.png"), width = 3000, height = 2000, unit = "px")
ggsave(TSoil_Fert_PCA, filename = file.path(OUTPUT_FIG_DIR, "TSoil_Fert_PCA.png"), width = 3000, height = 2000, unit = "px")

TSoil_3sec_PCload_Tab <- TSoil_3sec_PCload %>% 
  mutate('Variable description' = c("Soil Organic Carbon content", 
                                    "Soil pH", 
                                    "Cation Exchange Capacity", 
                                    "Sum of Bases", 
                                    "Available Phosphorus", 
                                    "Bulk Density", 
                                    "Available Water Capacity",
                                    "Total Nitrogen",
                                    "Depth of Soil",
                                    "Clay content",
                                    "Silt content",
                                    "Sand content",
                                    "Fine Sand content"),
         across(where(is.numeric), ~round(.,4))) %>%
  select('Variable description', Variable, PC1, PC2, PC3) %>%
  # remove anything after _ in Variable names
  mutate(Variable = gsub("_.*", "", Variable))
  
  TSoil_3sec_PCload_Tab %>% 
  # mutate(PC1 = abs(PC1)) %>% 
  arrange(desc(PC3))
write.csv(TSoil_3sec_PCload_Tab, file.path(OUTPUT_DIR, "SoilData/TSoil_3sec_PCload_Tab.csv"), row.names = FALSE)

TSoil_PCA_var <- fviz_eig(PCA_TSoil_3sec, choice = "variance", addlabels = TRUE, ggtheme = theme_pubr())+
    theme(plot.title = element_blank()) + theme(axis.title = element_text(size=14), axis.text = element_text(size=12))

TSoil_PCA_eig <- fviz_eig(PCA_TSoil_3sec, choice = "eigenvalue", addlabels = TRUE, ggtheme = theme_pubr())+
    theme(plot.title = element_blank()) + theme(axis.title = element_text(size=14), axis.text = element_text(size=12))

rownames(PCA_TSoil_3sec$rotation) <- gsub("_.*", "", rownames(PCA_TSoil_3sec$rotation))

TSoil_PCA_var12 <- fviz_pca_var(PCA_TSoil_3sec, axes = c(1, 2), col.var = "contrib", 
                                      gradient.cols = hcl.colors(18, palette = "Viridis")[1:15], 
                                      repel = TRUE, labelsize = 6, theme = theme_pubr()) + 
  theme(plot.title = element_blank())+ labs(color = "Variable\ncontribution (%)")+
  theme(legend.position = "top", legend.key.width = unit(2, "cm")) +
  theme(axis.title = element_text(size=14), axis.text = element_text(size=12), legend.text = element_text(size=12), legend.title = element_text(size=14))

TSoil_PCA_var23 <- fviz_pca_var(PCA_TSoil_3sec, axes = c(2, 3), col.var = "contrib", 
                                      gradient.cols = hcl.colors(18, palette = "Viridis")[1:15], 
                                      repel = TRUE, labelsize = 6, theme = theme_pubr()) + 
  theme(plot.title = element_blank())+ labs(color = "Variable\ncontribution (%)")+
  theme(legend.position = "top", legend.key.width = unit(2, "cm")) +
  theme(axis.title = element_text(size=14), axis.text = element_text(size=12), legend.text = element_text(size=12), legend.title = element_text(size=14))
TSoil_PCA_plot <- (TSoil_PCA_var|TSoil_PCA_eig)/(TSoil_PCA_var12|TSoil_PCA_var23)+
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 16))

# TSoil_PCA_plot <- ggarrange(TSoil_PCA_var, TSoil_PCA_eig, TSoil_PCA_var12, TSoil_PCA_var23, ncol = 2, nrow = 2)

ggsave(filename = file.path(OUTPUT_FIG_DIR, "TSoil_PCA_plot.png"), plot = TSoil_PCA_plot, width = 4000, height = 4000, dpi = 300, units = "px")
