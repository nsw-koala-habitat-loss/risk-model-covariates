# Purpose: To process socioeconomic data 

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

# Load Libraries
library(tidyverse)
library(tidyterra)
library(terra)
library(sf)
library(stringr)
library(qs)

# Socioeconomic covariates ----
## Population density
## Employment / unemployment: PEmp, PAgFrFhEmp
## Household income: Median_tot_hhd_inc_weekly, 
## Education: PYear12Ed, PBachEd
## Demographics: PBirthAus, PEngLang, Age, MortPay, HSize, PBornOS, 
### Couple_family_with_children_under_15_and_Total_Persons 
### Couple_family_with_no_children_under_15_and_Total_Persons
### One_parent_family_with_children_under_15_and_Total_Persons
### One_parent_family_with_no_children_under_15_and_Total_Persons
## Population Growth**
## Political preference
## Nature Relatedness

# load woody raster as template
Woody <- rast("Input/woody_nsw.tif")

# load SA1s spatial layer
SA1s_All <- st_read("Input/sa1s.gdb/", layer = "sa1s") %>% 
  mutate(Shape_Area = as.numeric(st_area(.)),
         Area = Shape_Area / 1e6)

# import census data at SA1 level

## Population density
PopDen <- read_csv("Input/2021_GCP_all_for_NSW_short-header/2021 Census GCP All Geographies for NSW/SA1/NSW/2021Census_G01_NSW_SA1.csv") %>% 
  dplyr::select(SA1_CODE_2021, Tot_P_P) %>%  mutate(SA1_CODE_2021 = as.character(SA1_CODE_2021)) %>% 
  left_join(SA1s_All, by = c("SA1_CODE_2021" = "SA1_CODE21")) %>%
  dplyr::mutate(PopDen = Tot_P_P/Area) %>% 
  dplyr::select(SA1_CODE_2021, PopDen)

## Employment
PEmp <- read_csv("Input/2021_GCP_all_for_NSW_short-header/2021 Census GCP All Geographies for NSW/SA1/NSW/2021Census_G46B_NSW_SA1.csv") %>%
  dplyr::select(SA1_CODE_2021, P_Tot_Emp_Tot, P_Tot_LF_Tot) %>% 
  dplyr::mutate(PEmp = ifelse(P_Tot_Emp_Tot== 0 | P_Tot_LF_Tot == 0, 0, P_Tot_Emp_Tot / P_Tot_LF_Tot)) %>% select(PEmp)

PAgFrFhEmp <- bind_cols(read_csv("Input/2021_GCP_all_for_NSW_short-header/2021 Census GCP All Geographies for NSW/SA1/NSW/2021Census_G56A_NSW_SA1.csv"),
                        read_csv("Input/2021_GCP_all_for_NSW_short-header/2021 Census GCP All Geographies for NSW/SA1/NSW/2021Census_G56B_NSW_SA1.csv") %>% select(-SA1_CODE_2021)) %>%
  dplyr::select(SA1_CODE_2021, Agri_for_fish_Tot, Tot_Tot, ID_NS_Tot) %>%
  dplyr::mutate( PAgFrFhEmp = ifelse(Agri_for_fish_Tot==0 | Tot_Tot - ID_NS_Tot == 0, 0, Agri_for_fish_Tot / (Tot_Tot - ID_NS_Tot))) %>% select(PAgFrFhEmp)

## Income
HInc <- read_csv("Input/2021_GCP_all_for_NSW_short-header/2021 Census GCP All Geographies for NSW/SA1/NSW/2021Census_G02_NSW_SA1.csv") %>% 
  dplyr::select(HInc = Median_tot_hhd_inc_weekly)

## Education
PYr12Ed <- read_csv("Input/2021_GCP_all_for_NSW_short-header/2021 Census GCP All Geographies for NSW/SA1/NSW/2021Census_G01_NSW_SA1.csv") %>% 
  dplyr::select(SA1_CODE_2021, High_yr_schl_comp_Yr_12_eq_P, High_yr_schl_comp_Yr_11_eq_P, High_yr_schl_comp_Yr_10_eq_P, High_yr_schl_comp_Yr_9_eq_P, High_yr_schl_comp_Yr_8_belw_P, High_yr_schl_comp_D_n_g_sch_P, Tot_P_P) %>%
  dplyr::mutate(PYr12Ed = ifelse(High_yr_schl_comp_Yr_12_eq_P ==0  | High_yr_schl_comp_Yr_12_eq_P + High_yr_schl_comp_Yr_11_eq_P + High_yr_schl_comp_Yr_10_eq_P + High_yr_schl_comp_Yr_9_eq_P + High_yr_schl_comp_Yr_8_belw_P + High_yr_schl_comp_D_n_g_sch_P ==0, 0,
                                                       High_yr_schl_comp_Yr_12_eq_P / (High_yr_schl_comp_Yr_12_eq_P + High_yr_schl_comp_Yr_11_eq_P + High_yr_schl_comp_Yr_10_eq_P + High_yr_schl_comp_Yr_9_eq_P + High_yr_schl_comp_Yr_8_belw_P + High_yr_schl_comp_D_n_g_sch_P))) %>% select(SA1_CODE_2021, PYr12Ed) %>% 
  dplyr::select(PYr12Ed)

PBachEd <- read_csv("Input/2021_GCP_all_for_NSW_short-header/2021 Census GCP All Geographies for NSW/SA1/NSW/2021Census_G49B_NSW_SA1.csv") %>% 
  dplyr::select(SA1_CODE_2021, P_PGrad_Deg_Total, P_GradDip_and_GradCert_Total, P_BachDeg_Total, P_Tot_Total, P_Lev_Edu_NS_Total, P_Lev_Edu_IDes_Total) %>%
  dplyr::mutate(PBachEd = ifelse(P_PGrad_Deg_Total + P_GradDip_and_GradCert_Total + P_BachDeg_Total == 0 | P_Tot_Total - P_Lev_Edu_NS_Total - P_Lev_Edu_IDes_Total == 0, 0,
    (P_PGrad_Deg_Total + P_GradDip_and_GradCert_Total + P_BachDeg_Total) / (P_Tot_Total - P_Lev_Edu_NS_Total - P_Lev_Edu_IDes_Total))) %>% select(SA1_CODE_2021, PBachEd) %>%
  dplyr::select(PBachEd)

## Demographics

Demographics <- bind_cols(
  read_csv("Input/2021_GCP_all_for_NSW_short-header/2021 Census GCP All Geographies for NSW/SA1/NSW/2021Census_G01_NSW_SA1.csv"),
  read_csv("Input/2021_GCP_all_for_NSW_short-header/2021 Census GCP All Geographies for NSW/SA1/NSW/2021Census_G02_NSW_SA1.csv"),
  read_csv("Input/2021_GCP_all_for_NSW_short-header/2021 Census GCP All Geographies for NSW/SA1/NSW/2021Census_G29_NSW_SA1.csv"),
  read_csv("Input/2021_GCP_all_for_NSW_short-header/2021 Census GCP All Geographies for NSW/SA1/NSW/2021Census_G08_NSW_SA1.csv"))%>% 
  dplyr::select(Birthplace_Australia_P, Birthplace_Elsewhere_P, Lang_used_home_Eng_only_P, Lang_used_home_Oth_Lang_P, Median_age_persons, 
                Average_household_size, Median_mortgage_repay_monthly, Median_rent_weekly, Tot_P_P, Tot_P_BP_B_OS, Tot_P_Tot_resp, Tot_P_BP_NS,
                CF_ChU15_a_Total_P, CF_no_ChU15_a_Total_P, OPF_ChU15_a_Total_P, OPF_no_ChU15_a_Total_P, Total_P) %>%
  dplyr::mutate(PBirthAus = ifelse(Birthplace_Australia_P ==0 | Birthplace_Elsewhere_P == 0, 0, Birthplace_Australia_P / (Birthplace_Australia_P + Birthplace_Elsewhere_P)), 
                PEngLang = ifelse(Lang_used_home_Eng_only_P == 0 | Lang_used_home_Oth_Lang_P == 0, 0,  Lang_used_home_Eng_only_P / (Lang_used_home_Eng_only_P + Lang_used_home_Oth_Lang_P)), 
                Age = Median_age_persons, 
                HSize = Average_household_size, 
                MortPay = Median_mortgage_repay_monthly, 
                Rent = Median_rent_weekly, 
                PBornOS = ifelse(Tot_P_BP_B_OS == 0| Tot_P_Tot_resp - Tot_P_BP_NS == 0, 0, Tot_P_BP_B_OS / (Tot_P_Tot_resp - Tot_P_BP_NS)),
                PFamCompCU15 = ifelse(CF_ChU15_a_Total_P==0 | Total_P==0, 0, CF_ChU15_a_Total_P / Total_P), 
                PFamCompCx15 = ifelse(CF_no_ChU15_a_Total_P==0 | Total_P==0, 0, CF_no_ChU15_a_Total_P / Total_P), 
                PFamCompOU15 = ifelse(OPF_ChU15_a_Total_P==0 | Total_P==0, 0, OPF_ChU15_a_Total_P / Total_P), 
                PFamCompOx15 = ifelse(OPF_no_ChU15_a_Total_P==0 | Total_P==0, 0, OPF_no_ChU15_a_Total_P / Total_P)) %>%
  dplyr::select(PBirthAus, PEngLang, Age, HSize, MortPay, Rent, PBornOS, PFamCompCU15, PFamCompCx15, PFamCompOU15, PFamCompOx15)

ScEcData <- bind_cols(PopDen, PEmp, PAgFrFhEmp, HInc, PYr12Ed, PBachEd, Demographics)
qsave(ScEcData, "Output/ScEcData.qs")

ScEcData <- qread("Output/ScEcData.qs")
SA1s_All <- vect("Input/sa1s.gdb/", layer = "sa1s")

ScEcData_vect <- SA1s_All %>% 
  left_join(ScEcData, by = join_by(SA1_CODE21 ==SA1_CODE_2021 )) %>% 
  select(PopDen, PEmp, PAgFrFhEmp, HInc, PYr12Ed, PBachEd, PBirthAus, PEngLang, Age, HSize, MortPay, Rent, PBornOS, PFamCompCU15, PFamCompCx15, PFamCompOU15, PFamCompOx15)

ggplot()+geom_spatvector(data = ScEcData_vect, aes(fill = PopDen))

ScEcData_rast <- rast(list(
  rasterize(ScEcData_vect, Woody, fun = "mean", field = "PopDen"),
  rasterize(ScEcData_vect, Woody, fun = "mean", field = "PEmp"),
  rasterize(ScEcData_vect, Woody, fun = "mean", field = "PAgFrFhEmp"),
  rasterize(ScEcData_vect, Woody, fun = "mean", field = "HInc"),
  rasterize(ScEcData_vect, Woody, fun = "mean", field = "PYr12Ed"),
  rasterize(ScEcData_vect, Woody, fun = "mean", field = "PBachEd"),
  rasterize(ScEcData_vect, Woody, fun = "mean", field = "PBirthAus"),
  rasterize(ScEcData_vect, Woody, fun = "mean", field = "PEngLang"),
  rasterize(ScEcData_vect, Woody, fun = "mean", field = "Age"),
  rasterize(ScEcData_vect, Woody, fun = "mean", field = "HSize"),
  rasterize(ScEcData_vect, Woody, fun = "mean", field = "MortPay"),
  rasterize(ScEcData_vect, Woody, fun = "mean", field = "Rent"),
  rasterize(ScEcData_vect, Woody, fun = "mean", field = "PBornOS"),
  rasterize(ScEcData_vect, Woody, fun = "mean", field = "PFamCompCU15"),
  rasterize(ScEcData_vect, Woody, fun = "mean", field = "PFamCompCx15"),
  rasterize(ScEcData_vect, Woody, fun = "mean", field = "PFamCompOU15"),
  rasterize(ScEcData_vect, Woody, fun = "mean", field = "PFamCompOx15")
  ))


ggplot()+ geom_spatraster(data = ScEcData_rast$PopDen)

# export raster
writeRaster(ScEcData_rast, "Output/ScEcData.tif", overwrite = TRUE)

summary(ScEcData)

# load spatial property units for each KMR
SUs <- list(CC = st_read("Input/lots_kmrs.gdb/", layer = "Central_Coast"),
            CST = st_read("Input/lots_kmrs.gdb/", layer = "Central_Southern_Tablelands"),
            DRP = st_read("Input/lots_kmrs.gdb/", layer = "Darling_Riverine_Plains"),
            FW = st_read("Input/lots_kmrs.gdb/", layer = "Far_West"),
            NC = st_read("Input/lots_kmrs.gdb/", layer = "North_Coast"),
            NT = st_read("Input/lots_kmrs.gdb/", layer = "Northern_Tablelands"),
            NS = st_read("Input/lots_kmrs.gdb/", layer = "Northwest_Slopes"),
            R = st_read("Input/lots_kmrs.gdb/", layer = "Riverina"),
            SC = st_read("Input/lots_kmrs.gdb/", layer = "South_Coast"))

# join ScEcData to SUs
ScEcData_SUs <- SUs
for(i in names(ScEcData_SUs)){
  ScEcData_SUs[[i]] <- ScEcData_SUs[[i]] %>% 
    left_join(ScEcData, by = join_by(SA1==SA1_CODE_2021)) %>% 
    mutate(Shape_Area = as.numeric(st_area(.)),
           Area = Shape_Area / 1e6) %>% 
    as_tibble() %>% 
    select(Area, PopDen, PEmp, PAgFrFhEmp, HInc, PYr12Ed, PBachEd, PBirthAus, PEngLang, Age, MortPay, HSize, PBornOS) 
}

qsave(ScEcData_SUs, "Output/ScEcData_SUs.qs")
