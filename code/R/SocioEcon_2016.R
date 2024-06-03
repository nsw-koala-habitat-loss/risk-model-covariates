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
library(Rcpp)
library(exactextractr)
library(factoextra)
library(ggpubr)

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
Woody_template <- rast("Input/Woody_template.tif")

# load 2016 mesh block to calculate population density
MB_shp_2016 <- vect("Input/2016_GCP_SA1_for_NSW_short-header/2016 Mesh Block/1270055001_mb_2016_nsw_shape/MB_2016_NSW.shp") %>% 
  project(crs(Woody)) %>% 
  tidyterra::select(MB_CODE16)


# load 2016 mesh block census data
MB_census_2016 <- read_csv("Input/2016_GCP_SA1_for_NSW_short-header/2016 Mesh Block/2016 census mesh block counts.csv")
Pop_2016_vect <- MB_shp_2016 %>% 
  tidyterra::left_join(MB_census_2016, by = join_by("MB_CODE16" == "MB_CODE_2016")) %>%
  tidyterra::mutate(PopDen = ifelse(Person==0|AREA_ALBERS_SQKM==0,0, Person/AREA_ALBERS_SQKM)) %>%
  tidyterra::drop_na() %>% 
  tidyterra::select(MB_CODE16 , Person16 = Person , PopDen)

writeVector(Pop_2016_vect, "Output/Shapefile/Pop_2016.shp", overwrite = TRUE)

PopDen_2016 <- rasterize(Pop_2016_vect, Woody, field = "PopDen") %>% 
  crop(Woody, snap = "out", mask = TRUE)
names(PopDen_2016) <- "PopDen"
writeRaster(PopDen_2016, "Output/Raster/PopDen_2016.tif")

PopNum_2016 <- rasterize(Pop_2016_vect, Woody, field = "Person16") %>% 
  crop(Woody, snap = "out", mask = TRUE)


# load 2011 mesh block to calculate population density
MB_shp_2011 <- vect("Input/2011_BCP_SA1_for_NSW_short-header/1270055001_mb_2011_nsw_shape/MB_2011_NSW.shp") %>% 
  project(crs(Woody)) %>% 
  tidyterra::filter(MB_CAT11 != "NOUSUALRESIDENCE") %>% 
  tidyterra::select(MB_CODE11)

# load 2011 mesh block census data
MB_census_2011 <- read_csv("Input/2011_BCP_SA1_for_NSW_short-header/censuscounts_mb_2011_aust.csv")
Pop_2011_vect <- MB_shp_2011 %>% 
  tidyterra::left_join(MB_census_2011, by = join_by("MB_CODE11" == "Mesh_Block_ID")) %>%
  tidyterra::select(MB_CODE11 , Person11 = Persons_Usually_Resident)

PopNum_2011 <- rasterize(Pop_2011_vect, Woody, field = "Person11") %>% 
  crop(Woody, snap = "out", mask = TRUE)

Rcpp::cppFunction("
NumericVector Cal_PopGrowth(NumericVector Person11, NumericVector Person16) {
    int n = Person11.size();
    NumericVector pop_growth(n);
    
    for(int i = 0; i < n; ++i) {
        if (Person11[i] == 0) {
            pop_growth[i] = Person16[i];  // Assign Population of 2016 to population growth when zero population in 2011 (prevent return Inf)
        } else if (Person16[i] - Person11[i] == 0) {
            pop_growth[i] = 0;   // Assign 0 if there is no change in population (prevent return NaN)
        } else {
            pop_growth[i] = (Person16[i] - Person11[i]) / Person11[i] ;
        }
    }
    return pop_growth;
  }
  ")

PopGro_2016 <- rast(list(PopNum_2011, PopNum_2016)) %>% 
  lapp(fun = Cal_PopGrowth, filename = "Output/Raster/PopGro_2016.tif", overwrite  = TRUE)

# load SA1s spatial layer
SA1s_All <- st_read("Input/2016_GCP_SA1_for_NSW_short-header/2016_SA1_shape/SA1_2016_AUST.shp") %>% 
  st_transform(crs = crs(Woody)) %>%
  filter(STATE_NAME == "New South Wales") %>%
  mutate(Shape_Area = as.numeric(st_area(.)),
         Area = Shape_Area / 1e6)



# import census data at SA1 level

# ## Population density
# PopDen <- read_csv("Input/2021_GCP_all_for_NSW_short-header/2021 Census GCP All Geographies for NSW/SA1/NSW/2021Census_G01_NSW_SA1.csv") %>% 
#   dplyr::select(SA1_CODE_2021, Tot_P_P) %>%  mutate(SA1_CODE_2021 = as.character(SA1_CODE_2021)) %>% 
#   left_join(SA1s_All, by = c("SA1_CODE_2021" = "SA1_CODE21")) %>%
#   dplyr::mutate(PopDen = Tot_P_P/Area) %>% 
#   dplyr::select(SA1_CODE_2021, PopDen)

## Employment
PEmp <- read_csv("Input/2016_GCP_SA1_for_NSW_short-header/2016 Census GCP Statistical Area 1 for NSW/2016Census_G43B_NSW_SA1.csv") %>%
  dplyr::select(SA1_7DIGITCODE_2016, P_Tot_Emp_Tot, P_Tot_LF_Tot) %>% 
  dplyr::mutate(PEmp = ifelse(P_Tot_Emp_Tot== 0 | P_Tot_LF_Tot == 0, 0, P_Tot_Emp_Tot / P_Tot_LF_Tot)) %>% select(SA1_7DIGITCODE_2016,PEmp)

PAgFrFhEmp <- bind_cols(read_csv("Input/2016_GCP_SA1_for_NSW_short-header/2016 Census GCP Statistical Area 1 for NSW/2016Census_G53A_NSW_SA1.csv"),
                        read_csv("Input/2016_GCP_SA1_for_NSW_short-header/2016 Census GCP Statistical Area 1 for NSW/2016Census_G53B_NSW_SA1.csv") %>% select(-SA1_7DIGITCODE_2016)) %>%
  dplyr::select(SA1_7DIGITCODE_2016, Agri_for_fish_Tot, Tot_Tot, ID_NS_Tot) %>%
  dplyr::mutate( PAgFrFhEmp = ifelse(Agri_for_fish_Tot==0 | Tot_Tot - ID_NS_Tot == 0, 0, Agri_for_fish_Tot / (Tot_Tot - ID_NS_Tot))) %>% select(PAgFrFhEmp)

## Income
HInc <- read_csv("Input/2016_GCP_SA1_for_NSW_short-header/2016 Census GCP Statistical Area 1 for NSW/2016Census_G02_NSW_SA1.csv") %>% 
  dplyr::select(HInc = Median_tot_hhd_inc_weekly)

## Education
PYr12Ed <- read_csv("Input/2016_GCP_SA1_for_NSW_short-header/2016 Census GCP Statistical Area 1 for NSW/2016Census_G01_NSW_SA1.csv") %>% 
  dplyr::select(SA1_7DIGITCODE_2016, High_yr_schl_comp_Yr_12_eq_P, High_yr_schl_comp_Yr_11_eq_P, High_yr_schl_comp_Yr_10_eq_P, High_yr_schl_comp_Yr_9_eq_P, High_yr_schl_comp_Yr_8_belw_P, High_yr_schl_comp_D_n_g_sch_P, Tot_P_P) %>%
  dplyr::mutate(PYr12Ed = ifelse(High_yr_schl_comp_Yr_12_eq_P ==0  | High_yr_schl_comp_Yr_12_eq_P + High_yr_schl_comp_Yr_11_eq_P + High_yr_schl_comp_Yr_10_eq_P + High_yr_schl_comp_Yr_9_eq_P + High_yr_schl_comp_Yr_8_belw_P + High_yr_schl_comp_D_n_g_sch_P ==0, 0,
                                                       High_yr_schl_comp_Yr_12_eq_P / (High_yr_schl_comp_Yr_12_eq_P + High_yr_schl_comp_Yr_11_eq_P + High_yr_schl_comp_Yr_10_eq_P + High_yr_schl_comp_Yr_9_eq_P + High_yr_schl_comp_Yr_8_belw_P + High_yr_schl_comp_D_n_g_sch_P))) %>% select(SA1_7DIGITCODE_2016, PYr12Ed) %>% 
  dplyr::select(PYr12Ed)

PBachEd <- read_csv("Input/2016_GCP_SA1_for_NSW_short-header/2016 Census GCP Statistical Area 1 for NSW/2016Census_G46B_NSW_SA1.csv") %>% 
  dplyr::select(SA1_7DIGITCODE_2016, P_PGrad_Deg_Total, P_GradDip_and_GradCert_Total, P_BachDeg_Total, P_Tot_Total, P_Lev_Edu_NS_Total, P_Lev_Edu_IDes_Total) %>%
  dplyr::mutate(PBachEd = ifelse(P_PGrad_Deg_Total + P_GradDip_and_GradCert_Total + P_BachDeg_Total == 0 | P_Tot_Total - P_Lev_Edu_NS_Total - P_Lev_Edu_IDes_Total == 0, 0,
    (P_PGrad_Deg_Total + P_GradDip_and_GradCert_Total + P_BachDeg_Total) / (P_Tot_Total - P_Lev_Edu_NS_Total - P_Lev_Edu_IDes_Total))) %>% select(SA1_7DIGITCODE_2016, PBachEd) %>%
  dplyr::select(PBachEd)

## Demographics

Demographics <- bind_cols(
  read_csv("Input/2016_GCP_SA1_for_NSW_short-header/2016 Census GCP Statistical Area 1 for NSW/2016Census_G01_NSW_SA1.csv"),
  read_csv("Input/2016_GCP_SA1_for_NSW_short-header/2016 Census GCP Statistical Area 1 for NSW/2016Census_G02_NSW_SA1.csv"),
  read_csv("Input/2016_GCP_SA1_for_NSW_short-header/2016 Census GCP Statistical Area 1 for NSW/2016Census_G25_NSW_SA1.csv"),
  read_csv("Input/2016_GCP_SA1_for_NSW_short-header/2016 Census GCP Statistical Area 1 for NSW/2016Census_G08_NSW_SA1.csv"))%>% 
  dplyr::select(Birthplace_Australia_P, Birthplace_Elsewhere_P, Lang_spoken_home_Eng_only_P, Lang_spoken_home_Oth_Lang_P, Median_age_persons, 
                Average_household_size, Median_mortgage_repay_monthly, Median_rent_weekly, Tot_P_P, Tot_P_BP_B_OS, Tot_P_Tot_Resp, Tot_P_BP_NS,
                CF_ChU15_a_Total_P, CF_no_ChU15_a_Total_P, OPF_ChU15_a_Total_P, OPF_no_ChU15_a_Total_P, Total_P) %>%
  dplyr::mutate(PBirthAus = ifelse(Birthplace_Australia_P ==0 | Birthplace_Elsewhere_P == 0, 0, Birthplace_Australia_P / (Birthplace_Australia_P + Birthplace_Elsewhere_P)), 
                PEngLang = ifelse(Lang_spoken_home_Eng_only_P == 0 | Lang_spoken_home_Oth_Lang_P == 0, 0,  Lang_spoken_home_Eng_only_P / (Lang_spoken_home_Eng_only_P + Lang_spoken_home_Oth_Lang_P)), 
                Age = Median_age_persons, 
                HSize = Average_household_size, 
                MortPay = Median_mortgage_repay_monthly, 
                Rent = Median_rent_weekly, 
                PBornOS = ifelse(Tot_P_BP_B_OS == 0| Tot_P_Tot_Resp - Tot_P_BP_NS == 0, 0, Tot_P_BP_B_OS / (Tot_P_Tot_Resp - Tot_P_BP_NS)),
                PFamCompCU15 = ifelse(CF_ChU15_a_Total_P==0 | Total_P==0, 0, CF_ChU15_a_Total_P / Total_P), 
                PFamCompCx15 = ifelse(CF_no_ChU15_a_Total_P==0 | Total_P==0, 0, CF_no_ChU15_a_Total_P / Total_P), 
                PFamCompOU15 = ifelse(OPF_ChU15_a_Total_P==0 | Total_P==0, 0, OPF_ChU15_a_Total_P / Total_P), 
                PFamCompOx15 = ifelse(OPF_no_ChU15_a_Total_P==0 | Total_P==0, 0, OPF_no_ChU15_a_Total_P / Total_P)) %>%
  dplyr::select(PBirthAus, PEngLang, Age, HSize, MortPay, Rent, PBornOS, PFamCompCU15, PFamCompCx15, PFamCompOU15, PFamCompOx15)

ScEcData_2016 <- bind_cols(PEmp, PAgFrFhEmp, HInc, PYr12Ed, PBachEd, Demographics)
qsave(ScEcData_2016, "Output/ScEcData_2016.qs")

ScEcData16 <- qread("Output/ScEcData_2016.qs")
SA1_shp_2016 <- vect("Input/2016_GCP_SA1_for_NSW_short-header/2016_SA1_shape/SA1_2016_AUST.shp") %>% 
  tidyterra::filter(STATE_NAME == "New South Wales") %>% 
  project(crs(Woody)) %>%
  tidyterra::select(SA1_7DIGIT)

ScEcData16_vect <- SA1_shp_2016 %>% 
  tidyterra::mutate(SA1_7DIGIT = as.numeric(SA1_7DIGIT)) %>% 
  tidyterra::left_join(ScEcData16, by = join_by(SA1_7DIGIT == SA1_7DIGITCODE_2016 )) %>% 
  tidyterra::select(PEmp, PAgFrFhEmp, HInc, PYr12Ed, PBachEd, PBirthAus, PEngLang, Age, HSize, MortPay, Rent, PBornOS, PFamCompCU15, PFamCompCx15, PFamCompOU15, PFamCompOx15)

ScEcData16_rast <- rast(list(
  rasterize(Pop_2016_vect, Woody, field = "PopDen"),
  PopGro_2016,
  rasterize(ScEcData16_vect, Woody, fun = "mean", field = "PEmp"),
  rasterize(ScEcData16_vect, Woody, fun = "mean", field = "PAgFrFhEmp"),
  rasterize(ScEcData16_vect, Woody, fun = "mean", field = "HInc"),
  rasterize(ScEcData16_vect, Woody, fun = "mean", field = "PYr12Ed"),
  rasterize(ScEcData16_vect, Woody, fun = "mean", field = "PBachEd"),
  rasterize(ScEcData16_vect, Woody, fun = "mean", field = "PBirthAus"),
  rasterize(ScEcData16_vect, Woody, fun = "mean", field = "PEngLang"),
  rasterize(ScEcData16_vect, Woody, fun = "mean", field = "Age"),
  rasterize(ScEcData16_vect, Woody, fun = "mean", field = "HSize"),
  rasterize(ScEcData16_vect, Woody, fun = "mean", field = "MortPay"),
  rasterize(ScEcData16_vect, Woody, fun = "mean", field = "Rent"),
  rasterize(ScEcData16_vect, Woody, fun = "mean", field = "PBornOS"),
  rasterize(ScEcData16_vect, Woody, fun = "mean", field = "PFamCompCU15"),
  rasterize(ScEcData16_vect, Woody, fun = "mean", field = "PFamCompCx15"),
  rasterize(ScEcData16_vect, Woody, fun = "mean", field = "PFamCompOU15"),
  rasterize(ScEcData16_vect, Woody, fun = "mean", field = "PFamCompOx15")
  )) %>% 
  crop(Woody, snap = "out", mask = TRUE)

# export raster
writeRaster(ScEcData16_rast, "Output/Raster/ScEcData_2016.tif", overwrite = TRUE)


# PCA for ScEcData_2016 ---- 

# load woody raster as template
Woody <- rast("Input/woody_nsw.tif")
Woody_template <- rast("Input/Woody_template.tif")

# load 2016 mesh block to calculate population density
MB_shp_2016 <- vect("Input/2016_GCP_SA1_for_NSW_short-header/2016 Mesh Block/1270055001_mb_2016_nsw_shape/MB_2016_NSW.shp") %>% 
  project(crs(Woody)) %>% 
  crop(Woody) %>%
  tidyterra::select(MB_CODE16)
MB_shp_2016_sf <- st_as_sf(MB_shp_2016)
# plot(MB_shp_2016)
ScEcData16_rast <- rast("Output/Raster/ScEcData_2016.tif")

ScEcData16_MB2016 <- exactextractr::exact_extract(ScEcData16_rast, MB_shp_2016_sf, fun = "mean")
ScEcData16_MB2016 <- cbind(MB_shp_2016_sf, ScEcData16_MB2016)
colnames(ScEcData16_MB2016)[2:19] <-  gsub(glob2rx("mean.*"), "", colnames(ScEcData16_MB2016)[2:19])
colnames(ScEcData16_MB2016)[3] <- "PopGro"
qsave(ScEcData16_MB2016, "Output/ScEcData16_MB2016.qs")

# Check for NAs and how to deal with them.
# ScEcData16_MB2016_NA <- ScEcData16_MB2016 %>% 
#   dplyr::filter(is.na(PEmp) | is.na(PAgFrFhEmp) | is.na(HInc) | is.na(PYr12Ed) | is.na(PBachEd) | is.na(PBirthAus) | is.na(PEngLang) | is.na(Age) | is.na(HSize) | is.na(MortPay) | is.na(Rent) | is.na(PBornOS) | is.na(PFamCompCU15) | is.na(PFamCompCx15) | is.na(PFamCompOU15) | is.na(PFamCompOx15)
#   )
# ScEcData16_MB2016_NA_v <- vect(ScEcData16_MB2016_NA) 
# ScEcData16_MB2016_NA_v2 <- ScEcData16_MB2016_NA_v %>% 
#   select(MB_CODE16) %>%
#   mutate(AREA = expanse(.))
# ScEcData16_MB2016_NA_v2 %>% filter(AREA > 1478374)
# ScEcData16_MB2016_NA_ex <- extract(ScEcData16_rast, ScEcData16_MB2016_NA_v, fun = function(x) mean(x, na.rm = T))

ScEcData16_MB2016_tb <- as_tibble(ScEcData16_MB2016) %>% drop_na()
summary(ScEcData16_MB2016_tb)

ptm <- proc.time()
PCA_ScEc_MB16 <- prcomp(ScEcData16_MB2016_tb[,2:19], scale = TRUE)
proc.time() -ptm
summary(PCA_ScEc_MB16)
qsave(PCA_ScEc_MB16, "Output/PCA_ScEc_MB16.qs")

PCA_ScEc_MB16_var <- fviz_eig(PCA_ScEc_MB16, choice = "variance", addlabels = TRUE, ggtheme = theme_pubr())
PCA_ScEc_MB16_eig <- fviz_eig(PCA_ScEc_MB16, choice = "eigenvalue", addlabels = TRUE, ggtheme = theme_pubr())
PCA_ScEc_MB16_var12 <- fviz_pca_var(PCA_ScEc_MB16, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, theme = theme_pubr())
PCA_ScEc_MB16_var34 <- fviz_pca_var(PCA_ScEc_MB16, axes = c(3, 4), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
PCA_ScEc_MB16_var56 <- fviz_pca_var(PCA_ScEc_MB16, axes = c(5, 6), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
get_pca_ind(PCA_ScEc_MB16)
PCA_ScEc_MB16$x[,1:7]
PCA_ScEc_MB16_plot <- ggarrange(PCA_ScEc_MB16_var, PCA_ScEc_MB16_eig, PCA_ScEc_MB16_var12, PCA_ScEc_MB16_var34, PCA_ScEc_MB16_var56, ncol = 2, nrow = 3)
ggsave("Output/PCA_ScEc_MB16_plot.png", PCA_ScEc_MB16_plot, width = 4000, height = 6000, dpi = 300, units = "px")

# Extract PC 
ScEc_MB16_PCval <- ScEcData16_MB2016_tb[,1] %>% 
  cbind(PCA_ScEc_MB16$x)
qsave(ScEc_MB16_PCval, "Output/ScEc_MB16_PCval.qs")

# Extract loadings of the variables
ScEc_MB16_PCload <- rownames_to_column(data.frame(PCA_ScEc_MB16$rotation), "Variable")
rownames(ScEc_MB16_PCload) <- NULL
qsave(ScEc_MB16_PCload, "Output/ScEc_MB16_PCload.qs")

ScEc_MB16_PCval_v <- MB_shp_2016 %>% 
  tidyterra::left_join(ScEc_MB16_PCval, by = join_by("MB_CODE16" == "MB_CODE16")) %>%
  tidyterra::select(MB_CODE16, PC1, PC2, PC3, PC4, PC5, PC6, PC7)

ScEc_MB16_PCval_r <- rast(list(
  rasterize(ScEc_MB16_PCval_v, Woody, field = "PC1"),
  rasterize(ScEc_MB16_PCval_v, Woody, field = "PC2"),
  rasterize(ScEc_MB16_PCval_v, Woody, field = "PC3"),
  rasterize(ScEc_MB16_PCval_v, Woody, field = "PC4"),
  rasterize(ScEc_MB16_PCval_v, Woody, field = "PC5"),
  rasterize(ScEc_MB16_PCval_v, Woody, field = "PC6"),
  rasterize(ScEc_MB16_PCval_v, Woody, field = "PC7")
)) %>% 
  crop(Woody, snap = "out", mask = TRUE)

writeRaster(ScEc_MB16_PCval_r, "Output/Raster/ScEc_PCA.tif", overwrite = TRUE)

