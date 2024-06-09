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

# Part1: Population Density----
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
writeRaster(PopDen_2016, "Output/Raster/PopDen16.tif")

# Part2: Population Growth----

Pop_2016_vect <- vect("Output/Shapefile/Pop_2016.shp")
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
NumericVector Cal_PopGrowth(NumericVector Person11, NumericVector Person16 ) {
    int n = Person11.size();
    NumericVector pop_growth(n);
    
    for(int i = 0; i < n; ++i) {
        if (Person11[i] == 0) {
            pop_growth[i] = Person16 [i];  // Assign Population of 2016 to population growth when zero population in 2011 (prevent return Inf)
        } else if (Person16 [i] - Person11[i] == 0) {
            pop_growth[i] = 0;   // Assign 0 if there is no change in population (prevent return NaN)
        } else {
            pop_growth[i] = (Person16 [i] - Person11[i]) / Person11[i] ;
        }
    }
    return pop_growth;
  }
  ")

PopNum_11_16 <- rast(list(PopNum_2011, PopNum_2016)) 

PopGro_2016 <- lapp(PopNum_11_16, fun = Cal_PopGrowth, filename = "Output/Raster/PopGro16.tif", overwrite = TRUE)



# Part3: Socioeconomic data ----

Woody <- rast("Input/woody_nsw.tif")
Woody_template <- rast("Input/Woody_template.tif")

# import census data at SA1 level
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

ScEcData16 <- bind_cols(PEmp, PAgFrFhEmp, HInc, PYr12Ed, PBachEd, Demographics)

# PCA for ScEcData_2016 ---- 

# Check for NAs and how to deal with them.
summary(ScEcData16)
ScEcData16 %>% summarise(across(everything(), ~sum(.==0))) %>% as.list()

### Remove variable PAgFrFhEmp (Percentage of Employment in Agriculture, Forestry and Fishing) as >70% SA1s have zero value. ###
ScEcData16 <- ScEcData16 %>% select(-PAgFrFhEmp)

SocioEcon16_PCA <- prcomp(ScEcData16[,2:16], scale = TRUE)
summary(SocioEcon16_PCA)
qsave(SocioEcon16_PCA, "Output/SocioEcon16_PCA.qs")
### First 5 PCs are needed to explain > 80% of the variation in the data.###
SocioEcon16_PCA <- qread("Output/SocioEcon16_PCA.qs")

SocioEcon16_PCA_var <- fviz_eig(SocioEcon16_PCA, choice = "variance", addlabels = TRUE, ggtheme = theme_pubr())
SocioEcon16_PCA_eig <- fviz_eig(SocioEcon16_PCA, choice = "eigenvalue", addlabels = TRUE, ggtheme = theme_pubr())
SocioEcon16_PCA_var12 <- fviz_pca_var(SocioEcon16_PCA, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, theme = theme_pubr())
SocioEcon16_PCA_var34 <- fviz_pca_var(SocioEcon16_PCA, axes = c(3, 4), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
SocioEcon16_PCA_var56 <- fviz_pca_var(SocioEcon16_PCA, axes = c(5, 6), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
get_pca_ind(SocioEcon16_PCA)
SocioEcon16_PCA$x[,1:5]
SocioEcon16_PCA_plot <- ggarrange(SocioEcon16_PCA_var, SocioEcon16_PCA_eig, SocioEcon16_PCA_var12, SocioEcon16_PCA_var34, SocioEcon16_PCA_var56, ncol = 2, nrow = 3)
ggsave("Output/SocioEcon16_PCA_plot.png", SocioEcon16_PCA_plot, width = 4000, height = 6000, dpi = 300, units = "px")

# Extract PC 
SocioEcon16_PCval <- cbind(ScEcData16[,1], SocioEcon16_PCA$x)
qsave(SocioEcon16_PCval, "Output/SocioEcon16_PCval.qs")

# Extract loadings of the variables
SocioEcon16_PCload <- rownames_to_column(data.frame(SocioEcon16_PCA$rotation), "Variable")
rownames(SocioEcon16_PCload) <- NULL
qsave(SocioEcon16_PCload, "Output/SocioEcon16_PCload.qs")

# Allocated PC values to Statistical area 1 SA1

SA1_shp_2016 <- vect("Input/2016_GCP_SA1_for_NSW_short-header/2016_SA1_shape/SA1_2016_AUST.shp") %>% 
  tidyterra::filter(STATE_NAME == "New South Wales") %>% 
  project(crs(Woody)) %>%
  tidyterra::mutate(SA1_7DIGIT = as.numeric(SA1_7DIGIT)) %>%
  tidyterra::select(SA1_7DIGIT)

SocioEcon16_PC_vect <- SA1_shp_2016 %>% 
  tidyterra::left_join(SocioEcon16_PCval, by = join_by("SA1_7DIGIT" == "SA1_7DIGITCODE_2016")) %>%
  tidyterra::select(SA1_7DIGIT, PC1, PC2, PC3, PC4, PC5)
names(SocioEcon16_PC_vect) <- c("SA1_7DIGIT", "ScEc_PC1", "ScEc_PC2", "ScEc_PC3", "ScEc_PC4", "ScEc_PC5")


SocioEcon16_PC_rast <- rast(list(
  rasterize(SocioEcon16_PC_vect, Woody, field = "ScEc_PC1"),
  rasterize(SocioEcon16_PC_vect, Woody, field = "ScEc_PC2"),
  rasterize(SocioEcon16_PC_vect, Woody, field = "ScEc_PC3"),
  rasterize(SocioEcon16_PC_vect, Woody, field = "ScEc_PC4"),
  rasterize(SocioEcon16_PC_vect, Woody, field = "ScEc_PC5")
)) %>% 
  crop(Woody, snap = "out", mask = TRUE)

writeRaster(SocioEcon16_PC_rast, "Output/Raster/SocioEcon16_PC.tif", overwrite = TRUE)
