# Data processing pipeline for covariates in the deforestation risk model

Replicable data processing pipeline that converts input layers into a consistent set of raster files with the common grid.

## Requirements
* R (terra, sf, tidyverse, ps)
* RStudio (for batch processing using RStudio API)

## Covariates and their corresponding scripts

|Covariate|Script|
|-|-|
|Agricultural profit|[AgriculturalProfit.R](code/R/AgriProfit.R)|
|Distance to road|[Distance to city & road](code/R/DistRoadCity.R)|
|Distance to urban centre|[Distance to city & road](code/R/DistRoadCity.R)|
|Ecological condition|[EcologicalCondition](code/R/ecol_cond.R)|
|Elevation|[Slope & elevation](code/R/Slope_Elevation.R)|
|Population density|Socio economic:Part 1(code/R/SocioEcon_2016.R)|
|Population growth|Socio economic:Part 2(code/R/SocioEcon_2016.R)|
|Parcel size|*Calculated during model fitting*|
|Land value|[Property value](code/R/PropVal.R)|
|Rainfall|[Climate](code/R/climate.R)|
|Slope|[Slope & elevation](code/R/Slope_Elevation.R)|
|Socio-economic PCA|[Socio economic:Part 3](code/R/SocioEcon_2016.R)|
|Soil PCA|[Soil PCA](code/R/Soil.R)|
|Temperature|[Climate](code/R/climate.R)|
|Drought history|[Drought]()(code/R/drought.R)|
|Fire history|[Fire](code/R/fire.R)|
|Forest tenure, class, type|[Forest](code/R/forest_tenure.R)|
|Land tenure|[Land tenure](code/R/LandTenure.R)|
|Land use type|[Land use](code/R/landuse.R)|
|Native Vegetation Regulatory(NVR)|[NVR Map](code/R/NativeVegReg.R)|
|Planning zones|[Planning zones](code/R/PlanningZones.R)|
|Political preference|[Political preference](code/R/PoliticPref.R)|

