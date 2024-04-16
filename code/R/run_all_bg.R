library(rstudioapi)

script_names <- c('landuse', 'income', 'pop_den', 'remoteness', 'fire', 'forest_tenure', 'soil_type', 'slope_elev', 'soil_nitrogen', 'soil_fert', 'climate', 'drought', 'ecol_cond')

for (name in script_names) {
  jobRunScript(
    file.path('code/R', paste0(name, '.R')),
    name = name,
    workingDir = 'H://risk-model-covariates',
    importEnv = FALSE,
    exportEnv = ""
  )
}


# Run climate for all years in parallel
for (y in 1988:2023) {
  yr <- c(y)
  jobRunScript(
    file.path('code/R/climate.R'),
    name = paste0('climate_', yr),
    workingDir = 'H://risk-model-covariates',
    importEnv = T,
    exportEnv = ""
  )
  Sys.sleep(.01)
}
