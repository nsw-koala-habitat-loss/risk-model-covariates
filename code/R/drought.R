library(tidyverse)
library(sf)

dirname <- "H://risk-model-covariates/code"

years <- 2016:2019
months <- 1:12

date_end_month <- seq(as.Date("20160201", format="%Y%m%d"),length=48,by="months")-1
date_end_month <- format(date_end_month, "%Y%m%d")

zipdir <- file.path(dirname, "../data/drought/GISlayers2016_2021")

# Initialize an empty vector to store folder names
foldernames <- c()

# List files in the specified directory (zipdir)
for (item in list.files(zipdir, '*.zip$')) {
  # Split the filename into root and extension
  file_parts <- strsplit(item, "\\.")[[1]]
  root <- file_parts[1]
  ext <- file_parts[2]
  
  # Append the root name to the foldernames vector
  foldernames <- c(foldernames, root)
  
  # Unpack the archive if the folder doesn't exist
  if (!file.exists(file.path(zipdir, root))) {
    unzip(file.path(zipdir, item), exdir = file.path(zipdir, root))
  }
  
  # Check if the .prj file exists in the folder
  if (!file.exists(file.path(zipdir, root, paste0(substr(root, 11, 30), ".prj")))) {
    file.copy(file.path(zipdir, "edis_xx.prj"), file.path(zipdir, root, paste0(substr(root, 11, 30), ".prj")))
    
    # Rename the .prj file
    file.rename(file.path(zipdir, root, "edis_xx.prj"), file.path(zipdir, root, paste0(substr(root, 11, 30), ".prj")))
  }
}


cdi <- foldernames[substr(foldernames, 16, 30) %in% date_end_month] %>%
  lapply(function(name) {
    shp <- sf::st_read(file.path(zipdir, name, paste0(substr(name, 11, 30),'.shp')))
    return(shp$CDI)
})

names(cdi) <- 1:length(cdi)
cdi_mode <- cdi %>%
  bind_cols() %>%
  apply(1, function(x) {
    u <- unique(x)
    tab <- tabulate(match(x, u))
    u[tab == max(tab)]
    max(u[tab == max(tab)])
  })

name <- foldernames[1]
cdi_shp <- sf::st_read(file.path(zipdir, name, paste0(substr(name, 11, 30),'.shp')))
cdi_shp$CDI <- cdi_mode
st_write(cdi_shp, "intermediate_data/cdi.shp", delete_dsn = T)
cdi_file <- file.path(dirname, "../intermediate_data/cdi.shp")
source('code/R/spatial_functions.R')
output <- cdi_file %>%
  projectShp(name = "drought") %>%
  shpToRast(name = "drought", field_name = "CDI") %>%
  resampleRast(name = "drought") %>%
  clipRast(name = "drought", to_output = TRUE)
