import arcpy
import os
import pathlib
from covariates import Covariate

dirname = os.path.dirname(os.path.realpath(__file__))

# Merge NSW and Sydney-specific land-use
nsw_landuse = os.path.join(dirname, "../data/landuse/landnswlanduse2007/NSWLanduse_2007shp.shp")
syd_landuse = os.path.join(dirname, "../data/landuse/landnswlanduse2007/LanduseSydney5haPolygonsOrGreater.shp")
landuse = os.path.join(dirname, "../intermediate_files/landuse.shp")

if not arcpy.Exists(landuse):
    arcpy.management.Merge([nsw_landuse, syd_landuse], landuse)

# Create new covariate class
landuse_cov = Covariate(landuse, "landuse", "LU_ALUM7Co")
landuse_cov.rasteriseShp()