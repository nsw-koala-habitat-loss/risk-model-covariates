from covariates import Covariate
import os
import arcpy

# Calculate distance from significant urban areas

dirname = os.path.dirname(os.path.realpath(__file__))
sua_shp = os.path.join(dirname, r"../data/dist_sua/SUA_2021_AUST_GDA2020_SHP/SUA_2021_AUST_GDA2020.shp")
sua_nsw_shp = os.path.join(dirname, r"../intermediate_files/sua_nsw.shp")
sua_nsw = arcpy.management.SelectLayerByAttribute(sua_shp, "NEW_SELECTION", "SUA_CODE21 >= '1001' And SUA_CODE21 < '2000'")
arcpy.management.CopyFeatures(sua_nsw, sua_nsw_shp)

sua_cov = Covariate(sua_nsw_shp, "dist_sua", "")
sua_cov.euclideanDistRaster()

#sua_cov.clipFeatures(os.path.join(dirname, "../intermediate_files/dist_sua_unclipped.tif"), 
#                     os.path.join(dirname, "../output/dist_sua.tif"))