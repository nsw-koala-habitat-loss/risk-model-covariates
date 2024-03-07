import netCDF4
import numpy
import arcpy
import requests
import os
from arcpy.ia import *

dirname = os.path.dirname(os.path.realpath(__file__))

years = range(2011, 2020, 1)

arcpy.CheckOutExtension("Spatial")

# Download to SILO database
silo_url = "https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/annual"
variable = "monthly_rain"

f_list = []
for year in years:
    fname = "{}.{}.nc".format(year, variable)
    path = os.path.join(dirname, "../data/prec", fname)
    f_list.append(fname)

    if os.path.isfile(path):
        # Skip download if the file already exists
        continue
    download_url = silo_url + "/" + variable + "/" + fname
    print(download_url)
    r = requests.get(download_url)
    open(path, 'wb').write(r.content)

workspace = os.path.join(dirname, "../intermediate_files/cov_workspace.gdb")
arcpy.env.overwriteOutput = True
if not arcpy.Exists(workspace):
    location, name = os.path.split(os.path.abspath(workspace))
    arcpy.management.CreateFileGDB(location, name)
arcpy.env.workspace = os.path.join(dirname, "../intermediate_files/cov_workspace.gdb")
    
rast_list = []
for fname in f_list:
    path = os.path.join(dirname, "../data/prec", fname)
    out_file = os.path.splitext(fname)[0].replace(".","_")
    rast_list.append(out_file)
    arcpy.md.MakeNetCDFRasterLayer(path,"monthly_rain","lon", "lat", out_file)
    arcpy.management.CopyRaster(out_file, os.path.join(dirname, "../data/prec", out_file+".tif"))

rc = RasterCollection(rast_list)

mean_raster = Mean(rasters = rc, process_as_multiband=True, extent_type="UnionOf", cellsize_type="MinOf", ignore_nodata="True")

mean_raster.save(os.path.join(dirname, "../data/prec", "monthly_rain.tif"))