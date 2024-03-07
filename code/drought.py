import arcpy
import numpy as np
import pandas as pd
import calendar
import datetime
import shutil
import os
from scipy import stats
from covariates import Covariate

dirname = os.path.dirname(os.path.realpath(__file__))

arcpy.env.overwriteOutput = True

## Download drought data of the study period
years = np.arange(2016, 2019, 1)
months = np.arange(1,12, 1)

def last_day_of_month(year, month):
    next_month = month % 12 + 1
    if next_month == 1:
        year += 1
    last_day = datetime.date(year, next_month, 1) - datetime.timedelta(days=1)
    return last_day.strftime("%Y%m%d")

dates = [last_day_of_month(y,m) for y in years for m in months]

zipdir = r"H:\risk-model-covariates\data\drought\GISlayers2016_2021"

# Unzip directories
foldernames = []
for item in os.listdir(zipdir):
    root, ext = os.path.splitext(item)
    if not ext == '.zip':
        continue
    foldernames.append(root)
    if not os.path.exists(os.path.join(zipdir,root)):
        shutil.unpack_archive(os.path.join(zipdir,item), extract_dir=os.path.join(zipdir,root))
    if not os.path.exists(os.path.join(zipdir, root, root[10:]+'.prj')):
        shutil.copy(os.path.join(zipdir, 'edis_xx.prj'), os.path.join(zipdir, root))

        # Remove "GISlayers_" prefix
        os.rename(os.path.join(zipdir, root, 'edis_xx.prj'), os.path.join(zipdir, root, root[10:]+'.prj'))

def getAttributeTable(table):
    columns = [f.name for f in arcpy.ListFields(table) if f.type!="Geometry"] 
    df = pd.DataFrame(data=arcpy.da.SearchCursor(table, columns), columns = columns)
    return(df)

tables = [getAttributeTable(os.path.join(zipdir, name, name[10:]+'.shp')) for name in foldernames]
mode = np.array(stats.mode(np.stack([table.CDI for table in tables]))[0])[0]
df = pd.DataFrame(data={'parishdpno': tables[0].parishdpno, 'CDImode':mode})
df_records = df.to_records(index = False)
df_np = np.array(df_records, dtype = np.dtype([("parishdpno", "f8"), ("CDImode", "f8")]))

# Copy data to intermediate files and write df with mode to shp
shp = os.path.join(dirname, "../intermediate_files/drought.shp")
df_table = os.path.join(dirname, "../intermediate_files/cov_workspace.gdb/drought_table")
if arcpy.Exists(df_table):
    arcpy.management.Delete(df_table)
arcpy.da.NumPyArrayToTable(df_np, df_table)

name = foldernames[0]
arcpy.management.CopyFeatures(os.path.join(zipdir, name, name[10:]+'.shp'), shp)
arcpy.da.ExtendTable(shp, "parishdpno", df_np, "parishdpno", append_only = False)
drought_cov = Covariate(shp, "drought","CDImode")
drought_cov.rasteriseShp()