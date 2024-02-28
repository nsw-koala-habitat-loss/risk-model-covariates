import arcpy
import os
import pathlib

dirname = os.path.dirname(os.path.realpath(__file__))
## Set reference raster ---------
ref_raster = os.path.join(dirname, "../data/risk_analysis/clearing_spatial_units_data_prep/woody_veg_loss_prep/veg_loss/agr_loss_1119.tif")

## ------------------------------

workspace = os.path.join(dirname, "../intermediate_files/cov_workspace.gdb")

class Covariate:
    def __init__(self, path, name, field=""):
        setup()
        self.path = path
        self.name = name
        self.field = field

    def projectShp(self):
        arcpy.management.Project(self.path, self.name+"_proj", ref_raster)

    def rasteriseShp(self):
        proj_shp = self.name+"_proj"
        if not arcpy.Exists(proj_shp):
            print("Projecting {} coordinate system...".format(self.name))
            self.projectShp()

        # Rasterises shapefile according to ref raster and then saves
        out_path = os.path.join(dirname, "../output/"+self.name+".tif")
        print("Converting {} to raster...".format(self.name))
        arcpy.conversion.PolygonToRaster(proj_shp, self.field, out_path)
        print("Raster saved in {}".format(out_path))

def setup():
    if not arcpy.Exists(workspace):
        location, name = os.path.split(os.path.abspath(workspace))
        arcpy.management.CreateFileGDB(location, name)
    arcpy.env.workspace = os.path.join(dirname, "../intermediate_files/cov_workspace.gdb")
    arcpy.env.snapRaster = ref_raster
    arcpy.env.extent = ref_raster
    

if __name__ == '__main__':
    setup()