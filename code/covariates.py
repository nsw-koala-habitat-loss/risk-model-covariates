import arcpy
import os
import pathlib
from arcpy.sa import DistanceAccumulation

dirname = os.path.dirname(os.path.realpath(__file__))
## Set reference raster ---------
ref_raster = os.path.join(dirname, "../data/risk_analysis/clearing_spatial_units_data_prep/woody_veg_loss_prep/veg_loss/agr_loss_1119.tif")
nsw = os.path.join(dirname, "../data/Admin_boundaries/NSW_LGA_GDA2020/NSW_LGA_GDA2020.shp")

## ------------------------------

workspace = os.path.join(dirname, "../intermediate_files/cov_workspace.gdb")
arcpy.env.overwriteOutput = True
if not arcpy.Exists(workspace):
    location, name = os.path.split(os.path.abspath(workspace))
    arcpy.management.CreateFileGDB(location, name)
arcpy.env.workspace = os.path.join(dirname, "../intermediate_files/cov_workspace.gdb")
arcpy.env.snapRaster = ref_raster
arcpy.env.extent = ref_raster

class Covariate:
    def __init__(self, path, name, field=""):
        self.path = path
        self.name = name
        self.field = field

    def projectShp(self, in_shp=None):
        proj_shp = self.name+"_projshp"
        if in_shp is None:
            in_shp = self.path
        if arcpy.Exists(proj_shp):
            arcpy.management.Delete(proj_shp)
        print("Projecting {} coordinate system...".format(self.name))
        arcpy.management.Project(in_shp, proj_shp, ref_raster)
        return(proj_shp)
    
    def clipShp(self, in_shp=None):
        clip_shp = self.name+"_clip"
        if in_shp is None:
            in_shp = self.path
        if arcpy.Exists(clip_shp):
            arcpy.management.Delete(clip_shp)
        print("Clipping shapefile {} to extent".format(self.name))
        arcpy.analysis.PairwiseClip(in_shp, nsw, clip_shp)
        return(clip_shp)
    
    def projectRaster(self):
        proj_rast = self.name+"_projrast"
        if arcpy.Exists(proj_rast):
            arcpy.management.Delete(proj_rast)
        print("Projecting {} coordinate system...".format(self.name))
        arcpy.management.ProjectRaster(self.path, proj_rast, ref_raster)
        return(proj_rast)

    def rasteriseShp(self):
        proj_shp = self.projectShp()
        clip_shp = self.clipShp(proj_shp)

        # Rasterises shapefile according to ref raster and then saves
        out_path = self.getOutPath()
        print("Converting {} layer to raster...".format(self.name))
        arcpy.conversion.PolygonToRaster(clip_shp, self.field, out_path)
        print("Raster saved in {}".format(out_path))
    
    def resampleRaster(self):
        proj_rast = self.projectRaster()
        clip_rast = self.name+"_clipped"
        resampled_rast = self.name+"_resampled"

        self.clipFeatures(proj_rast, clip_rast)
        print("Resampling {} layer to reference raster...".format(self.name))
        cellSizeX = arcpy.Describe(ref_raster).meanCellWidth
        cellSizeY = arcpy.Describe(ref_raster).meanCellHeight
        cellSizeXY = u"{} {}".format(cellSizeX, cellSizeY)
        out_path = self.getOutPath()
        arcpy.management.Resample(clip_rast, out_path, cellSizeXY)
        return(resampled_rast)
    
    def euclideanDistRaster(self):
        # Given a shp file, calculate the euclidean distance from the points and save to raster
        
        # Project the shape file
        proj_shp = self.projectShp() 
        out_path = self.getOutPath()
        unclipped_path = os.path.join(dirname, "../intermediate_files",self.name+"_unclipped.tif")
        arcpy.CheckOutExtension("Spatial")
        if arcpy.Exists(unclipped_path):
            arcpy.Delete_management(unclipped_path)
        
        for distAccFile in arcpy.ListFiles("Distanc_*"):
            arcpy.Delete_management(distAccFile)

        outDistAcc = DistanceAccumulation(proj_shp)
        outDistAcc.save(unclipped_path)
        
        self.clipFeatures(unclipped_path)

    def getOutPath(self):
        out_path = os.path.join(dirname, "../output/"+self.name+".tif")
        return(out_path)
    
    def clipFeatures(self, in_path, out_path=None):
        if out_path is None:
            out_path = self.getOutPath()

        print("Clipping {} to reference raster...".format(self.name))
        ref_raster_extent = arcpy.Describe(ref_raster).extent
        rectangle = "{} {} {} {}".format(ref_raster_extent.XMin, ref_raster_extent.YMin, ref_raster_extent.XMax, ref_raster_extent.YMax)
        arcpy.management.Clip(in_path, rectangle, out_path, nsw, "#", "ClippingGeometry")
        return(out_path)
    
    def append_ext(original_path, append, ext = None):
        # Extract the base name and current extension
        base_name, extension = os.path.splitext(os.path.basename(original_path))

        # Append your desired string
        new_base_name = base_name + '_' + append

        if (ext is not None):
            extension = ext

        # Construct the new path with the same extension
        new_path = os.path.join(os.path.dirname(original_path), new_base_name + extension)

        return new_path