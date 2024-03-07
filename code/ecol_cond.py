import arcpy
import os
import pathlib
from covariates import Covariate

dirname = os.path.dirname(os.path.realpath(__file__))

# Merge NSW and Sydney-specific land-use
ecol_cond_tif = os.path.join(dirname, "../data/ecol_cond/biodiversityecologicalconditionofterrestrialhabitat/bba_31a_cond_V2_nsw_raw.tif")

# Create new covariate class
ecol_cond_cov = Covariate(ecol_cond_tif, "ecol_cond", "")
ecol_cond_cov.resampleRaster()