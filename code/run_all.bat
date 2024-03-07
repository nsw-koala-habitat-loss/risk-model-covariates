@echo off
setlocal enabledelayedexpansion

set arcgispro_python="C:\Program Files\ArcGIS\Pro\bin\Python\envs\arcgispro-py3\python.exe"

REM Land use raster processing
start "" /b %arcgispro_python% "landuse.py"

REM Combined drought indicator
start "" /b %arcgispro_python% "drought.py"

REM Distance to SUA
start "" /b %arcgispro_python% "dist_sua.py"

REM Ecological condition indicator
start "" /b %arcgispro_python% "ecol_cond.py"

