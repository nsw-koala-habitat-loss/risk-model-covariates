@echo off
setlocal enabledelayedexpansion

set arcgispro_python="C:\Program Files\ArcGIS\Pro\bin\Python\envs\arcgispro-py3\python.exe"

REM Land use raster processing

start "" /b %arcgispro_python% "landuse.py"

REM Combined drought indicator

REM etc