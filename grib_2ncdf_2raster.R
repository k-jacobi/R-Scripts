# test of opening converted grib2 to ncd file
# script version 05/12/2011

library (rgdal)
library (sp)
library (raster)
library (ncdf)

options(timeout=180)
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Forecasts/CFS/')

ncfile <- "tmp2m_test2.nc"
#tmp_brick <- brick(ncfile)
tmp_brick <- brick(ncfile, varname="TMP_2-HTGL", level=4)

# class       : RasterBrick 
# dimensions  : 73, 144, 10512, 1  (nrow, ncol, ncell, nlayers)
# resolution  : 1, 1  (x, y)
# extent      : 0.5, 144.5, 0.5, 73.5  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
# values      : C:\AHT.PRO\Pakistan_Modelling\Data\Forecasts\CFS\tmp2m_test2.nc 
# zvar        : TMP_2-HTGL 
# level       : 4 

ncfile <- "tmp2m_test1.nc"
#tmp_brick <- brick(ncfile)
tmp_brick <- brick(ncfile, varname="TMP_2-HTGL")

# same dimensions

ncfile <- "tmp2m_test3.nc"
tmp_brick <- brick(ncfile, varname="TMP_2-HTGL", level=4)

# Error in raster:::.varName(nc, varname, warn = warn) : 
#   varname: TMP_2-HTGL does not exist in the file. Select one from:
# MapProjection, longitude, latitude, TMP_2_HTGL

tmp_brick <- brick(ncfile)

# Warning message:
# In raster:::.varName(nc, varname, warn = warn) :
#   varname used is: TMP_2_HTGL
# If that is not correct, set it to one of: MapProjection, longitude, latitude, TMP_2_HTGL

# class       : RasterBrick 
# dimensions  : 73, 144, 10512, 1  (nrow, ncol, ncell, nlayers)
# resolution  : 2.5, 2.5  (x, y)
# extent      : -1.25, 358.75, 242265111, 242265293  (xmin, xmax, ymin, ymax)
# coord. ref. : NA 
# values      : C:\AHT.PRO\Pakistan_Modelling\Data\Forecasts\CFS\tmp2m_test3.nc 
# zvar        : TMP_2_HTGL 

