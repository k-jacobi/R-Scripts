# import / extract GRIB2 reforecast file (monthly averages, many parameters)
# script version 03/12/2011

library (rgdal)
library (sp)
library (raster)

setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Forecasts/CFSR/')
gribfile <- "tmp2m.2009030700.time.grb2" # 1200 layers
gribfile <- "prate.2009030200.time.grb2" # 1220 layers

tmp_brick <- brick(gribfile) # 
tmp_array <- as.array(tmp_brick)

tmp_raster <- raster(tmp_brick, layer=25) # for testing
# tmp_raster <- rotate(tmp_raster) # adjust from 0/360 to -180/180
writeRaster(tmp_raster, 'tmp_test_rp25.tif', overwrite=TRUE, datatype="FLT4S")

t <- extract(tmp_brick, cellFromXY(tmp_brick, c(74,34+180)))*21600 # all precipitation values show 0!
t <- extract(tmp_brick, cellFromXY(tmp_brick, c(74,34+180))) # 

gdal_test <- readGDAL(gribfile)