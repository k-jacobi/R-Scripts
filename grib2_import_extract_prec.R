# import / extract GRIB2 file
# script version 30/11/2011

library (rgdal)
library (sp)
library (raster)

setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Forecasts/CFS/')
gribfile <- "prate.01.2011112700.daily" # data are precipitation rates in kg/m2/s
tmp_brick <- brick(gribfile) # 559 layers, very fast
# tmp_raster <- raster(tmp_brick, layer=2) # for testing
# tmp_raster <- rotate(tmp_raster) # adjust from 0/360 to -180/180
# writeRaster(tmp_raster, 'tmp_test2.tif', overwrite=TRUE, datatype="FLT4S")

gdal_test <- readGDAL(gribfile) # will give an error message

# prate.01.2011112700.daily has GDAL driver GRIB and has 73 rows and 144 columns
# Error in validityMethod(as(object, superClass)) :   Geographical CRS given to non-conformant data

# extract time series for single coordinate
t <- extract(tmp_brick, cellFromXY(tmp_raster, c(74,34)))*43200 # time series in mm

# get start time/date
t0 <- as.POSIXct(substr(gribfile,10,19), format="%Y%m%d%H", tz = "GMT")
days <- nlayers(tmp_brick)-nlayers(tmp_brick)%%2
ti <- seq(t0, by= t0-(t0-43200), len=days)
t.df <- data.frame(ti) # get time into df
t.df[,2] <- data.frame(t[1:days]) # get values into df
names(t.df) <- c('time', 'value')

plot(t.df, type='l')
grid()

# calculate max, min, total
d.df <- t.df
d.df[,1] <- data.frame(as.Date(d.df[,1]))
d.df <- d.df[seq(1,days,2),] 
d.df[,3] <- t.df[seq(2,days,2),2]
d.df[,4] <- (d.df[,2]+d.df[,3])
names(d.df)[2:4] <- c('max', 'min', 'sum')

# plot results
xlim <- c(d.df[1,1],d.df[days/2,1])
ylim <- c(min(d.df[,4]),max(d.df[,4]))
tmain <- "9-Months Precipitation Forecast for Mangla catchment"
tsource <- 'Data source: The NCEP Climate Forecast System Version 2 (CFSv2)'
plot (x=d.df[,1], y=d.df[,4], col="blue", type="h", xlim=xlim, ylim=ylim, xlab="", ylab="Precipitation in mm",main=tmain )
grid()
mtext(tsource, side=1, line=4, outer=FALSE, cex=0.75)
