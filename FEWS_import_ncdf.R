# Reading FEWS dataset and saving as GeoTiff
# algorithm change: 2011-10-23 # correct wrong spatial registration
# update 2012-01-17: defined area for user modification

library(rgdal)
library(raster)
library(ncdf)

# change settings here --------------------------------------------------
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Precipitation/NOAA/')

fnc <- "data.cdf"
f.template <- "catchmnt0.tif"  # contains raster definition (projection, limits, cellsize)
f.template <- "2001-05-01_m.tif"
d1 <- 3678 # number of first day, counting from day1
d2 <- 3681 # number of last day

nc <- open.ncdf(fnc, write=FALSE, readunlim=FALSE, verbose=FALSE) # works fine, reads file structure
# print(paste("The file has",nc$nvars,"variable")) # 1 variable: est_prcp 
v1 <- nc$var[[1]]
v11 <- v1[11] # list with inner structure
v111 <- v11[[1]] # 3 lists, one for each dimension

# geographic range of ncdf file: X=70:110, Y=5:35
# area of interest for Mangla
# "degrees_east" : 72.8 - 75.8 - start 29 , count 31 - X, verified start pos. with v111[[1]]$vals[xp0]
# "degrees_north" : 32.8 - 35.0 - start 279 , count 23 - Y, verified start pos. with v111[[2]]$vals[yp0]
# "days since days 2001-05-01" : all 0:3681 - T

# limits of intended raster output for Mangla (cell centers, not outer borders!)
xmin <- 72.8
xmax <- 75.8
ymin <- 32.8
ymax <- 35.0

# indexes in ncdf. total x=401, total y=301
xp0 <- 29 # start position for xmin
yp0 <- 279
xc <- 31 # count for x 
yc <- 23 # not 22
# end of code section with editable settings ----------------------------

day1 <- as.POSIXct("2001-05-01", format="%Y-%m-%d", tz = "") 
spd <- 86400 # each additional day is adding 86400 seconds

e <- raster(f.template)
prj_ll <- "+init=epsg:4326" # projection of data in ncdf
dd <- 0.1 # resolution of rainfall raster

# for information only, not required for calculation as read directly from raster objects
prj_utm <- "+init=epsg:32624" # UTM 43N, WGS84
# res_ll <- c(0.1,0.1) # resolution of daily rainfall in degrees
# res_utm <- c(500,500) # resolution of model raster in m

# makes one long array, but .Dim = c(31L, 23L, 3681L) 
# count -1 reads all values along that dimension 

pdnoaa <- get.var.ncdf(nc, varid="est_prcp", start=c(xp0,yp0,1), count=c(xc,-1,-1), verbose=FALSE, signedbyte=TRUE, forcevarid=NA) 

for (td in d1:d2){ 
    dayout <- strftime(day1+spd*(td-1), format="%Y-%m-%d")  # calculates date and converts into string
    rfile <-  paste(dayout,"_m.tif",sep="")
    rm <- (pdnoaa [,,td])*10 # single 'layer' of the array to matrix, multiply by factor 10 to store as integers
    rtm <- t(rm) # transpose matrix, otherwise cells not rectangular
    myr <- raster(rtm, xmn=xmin-dd/2, xmx=xmax+dd/2, ymn=ymin-dd/2, ymx=ymax+dd/2, crs=prj_ll) # matrix to raster
    myr <-flip (myr,'y') # north-south
    
    myrm <- projectRaster(from=myr, to=e) # reproject raster to geographic properties of template grid 'e'
    myrm <- projectRaster(from=myr, crs=prj_utm, res=500) # reproject raster to geographic properties of template grid 'e'
    
    writeRaster(myrm, rfile, overwrite=TRUE, datatype="INT2U")
    cat (td, dayout, maxValue(myr),"\n")
}
