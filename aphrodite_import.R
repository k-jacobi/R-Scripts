# Reading Aphrodite dataset (daily rainfall grid) and exporting to Mangla SRM tables
# script version 2012-01-15

library(ncdf)
library(raster)

# change settings here --------------------------------------------------
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Precipitation/Aphrodite/')
year <- "2006"

fnc <- paste("APHRO_MA_025deg_V1003R1",year,"nc",sep=".")
SRM_zones <- "catchmnt0.tif" # contains raster definition (projection, limits, cellsize)

# area of interest for Mangla
# "degrees_east" : 72.125 ... 75.875 - start 49 , count 15 | X, verified start pos. with v111[[1]]$vals[49]
# "degrees_north" : 32.375 ... 35.625 - start 190 , count 13 | Y, verified start pos. with v111[[2]]$vals[190]
# "days since 2000-1-1 0" : all 366 - T

# limits of intended raster output for Mangla (cell centers, not outer borders!)
xmin <- 72.125
xmax <- 75.875
ymin <- 32.375
ymax <- 35.625
dd <- 0.25 # resolution of rainfall raster

# indexes in ncdf. total x=401, total y=301
xp0 <- 49 # start position for xmin
yp0 <- 190
xc <- 15 # count for x 
yc <- 13
# end of code section with editable settings ----------------------------

if(as.integer(year)%/%4!=as.integer(year)/4) tc<- 365 else tc <- 366
nc <- open.ncdf(fnc, write=FALSE, readunlim=FALSE, verbose=FALSE) # works fine, reads file structure

print(paste("The file has",nc$nvars,"variables")) # 2 variables, precip and rstn
# units = "mm/day", longname = "daily precipitation analysis interpolated onto 0.25deg grids [mm/day]",
v1 <- nc$var[[1]]
v11 <- v1[11] # list with inner structure
v111 <- v11[[1]] # 3 lists, one for each variable

spd <- 86400 # each additional day is adding 86400 seconds
day1 <- as.POSIXct(paste(year,"01-01",sep="-"), format="%Y-%m-%d", tz = "")

e <- raster (SRM_zones)
prj_ll <- "+init=epsg:4326"

pda <- get.var.ncdf(nc, varid="precip", start=c(xp0,yp0,1), count=c(xc,yc,tc), verbose=FALSE, signedbyte=TRUE, forcevarid=NA) 
# makes one long array, but .Dim = c(15L, 13L, 365L)

for (td in 1:tc){
    dayout <- strftime(day1+spd*(td-1), format="%Y-%m-%d")  # calculates date and converts into string
    rfile <-  paste(dayout,"_m.tif",sep="")
    rm <- (pda [,,td])*10 # single 'layer' of the array to matrix, multiply by factor 10 to store as integers
    rtm <- t(rm) # transpose matrix, otherwise cells not rectangular
    myr <- raster(rtm, xmn=xmin+dd/2, xmx=xmax+dd/2, ymn=ymin+dd/2, ymx=ymax+dd/2, crs=prj_ll) # matrix to raster
    myr <-flip (myr,'y') # north-south
    
    myrm <- projectRaster(from=myr, to=e) # reproject raster to geographic properties of model grid 'e' (elevation zones)
    
    writeRaster(myrm, rfile, overwrite=TRUE, datatype="INT2U")
    cat (td, dayout, maxValue(myr)/10,"\n")
}
