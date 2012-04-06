# download of recent daily precipitation estimates (NOAA-FEWS)
# script version: 2012-01-22

library(rgdal)
library(raster)

# change settings here --------------------------------------------------
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Precipitation/NOAA/')

date_first <- as.Date("2012-01-20") # set period for downloads - change!
date_last <- as.Date("2012-04-03")

SRM_zones <- "catchmnt0.tif" # contains raster definition (projection, limits, cellsize)
# end of code section with editable settings ----------------------------

day0 <- as.Date("2001-05-01") # dates on NOAA-FEWS start counting from here - do NOT change

e <- raster (SRM_zones)
options(download.file.method="wget") 

for (day in date_first:date_last){
    datestr <- as.character(as.Date(day, "1970-01-01"))
    fname <- paste("data",datestr,".tiff", sep="") # compose filename
    dnum <- as.character(as.double(round(as.Date(datestr)-day0))) # calculate difference in days
    isource <- paste("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.SAsia/.RFEv2/.DAILY/est_prcp/T/", dnum,"./VALUE/X/70./80./RANGEEDGES/Y/30/36/RANGEEDGES/%5BX/Y/%5D/data.tiff?filename=", fname, sep="")
    
    download.file(isource, fname, quiet=T, method="auto", mode="wb") # don't download if already on disk
    myr <- raster(fname)
    projection(myr) <- "+init=epsg:4326" # Geographical Coordinates, WGS84
    myrm <- projectRaster(from=myr, to=e) # reproject raster to geographic properties of model grid 'e' (elevation zones)
    myrm <- myrm*10 # new: save in tenths of a millimeter
    rfile <- paste(substr(fname,5,14),"_m.tif",sep="")
    writeRaster(myrm, rfile, overwrite=TRUE, datatype="INT2U")
}
