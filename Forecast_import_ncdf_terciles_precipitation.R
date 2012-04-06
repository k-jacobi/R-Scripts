# Reading IRI precipitation forecast dataset and saving as Table
# script version: 19/09/2011

library(ncdf)
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Forecasts/')

fnc <- "forecast_precipitation.cdf" # (91.8MB)
# fnct <- "forecast_temperature.cdf" # (142.3MB)
nc <- open.ncdf(fnc, write=FALSE, readunlim=FALSE, verbose=FALSE) # precipitation

#print(paste("The file has",nc$nvars,"variable(s)")) 

    # forecast_precipitation: Tercile Probability

    # name = "C", len = 3, units = "percent", longname = "Tercile Probability"
    # name = "X", len = 144, units = "degree_east", vals = structure(c(-178.75 ... 178.75)) - step 2.5
    # name = "Y", len = 72, units = "degree_north", vals = structure(c(88.75 ... -88.75)) - step 2.5
    # name = "L", len = 4L, units = "months", vals = structure(c(1, 2, 3, 4)) - lead time
    # name = "F", len = 138 units = "months since 1960-01-01", vals = structure(c(452.5 ... 619.5)) - step 1
    # 452.5/12=37.71 = 1997-08-16 | 619.5 = 2011-07-15 - 3 months forecasts, but data were downloaded on 2011-08-15

v1 <- nc$var[[1]] # units = "percent", longname = "Dominant Tercile Probability" - 4 dims
v11 <- v1[11] # list with inner structure
v111 <- v11[[1]] # 4 lists, one for each dimension

v2 <- nc$var[[2]] # units = "percent", longname = "Tercile Probability" - 5 dims
#  size = c(3L, 144L, 72L, 4L, 138L) - C,X,Y,L,F
v21 <- v2[11] # list with inner structure

# name = "C", len = 3 units = "ids", vals =() - Below Normal =0, Normal =1, Above Normal =2

v3 <-  nc$var[[3]] # units = "months since 1960-01-01", longname = "target_date"
v4 <-  nc$var[[4]] # units = "unitless", longname = "target_season"

# area of interest for Northern Pakistan
# "degrees_east" verified start pos. with v111[[1]]$vals[xp0]
# "degrees_north" verified start pos. with v111[[2]]$vals[yp0]
# precipitation single cell:  73.75, 33.75 start=102, count = 1, start = 23, count=1
# temperatures 4 cells: X = 73, 75; Y = 33, 35 start = ?, count=2, start = ?, count = 2
xp0 <- 102
yp0 <- 23
f0 <- 26 # March 2002
f9 <- do.call(rbind, v3[5])[1] # convert list into df, than access elements
# C all 0,1,2
# L all 1:4 # limit to 1 for the beginning of T, 3 to beginning of P
L <- 3 # precipitation
# Lp <- 3
# F "months since 1960-01-01" :  start f0, up to end (first years have NA, exclude them)

# restrict data extract!
# for precipitation, use F= March, L=3 (June - August) 

m0 <- v111[[4]]$vals[1] # first date expressed in months after day1

# function for date calculation
theDate_i <- function(m, m0) { # enter serial number of month in netCDF
    day1str <- "1960-01-01" # start day as string
    year0 <- as.numeric(substr(day1str,1,4))
    month0 <- as.numeric(substr(day1str,6,7))
    #m0 <- m0+m-1 # note: first 16 numbers are in 3-month intervals
    m0 <- m0+m+29 # note: first 16 numbers are in 3-month intervals
    a0 <- floor(m0/12) # number of years to be added to first date
    m00 <- floor(m0%%12)-1 # integer + modulus - number of months to be added additionally to first year
    year1 <- year0 + a0
    substr(day1str,1,4) <- as.character(year0 + a0)
    substr(day1str,6,7) <- sprintf("%02d",month0 + m00)
    substr(day1str,9,10) <- "15" # to add remaining half month
    return(day1str)
}

theDate_o <- function(m) { # enter number of month as shown in netCDF
    day1str <- "1960-01-01" # start day as string
    year0 <- as.numeric(substr(day1str,1,4))
    month0 <- as.numeric(substr(day1str,6,7))
    a0 <- floor(m/12) # number of years to be added to first date
    m00 <- floor(m%%12)-1 # integer + modulus - number of months to be added additionally to first year
    year1 <- year0 + a0
    substr(day1str,1,4) <- as.character(year0 + a0)
    substr(day1str,6,7) <- sprintf("%02d",month0 + m00)
    substr(day1str,9,10) <- "15" # to add remaining half month
    return(day1str)
}


# for L=1:4, makes one long array, but .Dim = c(3,4,114) #  array[1368]
# for L=1, makes a 3*107 matrix
# count -1 reads all values along that dimension 

pd_prob<- get.var.ncdf(nc, varid="prob", start=c(1,xp0,yp0,L,f0), count=c(-1,1,1,1,-1), verbose=FALSE, signedbyte=TRUE, forcevarid=NA)

pd_prob <- t(pd_prob) # transpose matrix


# current end script revision
# convert this into dataframe
# simplified system: just one dataset per year (L=3)
# need to calculate F for every year (step 12)

y1 <- as.integer(substr(theDate_i(f0,m0),1,4))
y2 <- 2011
yn <- y2-y1+1

fcast <- data.frame(Year=c(y1:y2), Month=integer(yn) , L=integer(yn), C1=integer(yn), C2=integer(yn), C3=integer(yn))
mi <- as.integer(substr(theDate_i(f0,m0),6,7))
fcast[,2] <- mi
fcast[,3] <- L

m <-f0-12
for (y in y1:y2){
    m <- m+12
    print (theDate_i(m,m0))
}

m <- 1
for (l in 1:yn) {
    fcast[l,4:6] <- pd_prob[m,]
    m <- m+12
}    

# for (f in f0:3681){
#     dayout <- strftime(day1+spd*(td-1), format="%Y-%m-%d")  # calculates date and converts into string
#     rfile <-  paste(dayout,"_m.tif",sep="")
#     # the following line generated error 'subscript out of bounds' for td>1500 if not whole file was read in
#     # possibly data were read in wrongly, when not starting from day 0, better repeat whole processing
#     rm <- (pdnoaa [,,td])*10 # single 'layer' of the array to matrix, multiply by factor 10 to store as integers
#     rtm <- t(rm) # transpose matrix, otherwise cells not rectangular
#     myr <- raster(rtm, xmn=xmin+dd/2, xmx=xmax+dd/2, ymn=ymin+dd/2, ymx=ymax+dd/2, crs=prj_ll) # matrix to raster
#     myr <-flip (myr,'y') # north-south
#     
#     myrm <- projectRaster(from=myr, to=e) # reproject raster to geographic properties of model grid 'e' (elevation zones)
#     
#     writeRaster(myrm, rfile, overwrite=TRUE, datatype="INT2U")
#     cat (td, dayout, maxValue(myr),"\n")
# }
