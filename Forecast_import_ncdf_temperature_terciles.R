# Reading IRI precipitaion forecast dataset and saving as GeoTiff
# script version: 21/08/2011

library(rgdal)
library(raster)
library(ncdf)
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Forecasts/')

#fnc <- "forecast_precipitation.cdf" # (91.8MB)
fnc <- "forecast_temperature.cdf" # (142.3MB)
nc <- open.ncdf(fnc, write=FALSE, readunlim=FALSE, verbose=FALSE) # works fine, reads file structure

print(paste("The file has",nc$nvars,"variable(s)")) 

    # forecast_precipitation: Tercile Probability

    # name = "C", len = 3, units = "percent", longname = "Tercile Probability"
    # name = "X", len = 144, units = "degree_east", vals = structure(c(-178.75 ... 178.75)) - step 2
    # name = "Y", len = 72, units = "degree_north", vals = structure(c(88.75 ... -88.75)) - step 2
    # name = "L", len = 4L, units = "months", vals = structure(c(1, 2, 3, 4)) - lead time
    # name = "F", len = 138 units = "months since 1960-01-01", vals = structure(c(455.5 ... 619.5)) - step 1
    # 452.5/12=37.71 = 1997-08-16 | 619.5 = 2011-07-15 - 3 months forecasts, but data were downloaded on 2011-08-15

v1 <- nc$var[[1]] # units = "percent", longname = "Dominant Tercile Probability" - 4 dims
v11 <- v1[11] # list with inner structure
v111 <- v11[[1]] # 4 lists, one for each dimension

v2 <- nc$var[[2]] # units = "percent", longname = "Tercile Probability" - 5 dims
#  size = c(3L, 144L, 72L, 4L, 138L) - C,X,Y,L,F
v21 <- v2[11] # list with inner structure
v211 <- v21[[1]] # 5 lists, one for each dimension

# name = "C", len = 3 units = "ids", vals =() - Below Normal =0, Normal =1, Above Normal =2

v3 <-  nc$var[[3]] # units = "months since 1960-01-01", longname = "target_date"
v4 <-  nc$var[[4]] # units = "unitless", longname = "target_season"

# area of interest for Northern Pakistan
# "degrees_east" verified start pos. with v111[[1]]$vals[xp0]
# "degrees_north" verified start pos. with v111[[2]]$vals[yp0]
# single cell:  75, 33 start=128, count = 1, start = 29, count=1
xp0 <- 128
yp0 <- 29
f0 <- 34 # first years look funny 
f9 <- v211[[5]]$len #137 
# C all 0,1,2
# L all 1:4
# F "months since 1960-01-01" :  start f0, up to end (first years have NA, exclude them)

# function for date calculation
f_date <- function(f) {
    day1str <- "1960-01-01" # start day as string
    year0 <- as.numeric(substr(day1str,1,4))
    month0 <- as.numeric(substr(day1str,6,7))
    m0 <- v211[[5]]$vals[f] # date expressed in months after day1
    a0 <- floor(m0/12) # number of years to be added to first date
    m00 <- floor(m0%%12) # integer + modulus - number of months to be added additionally to first year
    year1 <- year0 + a0
    substr(day1str,1,4) <- as.character(year0 + a0)
    substr(day1str,6,7) <- sprintf("%02d",month0 + m00)
    substr(day1str,9,10) <- "15" # to add remaining half month
    return(day1str)
}

# size = c(3L, 144L, 72L, 4L, 137L) - C,X,Y,L,F
# makes one long array, but .Dim = c(3,4,114) #  array[1368]
# count -1 reads all values along that dimension 

#tf_prob<- get.var.ncdf(nc, varid="prob", start=c(1,xp0,yp0,1,f0), count=c(-1,1,1,-1,-1), verbose=FALSE, signedbyte=TRUE, forcevarid=NA)

tf_prob<- get.var.ncdf(nc, varid="prob", start=c(1,xp0,yp0,1,f0), count=c(-1,1,1,-1,-1), verbose=FALSE, signedbyte=TRUE, forcevarid=NA)

# convert this into dataframe

df <- apply (tf_prob, 1, I) # convert array into dataframe
#dfO <- data.frame(cbind(fdate="fdate",L=1:nrow(df),t0=1,t1=1,t2=1))
dfO <- data.frame(fdate="dummy",L=1:nrow(df),t0=1,t1=1,t2=1, stringsAsFactors=FALSE)
dfi <- data.frame(cbind(fdate="date",L=1,t0=1,t1=1,t2=1))
rn=1

for (f in f0:f9){
    dayf <- f_date(f)
    for (l in 1:4) {
        df1 <- data.frame(dayf,l,df[rn,1],df[rn,2],df[rn,3], stringsAsFactors=FALSE)
        dfO[rn,] <- df1
        rn <- rn+1
    }
}
