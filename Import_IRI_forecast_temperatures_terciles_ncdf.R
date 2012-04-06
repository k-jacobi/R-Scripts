# Reading IRI temperature forecast dataset and saving as Table
# script version: 26/09/2011

library(ncdf)
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Forecasts/')

fnc <- "forecast_temperature.cdf" # (142.3MB)
nc <- open.ncdf(fnc, write=FALSE, readunlim=FALSE, verbose=FALSE) # precipitation

#print(paste("The file has",nc$nvars,"variable(s)")) 

    # forecast_???: Tercile Probability

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
# temperatures 4 cells: X = 73, 75; Y = 35, 33 start = 127, count=2, start = 28, count = 2

# spatial reference:
xp0 <- 127 # temperatures
#xp0 <- c(127,127,128,128)
yp0 <- 29 # temperatures
#yp0 <- c(29,29,28,28)

# temporal reference
f0 <- 25 # March 2002 for temperatures = month 507.5
f9 <- do.call(rbind, v3[5])[1] # convert list into df, than access elements = total number of months
# C all 0,1,2
# L : limit to 1 for the beginning of T

L <- 1 # temperatures
Gxy <- paste(as.character(v111[[1]]$vals[xp0]),as.character(v111[[2]]$vals[yp0]),sep="-")

# Lp <- 3
# F "months since 1960-01-01" :  start f0, up to end (first years have NA, exclude them)

# restrict data extract!

m0 <- v111[[4]]$vals[1] # first date expressed in months after day1

# function for date calculation
theDate_i <- function(m, m0) { # enter serial number of month in netCDF
    day1str <- "1960-01-01" # start day as string
    year0 <- as.numeric(substr(day1str,1,4))
    month0 <- as.numeric(substr(day1str,6,7))
    m0 <- m0+m+27 # note: first 15 numbers are in 3-month intervals for temperatures
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

y1 <- as.integer(substr(theDate_i(f0,m0),1,4))
y2 <- 2011
yn <- y2-y1+1

fcast <- data.frame(Year=c(y1:y2), Month=integer(yn) , L=integer(yn), Terc=integer(yn), Max=integer(yn), C1=integer(yn), C2=integer(yn), C3=integer(yn)) # new data frame

y1 <- 2002
y2 <- 2011
F <- 4
L <- 1
r <-1

mi <- as.integer(substr(theDate_i(f0,m0),6,7))
fcast[,2] <- F
fcast[,3] <- L

m <-f0-12 +F -3
for (y in y1:y2){
    m <- m+12
    print (theDate_i(m,m0))
}

#  size = c(3L, 144L, 72L, 4L, 137L) - C,X,Y,L,F
pd_prob<- get.var.ncdf(nc, varid="prob", start=c(1,xp0,yp0,L,f0), count=c(-1,1,1,1,-1), verbose=FALSE, signedbyte=TRUE, forcevarid=NA)

pd_prob <- t(pd_prob) # transpose matrix

m <- F-2
for (l in 1:yn) {
    fcast[l,6:8] <- pd_prob[m,]
    fcast[l,5] <- max(pd_prob[m,])
    if (fcast[l,5]==33) fcast[l,4]<-0 else fcast[l,4] <- which.max(pd_prob[m,])
    m <- m+12
}

# now relate terciles with quantiles from historic data Srinagar
# for F=3, L=1 stored und month=5 i.e. m+2
MA3.file <- "Temp_amov3mp.txt"
MA3.df <- read.table(MA3.file, header=T)

# now relate quantiles based on forecast main tercile
stats.file <- "Temp_amov3_stats.txt"
MA3s.df <- read.table(stats.file, header=T)

for (y in y1:y2){
    fcast[r,9] <- subset(MA3.df[,3], subset=(MA3.df[,1]==y & MA3.df[,2]==F+L+1))
    fcast[r,10] <- subset(MA3.df[,4], subset=(MA3.df[,1]==y & MA3.df[,2]==F+L+1))
    c <-  fcast[r,4]
    if (c>0) fcast[r,11] <-MA3s.df[F+L+1,2+c]
    r <-r+1
}
names(fcast)[9] <- 'Quant_obs'
names(fcast)[10] <- 'Temp_obs'
names(fcast)[11] <- 'Temp_forecast'

write.table (fcast,"Temp_fcast.txt", append = TRUE, row.names=FALSE)
