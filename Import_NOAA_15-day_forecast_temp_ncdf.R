# Reading 15-day temperature forecast subset and saving as Table
# script version: 18/10/2011

library(ncdf)
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Short-term_forecasts/')

fnc <- "out_tmpT62hFe.temp_1000_ensmean.nc" # (70KB) 
# test file, requested through web page, mean of all 15 members
# single coordinate: 35N, 75E

nc <- open.ncdf(fnc, write=FALSE, readunlim=FALSE, verbose=FALSE) 

print(paste("The file has",nc$nvars,"variable(s)")) # one variable only

v1 <- nc$var[[1]] # 
v11 <- v1[11] # list with inner structure
v111 <- v11[[1]] # 4 lists, one for each dimension

v0 <-c(v1[1:9]) # describes temperature and its units
vOS <- c(v1[15:16]) # data offset = 477.66
vSF <- c(v1[17:18]) # scale factor = 0.00999999

L <- v111[3] # forecast list
LL <-  unlist(do.call(rbind, L)[7]) # hour values 24, 36 ... 360


    # forecast_???: temperature in K - described in vv
    # TCelsius     = TK − 273,15

    # name = "lon", len = 1, units = "degree_east", vals = structure(75) - step 2.5
    # name = "lat", len = 1, units = "degree_north", vals = structure(35) - step 2.5
    # name = "hrs", len = 29, units = "", 29 values from 24 to 360 in 12 hour steps - forecast horizons (L)
    # name = "time", len = 1012 units = "hours since 1-1-1 00:00:0.0", 1012 values with NA each - should be forecast issue date/time

# spatial reference - already set in data requirement

# temporal reference - unclear
# try to extract all data at one (as it was a special request) - it results in 29x1012 doble matrix, in °K with 3 decimal digits 
# how to identyfy the time exactly?
# 1.3. - 31.5. - from 2001 - 2011 = 92 days * 11 years = 1012 days
# make dataframe with column1=date and 29 columns for forecast periods

TK <- 273.15
LLc <- as.character(LL)                                                                                                          
cdf <- data.frame(date=as.Date("1970-01-01"), rbind(1:14))
# names(cdf)[2:15] <- LLc

# Fill dates to df
r=1
for (year in 2001:2011){
    day1 <- as.Date(paste(as.character(year),"03","01",sep="-"))
    day2 <- as.Date(paste(as.character(year),"05","31",sep="-"))
    for (day in day1:day2){
        theDate <- as.Date(day,origin="1970-01-01")
        cdf[r,1] <- theDate
        r <- r+1
    } # end of day
} # end of year

alldat<- get.var.ncdf(nc, varid="temp", start=c(1,1,1,1), count=c(-1,-1,-1,-1), verbose=FALSE, signedbyte=TRUE, forcevarid=NA)

AA <- 8.86 # altitude adjustment from 1000mbar (110m) to Srinagar
alldat <- alldat-TK-AA # convert from K to °C and adjust to pressure at Srinagar

# calculate daily means
# time reference 0:0 equals 05h local time (min), 12 hours later is max.
# thus mean of h24,h36 is mean for day+1 etc.

cdft <- t(alldat)
for (day in 1:14){
    cdf[,1+day] <- (cdft[,2*day]-1+cdft[,2*day])/2
}

write.table (cdf,"Temp_fcast_15.txt", append = F, row.names=FALSE)

# graphical output


### script ends here!

# f0 <- 25 # March 2002 for temperatures = month 507.5
# f9 <- do.call(rbind, v3[5])[1] # convert list into df, than access elements = total number of months
# # C all 0,1,2
# # L : limit to 1 for the beginning of T
# 
# L <- 1 # temperatures
# Gxy <- paste(as.character(v111[[1]]$vals[xp0]),as.character(v111[[2]]$vals[yp0]),sep="-")
# 
# # Lp <- 3
# # F "months since 1960-01-01" :  start f0, up to end (first years have NA, exclude them)
# 
# # restrict data extract!
# 
# m0 <- v111[[4]]$vals[1] # first date expressed in months after day1
# 
# # function for date calculation
# theDate_i <- function(m, m0) { # enter serial number of month in netCDF
#     day1str <- "1960-01-01" # start day as string
#     year0 <- as.numeric(substr(day1str,1,4))
#     month0 <- as.numeric(substr(day1str,6,7))
#     m0 <- m0+m+27 # note: first 15 numbers are in 3-month intervals for temperatures
#     a0 <- floor(m0/12) # number of years to be added to first date
#     m00 <- floor(m0%%12)-1 # integer + modulus - number of months to be added additionally to first year
#     year1 <- year0 + a0
#     substr(day1str,1,4) <- as.character(year0 + a0)
#     substr(day1str,6,7) <- sprintf("%02d",month0 + m00)
#     substr(day1str,9,10) <- "15" # to add remaining half month
#     return(day1str)
# }
# 
# theDate_o <- function(m) { # enter number of month as shown in netCDF
#     day1str <- "1960-01-01" # start day as string
#     year0 <- as.numeric(substr(day1str,1,4))
#     month0 <- as.numeric(substr(day1str,6,7))
#     a0 <- floor(m/12) # number of years to be added to first date
#     m00 <- floor(m%%12)-1 # integer + modulus - number of months to be added additionally to first year
#     year1 <- year0 + a0
#     substr(day1str,1,4) <- as.character(year0 + a0)
#     substr(day1str,6,7) <- sprintf("%02d",month0 + m00)
#     substr(day1str,9,10) <- "15" # to add remaining half month
#     return(day1str)
# }
# 
# y1 <- as.integer(substr(theDate_i(f0,m0),1,4))
# y2 <- 2011
# yn <- y2-y1+1
# 
# fcast <- data.frame(Year=c(y1:y2), Month=integer(yn) , L=integer(yn), Terc=integer(yn), Max=integer(yn), C1=integer(yn), C2=integer(yn), C3=integer(yn)) # new data frame
# 
# y1 <- 2002
# y2 <- 2011
# F <- 4
# L <- 1
# r <-1
# 
# mi <- as.integer(substr(theDate_i(f0,m0),6,7))
# fcast[,2] <- F
# fcast[,3] <- L
# 
# m <-f0-12 +F -3
# for (y in y1:y2){
#     m <- m+12
#     print (theDate_i(m,m0))
# }
# 
# #  size = c(3L, 144L, 72L, 4L, 137L) - C,X,Y,L,F
# pd_prob<- get.var.ncdf(nc, varid="prob", start=c(1,xp0,yp0,L,f0), count=c(-1,1,1,1,-1), verbose=FALSE, signedbyte=TRUE, forcevarid=NA)
# 
# pd_prob <- t(pd_prob) # transpose matrix
# 
# m <- F-2
# for (l in 1:yn) {
#     fcast[l,6:8] <- pd_prob[m,]
#     fcast[l,5] <- max(pd_prob[m,])
#     if (fcast[l,5]==33) fcast[l,4]<-0 else fcast[l,4] <- which.max(pd_prob[m,])
#     m <- m+12
# }
# 
# # now relate terciles with quantiles from historic data Srinagar
# # for F=3, L=1 stored und month=5 i.e. m+2
# MA3.file <- "Temp_amov3mp.txt"
# MA3.df <- read.table(MA3.file, header=T)
# 
# # now relate quantiles based on forecast main tercile
# stats.file <- "Temp_amov3_stats.txt"
# MA3s.df <- read.table(stats.file, header=T)
# 
# for (y in y1:y2){
#     fcast[r,9] <- subset(MA3.df[,3], subset=(MA3.df[,1]==y & MA3.df[,2]==F+L+1))
#     fcast[r,10] <- subset(MA3.df[,4], subset=(MA3.df[,1]==y & MA3.df[,2]==F+L+1))
#     c <-  fcast[r,4]
#     if (c>0) fcast[r,11] <-MA3s.df[F+L+1,2+c]
#     r <-r+1
# }
# names(fcast)[9] <- 'Quant_obs'
# names(fcast)[10] <- 'Temp_obs'
# names(fcast)[11] <- 'Temp_forecast'
# 
# write.table (fcast,"Temp_fcast.txt", append = TRUE, row.names=FALSE)
