# load daily precipitation data from GHCND text file
# last update: 07/12/2011 

library(sqldf)
library(timeSeries)
library(zoo)

options(warn=-2)

setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Temperatures/GHCND/') # set working directory 

isource <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/IN008010200.dly"
# isource2 <- "ftp://anonymous:k_jacobi@ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/IN008010200.dly"
fname <- "IN008010200.dly"
# download.file(isource, fname, quiet=F, method="curl") # automatically download of new file failes, use browser

station <- "Srinagar"
y1 <- 1973 # first year to extract

# data are stored as 1 line per month
# 4 first elements
# ID            1-11   Character
# YEAR         12-15   Integer
# MONTH        16-17   Integer
# ELEMENT      18-21   Character
# 4 colums per day
# VALUE        22-26   Integer
# MFLAG        27-27   Character - measurement flag
# QFLAG        28-28   Character - quality flag. I = failed internal consistency check
# SFLAG        29-29   Character - source flag

ds <-"%Y-%m-%d"

# construct list for fixed width input from file
hl <- c(11,4,2,4) # line header
di <- c(5,1,1,1) # data per day
wl <- di
for (d in 1:30) {wl <- c(wl,di)}
wl <- c(hl,wl)

# construct column numbers containing values
v <- c(5)
for (d in 1:30) {v <- c(v,5+d*4)}

# read complete text file
dly <- read.fwf(fname, widths=wl) # read data file

# query for individual variables
tsql <- paste("SELECT * FROM dly WHERE V2 >=",y1," AND V4='PRCP'",sep="")
dlyr <- sqldf(tsql) # wide table, but for precipitation only

# prepare empty dataframe to take the daily values
l <- dim(dlyr)[1]
cdf <- data.frame(date=character(0), value=numeric(0), stringsAsFactors =F )
cdfd <- cdf

# bring into DB-format with 1 date per row
for (d in 1:31){
    cdfd[1:l,1] <- NA # prepare data frame rows
    # use sprintf for leading zeros in datestring
    cdfd[,1] <- paste(dlyr[,2],sprintf("%02d",dlyr[,3]),sprintf("%02d",d),sep="-") # dates
    dv <- dlyr[,v[d]] # extract values for single day
    ind9 <- which(dv == -9999) # indices for missing values
    dv <- replace(dv, ind9, NA) # replace
    cdfd [,2] <- dv/10 
    cdf <- rbind(cdf, cdfd) # append block of daily values to main dataframe
} # end of loop over d

cdf <- cdf[order(cdf$date),] # sort dataframe
cdf[,1] <- as.Date(cdf[,1], format=ds) # convert strings to dates (not POSIXct !)
cdf <- cdf[!(is.na(cdf[,1])),] # remove rows with not existing dates like 31st April

# load temperature data without missing months
tdf <- read.table("temperatures.txt", header=T)
tdf[,2] <- as.character(tdf[,2])
cdf[,1] <- as.character(cdf[,1]) # convert for query
dlim <- "'2000-01-01'"
tsql <- paste("SELECT tdf.date, cdf.value FROM tdf LEFT JOIN cdf ON cdf.date = tdf.date WHERE tdf.date >=",dlim)
pdf <- sqldf(tsql)
pdf[,1] <- as.Date(pdf[,1])
names(pdf)[2]<- station
write.table(pdf,"Precipitation_srinagar.txt", row.names=F)

