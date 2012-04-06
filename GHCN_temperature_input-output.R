# load daily climate data from GHC text file
# last update: 2012-04-04 (changed graphics settings)

library(sqldf)
library(timeSeries)
library(zoo)

# change settings here --------------------------------------------------
options(warn=-2)

setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Temperatures/GHCND/') # set working directory 

isource <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/IN008010200.dly"
fname <- "IN008010200.dly"
#download.file(isource, fname, quiet=F, method="auto", mode="wb") # if automatically download of new file failes, use browser

station <- "Srinagar"
y1 <- 1973 # first year to extract - start of temperature data for NOAA Srinagar

# chart settings - may differ for a different station
ylimi <- c(-10,40) # for daily data
ylima <- c(-5,28) # for averages
y1 <-2001 # first year of chart output
y2 <-2012 # last year of chart output
# end of code section with editable settings ----------------------------

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

e <- c("TMIN","TMAX","PREC")
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

dly <- read.fwf(fname, widths=wl) # read data file

tsql <- paste("SELECT * FROM dly WHERE V2 >=",y1," AND V4='",e[1],"'",sep="")
tmindf <- sqldf(tsql)
tsql <- paste("SELECT * FROM dly WHERE V2 >=",y1," AND V4='",e[2],"'",sep="")
tmaxdf <- sqldf(tsql)
dlyr <- tmindf

# prepare empty dataframe to take the daily values
l <- dim(dlyr)[1]
cdf <- data.frame(station=character(0), date=character(0),tmin=numeric(0), tmax=numeric(0), tmean=numeric(0), stringsAsFactors =F )
cdfd <- cdf

# bring into DB-format with 1 date per row
for (d in 1:31){
    cdfd[1:l,1] <- station
    # use sprintf for leading zeros in datestring
    cdfd[,2] <- paste(tmindf[,2],sprintf("%02d",dlyr[,3]),sprintf("%02d",d),sep="-") 
    for (c in 1:2){
        if (c==1) dlyr <- tmindf else dlyr <- tmaxdf
        dv <- dlyr[,v[d]] # extract values for single day
        ind9 <- which(dv == -9999) # indices for missing values
        dv <- replace(dv, ind9, NA) # replace
        cdfd [,c+2] <- dv/10
    }
    cdf <- rbind(cdf, cdfd) # append block of daily values to main dataframe
}

cdf <- cdf[order(cdf$date),] # sort dataframe
cdf[,2] <- as.Date(cdf[,2], format=ds) # convert strings to dates (not POSIXct !)
cdf <- cdf[!(is.na(cdf[,2])),] # remove rows with not existing dates like 31st April
write.table(cdf,"Temperatures.txt", row.names=F)
    
# next step: filling gaps

cdft <- cdf
cdft[,2] <- as.POSIXct(cdft[,2], format=ds) # required for conversion into time series, Date is not sufficient

tmins <- cdft[,2:3]
tmin_ts <- as.timeSeries(tmins)
tmins[,1] <- as.Date(tmins[,1])
tmin_tsi <- interpNA(tmin_ts, method = "linear")  # interpolation
tmini <- as.data.frame(tmin_tsi) 
tmini[,2] <- tmini[,1]
tmini[,1] <- as.Date(row.names(tmini))
names(tmini) <- c("date","tmin")

tmaxs <- tmins
tmaxs[,2] <- cdf[,4]
tmax_ts <- as.timeSeries(tmaxs)
tmaxs[,1] <- as.Date(tmaxs[,1])
tmax_tsi <- interpNA(tmax_ts, method = "linear")  # interpolation
tmaxi <- as.data.frame(tmax_tsi) 
tmaxi[,2] <- tmaxi[,1]
tmaxi[,1] <- tmini[,1]
names(tmaxi) <- c("date","tmax")

ylab <- "Temperature in °C"
par(mar=c(4.5,4,3.5,1)+0.1) # resizes plot box window
par(cex=0.7) # relates to fonts of all graphics
par(xpd=F) # restrict output to plot area
dlegend <- c("Tmax observed", "Tmax filled", "Tmin observed", "Tmin filled")
pcolors <- c("darkred","orange","blue","green")
tsource <- 'Data source: "NOAA Global Historical Climatology Network - Daily" and own calculations'

for (y in y1:y2){
    main <- paste("Daily Temperatures at Srinagar, Year",as.character(y))
    xlim <- c(as.Date(paste(y,"01-01",sep="-"),format=ds),as.Date(paste(y,"12-31",sep="-"),format=ds))
    ticks <- seq(xlim[1], xlim[2]+1, by = "months")
    plot(tmini, xlim=xlim, ylim=ylimi, col="green", type="l", main=main,xlab="",ylab=ylab)
    par(new=T)
    plot(tmins, xlim=xlim, ylim=ylimi, col="blue", type="l", xlab="",ylab="")
    par(new=T)
    plot(tmaxi, xlim=xlim, ylim=ylimi, col="orange", type="l", xlab="",ylab="") 
    par(new=T)
    plot(tmaxs, xlim=xlim, ylim=ylimi, col="darkred", type="l", xlab="",ylab="")
    axis(1, tck=1, at=ticks, col="grey40", lty=3, labels=F)
    axis(2, tck=1, col="grey40", lty=3, labels=F)
    legend (xlim[1]+160, 8, dlegend, lty=1, lwd=1, col=pcolors)
    mtext(tsource, side=1, line=3, outer=FALSE, cex=0.75)
     # output of charts to file
    dev.copy(png, width=800, height=500, paste("Daily_temperatures_", y, ".png", sep=""))
    dev.off()
}

# prepare data for output to text
tsouti <- cdf
tsouti [,2:3] <- tmini
tsouti [,4] <- tmaxi[2]
tsouti [,5] <- (tsouti[,3]+tsouti[,4])/2

write.table(tsouti,"Temp_interpol.txt", row.names=F)

### statistical processing
tst <- tsouti[,2:3]
tst[,2]<- tsouti[,5]
tst[,1]<-as.character(tst[,1])
names(tst)[2] = "tmean"
tsql <- "SELECT date, substr(date,1,4) year, substr(date,6,2) month, CASE WHEN substr(date,9,2) > '20' THEN 3 WHEN substr(date,9,10) BETWEEN '11' AND '20' THEN 2 ELSE 1 END decade, tmean FROM tst WHERE tmean IS NOT NULL"
tsts <- sqldf(tsql)

# calculate monthly means
tsql <- "SELECT year, month,Avg(tmean) Tmean FROM tsts GROUP BY year, month"
tstm <- sqldf(tsql)
write.table(tstm,"Temp_monthly.txt", row.names=F)

# calculate 10-day means
tsql <- "SELECT year, month,decade,Avg(Tmean) tmean FROM tsts GROUP BY year,month,decade"
tstd <- sqldf(tsql)
tstd[,3] <- (as.numeric(tstd[,2])-1)*3+tstd[,3]
write.table(tstm,"Temp_10day.txt", row.names=F)

# quantile list
#ql <- c(0.05, 0.2, 0.333, 0.5, 0.667, 0.8, 0.95)
ql <- c(0.05, 0.2, 0.5, 0.8, 0.95)
quant <- quantile(tstm[,3],probs=ql,names=T, type=6)
qdt <- as.table(quant)
qdf <- as.data.frame(qdt)
qll <- as.vector(qdf[,1])
qdf <- data.frame(Period=character(0), Q_1=numeric(0), Q_2=numeric(0), Q_3=numeric(0), Q_4=numeric(0), Q_5=numeric(0), stringsAsFactors =F)
names(qdf)[2:6] <- qll
qdfd <- qdf

# calculate cumulative frequencies for each month
for (m in 1:12) {
    tstmi <- subset(tstm, (tstm["month"]==sprintf("%02d",m)))
    quant <- quantile(tstmi[,3],probs=ql,names=F, type=6)
    qdf[m,] <- c(m,quant)
  }
write.table(qdf,"Temp_monthly_stats.txt", row.names=F)

# calculate cumulative frequencies for each 10-day period
for (d in 1:36) {
    tstdi <- subset(tstd, (tstd["decade"]==d))
    quant <- quantile(tstdi[,4],probs=ql,names=F, type=6)
    qdfd[d,] <- c(d,quant)
  }
write.table(qdf,"Temp_10day_stats.txt", row.names=F)

ylab <- "Mean Temperature in °C"
pcolors <- c("blue", "cornflowerblue",  "green", "red", "magenta")
xlabels <- substr(month.name,1,3)
tsource <- tsource <- 'Data source: "NOAA Global Historical Climatology Network - Daily" period 1973-2011 and own calculations'

# output per month
ctitle <- "Mean monthly temperatures for Srinagar, different empirical frequencies and year "

for (y in y1:y2) {
    ytitle <- paste(ctitle,y)
    ti <- subset(tstm, tstm[,1]==sprintf("%04d",y))
    ti [,1] <- c(1:12)
    plot(ti[,3],ylim=ylima,type="b",pch=15,main=ytitle,ylab=ylab,xlab="",labels=F,tck=0)
    axis(1, at=1:12, tck=1, col="grey40", lty=3, labels=xlabels, tick=T)
    axis(2, tck=1, col="grey40", lty=3, labels=T, tick=T)
    legend (title="cum. P",6.5, 12, qll[5:1], lty=1, lwd=c(2,2,4,2,2), col=pcolors[5:1])

    for (c in 2:6) {
        par(new=T) # this adds another curve to the existing plot!
        if (c==4) lwd <- 4 else lwd <-2
        plot(qdf[,c],ylim=ylima, col=pcolors[c-1],type="l",lwd=lwd,ylab="",xlab="",labels=F,tck=0)
     }
   par(new=T)
   plot(ti[,3],ylim=ylima,type="b",lwd=2, pch=15,main=ytitle,ylab=ylab,xlab="",labels=F,tck=0)    
   mtext(tsource, side=1, line=3, outer=FALSE, cex=0.75)
   # output of charts to file
   dev.copy(png, width=800, height=500, paste("M_temperatures_", y, ".png", sep=""))
   dev.off()
} # end of loop over years
        
# output per decade
ctitle <- "Mean temperatures per 10-day interval for Srinagar, different empirical frequencies and year "

for (y in y1:y2) {
    ytitle <- paste(ctitle,y)
    ti <- subset(tstd, tstd[,1]==sprintf("%04d",y))
    plot(ti[,4],ylim=ylima,type="b",pch=15,main=ytitle,ylab=ylab,xlab="",labels=F,tck=0)
    axis(1, at= 3*(1:12)-2, tck=1, col="grey40", lty=3, labels=xlabels,tick=T)
    axis(2, tck=1, col="grey40", lty=3, labels=T, tick=T)
    legend (title="cum. P",18, 12, qll[5:1], lty=1, lwd=c(2,2,4,2,2), col=pcolors[5:1])

    for (c in 2:6) {
        par(new=T) # this adds another curve to the existing plot!
        if (c==4) lwd <- 4 else lwd <-2
        plot(qdfd[,c],ylim=ylima, col=pcolors[c-1],type="l",lwd=lwd,ylab="",xlab="",labels=F,tck=0)
     }
   par(new=T)
   plot(ti[,4],ylim=ylima,type="b",lwd=2, pch=15,main=ytitle,ylab=ylab,xlab="",labels=F,tck=0)    
   mtext(tsource, side=1, line=3, outer=FALSE, cex=0.75)
   # output of charts to file
   dev.copy(png, width=800, height=500, units = "px", pointsize= 12, paste("D10_temperatures_", y, ".png", sep=""))
   dev.off()
} # end of loop over years

    