# Comparison of observed and calculated snow depletion curves
# script version: 17/09/2011

library(sqldf)
library(zoo)
# library(pixmap) 
options(warn=-1)

setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Precipitation/NOAA/SDC_analysis/') # set working directory

ez_min <-4
ez_max <-10
date_min <-"2008-09-01"
date_max <-"2009-08-01"
y.str <-paste(substr(date_min,1,4),substr(date_max,1,4),sep="/")

sdc_obs <- read.table("snowcover.txt", header=TRUE, as.is=T) # observed sdc from MODIS
sdc_obs[,1]<-gsub("_", "-",sdc_obs[,1])
sdc_obs[,4]<-as.numeric(sdc_obs[,4])

sdc_calc <- read.table("swe_db_2009.txt", header=TRUE, as.is=T) # calculated from precipitation, temperature, SRM parameters
names(sdc_calc)[3] <-"area"
clist <- read.table("clist.txt", header=TRUE, as.is=T) # class definition with total area
tsql<-"SELECT clist.ez, clist.zmin, clist.zmax, clist.ncells FROM clist WHERE clist.catchment = 'Total'"
ez_classes <- sqldf(tsql)
# logo <- read.pnm("WAPDA.pnm")

ylim <-c(0,1)
xlim = c(as.Date(date_min), as.Date(date_max))
col=c(3,4) # col=c(3,2)
Days <- seq(from = as.Date(date_min), to = as.Date(date_max), by = "day")
Months <- Days[format(Days, "%d") == "01"]

par(mar=c(3,4,4,11)+0.1) # resizes plot box window
par(cex=0.8) # relates to fonts of all graphics elements
par(xpd=TRUE) # for legend outside of plot area

for (ez in ez_min:ez_max){
    main <- paste("Comparison of observed and calculated snow cover, year",y.str,", EZ=", as.character(ez))
        tsql <- paste("SELECT sdc_calc.date, sdc_obs.snow/ez_classes.ncells as obs, sdc_calc.area*4/ez_classes.ncells as calc FROM sdc_calc INNER JOIN sdc_obs ON sdc_calc.ez = sdc_obs.ez AND sdc_calc.date = sdc_obs.date INNER JOIN ez_classes ON ez_classes.ez = sdc_calc.ez WHERE sdc_calc.ez =", ez, " AND sdc_calc.date >= '", date_min, "' AND sdc_calc.date <='", date_max, "'", sep="")
    ts <- sqldf(tsql)
    ts[,1] <- as.Date(ts[,1])
    ts.zoo <- zoo(ts, order.by=ts[,1])

    plot.zoo(ts.zoo, plot.type='single', ylim=ylim, main=main, col=col, lwd=2, xlab="", ylab="snow cover ratio")
    axis(1, tck=1, at = as.numeric(Months), col="grey40", lty=3, labels = FALSE)
    axis(2, tck=1, col="grey40", lty=3, labels = FALSE)
    legend (as.Date(date_max)+18, 1.0, c("calculated","observed"), lty=1, lwd=2, col=col)
    #addlogo(logo, c(as.Date(date_max)-100,as.Date(date_max)-150), c(0.2,0.4))
    dev.copy(png, width=800, height=500, paste("SDC_compare_ez", sprintf("%02d",ez), "_",y, ".png", sep=""))
    dev.off()
} 
