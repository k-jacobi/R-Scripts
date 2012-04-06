# import / extract GRIB2 file
# script version 05/12/2011

library (rgdal)
library (sp)
library (raster)
library(TTR) # for moving average function

# function for making good tick marks
myticks <- function (tlim){
    dlim <- as.integer(substr(as.character(tlim),9,10))
    mlim <- as.integer(substr(as.character(tlim),6,7))
    ylim <- as.integer(substr(as.character(tlim),1,4))
    mlim2 <- ifelse(dlim < 15, mlim, ifelse(mlim == 12, 1, mlim+1))
    ylim2 <- ifelse(mlim2<mlim,ylim+1,ylim)
    tlim2 <- as.Date(paste(as.character(ylim2),sprintf("%02d",mlim2),"01",sep="-"))
    ticks <- seq(tlim2[1], tlim2[2], by = "months")
    return (ticks)
}

setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Forecasts/CFS/dl/')
varid <- "tmp2m"
date1 <- as.Date("2011-11-23")
#date2 <- as.Date("2011-12-02")
date2 <- as.Date("2011-11-28")

par(mar=c(4.5,4,4,9)+0.1) # resizes plot box window
par(xpd=F)
par(cex=0.7)
ylab <- "Temperature in °C"

for (d in date1:date2){
    idate <- paste(gsub("-","",as.character(as.Date(d))),"00",sep="")

    for (n in 1:4){
        gribfile <- paste(varid, sprintf("%02d",n), idate, "daily",sep=".")
        # data are already in °C
        tmp_brick <- brick(gribfile) # 559 layers, very fast
        
        # extract time series for single coordinate
        t <- extract(tmp_brick, cellFromXY(tmp_brick, c(74,34))) # time series
        
        # get start time/date
        t0 <- as.POSIXct(substr(gribfile,10,19), format="%Y%m%d%H", tz = "GMT")
        days <- nlayers(tmp_brick)-nlayers(tmp_brick)%%2
        ti <- seq(t0, by= t0-(t0-43200), len=days)
        t.df <- data.frame(ti) # get time into df
        t.df[,2] <- data.frame(t[1:days]) # get values into df
        names(t.df) <- c('time', 'value')
        
        # calculate max, min, average
        d.df <- t.df
        d.df[,1] <- data.frame(as.Date(d.df[,1]))
        d.df <- d.df[seq(1,days,2),] 
        d.df[,1] <- as.Date(d.df[,1]) # convert to date!
        d.df[,3] <- t.df[seq(2,days,2),2]
        d.df[,4] <- (d.df[,2]+d.df[,3])/2
        
        i <- 9 # (interval of moving average)
        ih <- as.integer(9/2)
        d.df[,5] <- SMA(d.df[,4],n=i) # moving average, at end of period
        mrow <- nrow(d.df)-ih
        for (r in 1:mrow) d.df[r,5] <- d.df[r+ih,5] # average shifted from begin of period
        
        names(d.df)[2:5] <- c('min', 'max', 'avg','movavg')
        
        if (n==1 & d==date1) s.df <- d.df # summary df
    
        # plot results
        par(xpd=F)
        xlim <- c(d.df[1,1],d.df[days/2,1])
        ticks <- myticks(xlim)
        ylim <- c(-20,35)
        tmain <- "9-Months Temperature Forecast for Mangla Catchment"
        
        tsource <- paste('Data source: The NCEP Climate Forecast System Version 2 (CFSv2)'," - file: ",gribfile)
        plot (d.df[,1:2], type="l", col="red", xlim=xlim, ylim=ylim, xlab="", ylab=ylab, main=tmain )
        par(new=T)
        plot (x=d.df[,1], y=d.df[,3], col="blue", type="l", xlim=xlim, ylim=ylim, xlab="", ylab="") 
        par(new=T)
        plot (x=d.df[,1], y=d.df[,4], col="green", type="l", lwd=1, xlim=xlim, ylim=ylim, xlab="", ylab="")
        par(new=T)
        plot (x=d.df[,1], y=d.df[,5], col="darkgreen", type="l", lwd=2, xlim=xlim, ylim=ylim, xlab="", ylab="")
        grid(nx=NA, ny=NULL)
        axis(1, tck=1, at=ticks, col="grey80", lty=3, labels=F)
        mtext(tsource, side=1, line=3, outer=FALSE, cex=0.7)
        par(xpd=T) # for legend outside of plot area
        legend (title="daily val.",x=xlim[2]+14, y=ylim[2]+2, legend=c("max","avg","mova","min"), lty=1, lwd=c(1,1,2,1), col=c("red","green","darkgreen","blue"))
    
        # store results
        outfile.t <- paste(substr(gribfile,1,19),"txt",sep=".")
        write.table(d.df, outfile.t, col.names=T, row.names=FALSE, append=F)    # output to file
        outfile.g <- paste(substr(gribfile,1,17),"png",sep=".")
        dev.copy(png, width=800, height=500, units ="px", pointsize=12, outfile.g)
        dev.off()
    } # end of n
} # end of d

