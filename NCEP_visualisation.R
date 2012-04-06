# visualize NCEP predictions 
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

# change settings here --------------------------------------------------
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Forecasts/CFS/dl/')
varid <- "tmp2m" # either tmp2m or prate
date1 <- as.Date("2011-11-23")
date2 <- as.Date("2011-12-02")
#date2 <- as.Date("2011-11-28")
dur <- 270 # duration of forecast-analysis in days
mystats <- 4 #  to extract data from stored d.df. 4 = average, 5 = moving average
stext <- ifelse(mystats==4, "Daily Values","9-Days Moving Averages")
# end of code section with editable settings ----------------------------

# general plot settings
par(mar=c(4.5,4,4,9)+0.1) # resizes plot box window
par(cex=0.7)
ylab <- ifelse(varid=="tmp2m", "Temperature in °C", "Precipitation in mm")
ylabi <- ifelse(varid=="tmp2m", "Temperature", "Precipitation")
if (varid=="tmp2m") ylim <- ylim <- c(-15, 0) else ylim <- c(-0, 50)
tmain <- paste("9-Months",ylabi,"Forecast for Mangla Catchment")
tmain <- paste("  ",tmain," - Comparison of Ensemble Members for",stext)
colors <- c("red","green","blue","orange")
xlim <- c(date1,date1+dur)
ticks <- myticks(xlim)
ndays <- as.character(as.integer(date2 - date1)+1)
tsource <- paste('Data source: The NCEP Climate Forecast System Version 2 (CFSv2)',ndays,"days each with 4 daily grib files")

# Compare the summaries of 4 different simulations of d days
c <- 1
di <- 0
for (d in date1:date2){
    idate <- paste(gsub("-","",as.character(as.Date(d))),"00",sep="")
    for (n in 1:4){
        c <- c+1 # column for string data in s.df
        tfile <- paste(varid, sprintf("%02d",n), idate, "txt",sep=".")
        d.df <- read.table(tfile, header=T)
        
        if (n==1 & d==date1) {
            s.df <- d.df[1:dur,] # summary df
            s.df[,2:5] <- NA
            s.df[,1] <- as.Date(s.df[,1])
        }
        
        for (r in 1: dur) s.df[r+di,c] <- as.integer(d.df[r,mystats]*10)/10 # stagger and reformatting
        
        names(s.df)[c] <- paste(d,sprintf("%02d",n),sep=".")
        if (n==1 & d==date1){
            plot(s.df[,1:2], type="l", col="red", xlim=xlim, ylim=ylim, xlab="", ylab=ylab, main=tmain)
        } else {
            par(new=T)
            plot(x=s.df[,1], y=s.df[,c], type="l", col=colors[n], xlim=xlim, ylim=ylim, xlab="", ylab="", main="")
        } # end of if
    } # end of loop over 4 simulations
    di <- di + 1
} # end of d

grid(nx=NA, ny=NULL)
axis(1, tck=1, at=ticks, col="grey80", lty=3, labels=F)
mtext(tsource, side=1, line=3, outer=FALSE, cex=0.7)
par(xpd=T) # for legend outside of plot area
    legend (title="forecasts",x=xlim[2]+as.integer((xlim[2]-xlim[1])/15), y=ylim[2], c(1:4), lty=1, lwd=1, col=colors[1:4])
par(xpd=F)
outfile.t <- paste(varid, idate, as.character(mystats),"txt",sep=".")
write.table(s.df, outfile.t, col.names=T, row.names=FALSE, append=F)    # output to file
outfile.g <- paste(varid, idate, as.character(mystats),"ind","png",sep=".")
dev.copy(png, width=1000, height=625, units="px", pointsize=18, outfile.g)
dev.off()

# Compare the summaries of d days separate for each simulation (1:4)
colors <- c("black", "red", "green", "blue","darkred","black", "red", "green", "blue","darkred")
lty <- c(1,1,1,1,1,2,2,2,2,2)
nc <- ncol(s.df)
for (n in 1:4){
    tmain <- paste("Comparison of daily values for forecasts on consecutive days from CFSv2, prediction series #", as.character(n))
    # make subset from s.df
    cl <- append(1, seq (1+n,nc, by=4))
    sn.df <- s.df[,cl]
    ylim <- c(min(sn.df[,-1],na.rm=T), max(sn.df[,-1],na.rm=T))
    plot(sn.df[,1:2], type="l", xlim=xlim, ylim=ylim, xlab="", ylab=ylab, main=tmain)
    for (d in 3:length(cl)){
        par(new=T)
        plot(x=sn.df[,1], y=sn.df[,d], type="l", lty=lty[d-1], col=colors[d-1], xlim=xlim, ylim=ylim, xlab="", ylab="", main="")
    } # end of d
    labs <- as.Date(as.integer(substr(names(sn.df)[2:length(cl)],1,5)))
    grid(nx=NA, ny=NULL)
    axis(1, tck=1, at=ticks, col="grey80", lty=3, labels=F)
    mtext(tsource, side=1, line=3, outer=FALSE, cex=0.7)
    par(xpd=T) # for legend outside of plot area
        legend (title="forecasts",x=xlim[2]+as.integer((xlim[2]-xlim[1])/15), y=ylim[2], labs, lty=lty, lwd=1, col=colors)
    par(xpd=F)
  
    outfile.g <- paste(varid, sprintf("%02d",n), idate, as.character(mystats),"png",sep=".")
    dev.copy(png, width=1000, height=625, units="px", pointsize=18, outfile.g)
    dev.off()  
} # end of n

# summarize results / statistics
nr <- dur-4
sod.df <- s.df[5:nr,-1]
nc <- ncol(sod.df)
ss.df <- s.df[5:nr,1:6]
nr <- nr-4
for (c in 1:nr) {
    ss.df[c,2] <- max(sod.df[c,],na.rm=T)  
    ss.df[c,3] <- quantile(sod.df[c,],probs=0.8,na.rm=T)
    ss.df[c,4] <- quantile(sod.df[c,],probs=0.5,na.rm=T)
    ss.df[c,5] <- quantile(sod.df[c,],probs=0.2,na.rm=T)
    ss.df[c,6] <- min(sod.df[c,],na.rm=T)
}

names(ss.df) <- c("date","max","q_0.8","q_0.5","q_0.2","min")

# plot results - frequency analyses
tmain <- paste("Frequency Analysis of",stext ,"for different forecasts from CFSv2")
xlim <- c(ss.df[1,1], ss.df[nr,1])
ticks <- myticks(xlim)
ylim <- c(min(ss.df[,6]), max(ss.df[,2]))
colors <- c("red","red","green","blue","blue")
lty <- c(2,1,1,1,2)
lwd <- c(1,1,2,1,1)
par(cex=0.7)

plot(ss.df[,1:2], type="l", lty=2, col="red", xlim=xlim, ylim=ylim, ylab=ylab, xlab="", main=tmain)
for (c in 3:6){
    par(new=T)
    plot(x=ss.df[,1], y=ss.df[,c], type="l", lty=lty[c-1], lwd=lwd[c-1], col=colors[c-1], xlim=xlim, ylim=ylim, xlab="", ylab="", main="")
}

grid (nx=NA, ny=NULL)
axis(1, tck=1, at=ticks, col="grey80", lty=3, labels=F)
par(xpd=T)
    legend (title="quantile",x=xlim[2]+as.integer((xlim[2]-xlim[1])/12), y=ylim[2], names(ss.df)[2:6], lty=lty, lwd=lwd, col=colors)
par(xpd=F)
mtext(tsource, side=1, line=3, outer=FALSE, cex=0.7)
outfile.g <- paste(varid, idate, "stats",as.character(mystats),"png",sep=".")
dev.copy(png, width=1000, height=625, units="px", pointsize=18, outfile.g)
dev.off()
