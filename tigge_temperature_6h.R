# probe and export temperature data from TIGGE grib file
# 'step'    : "6/to/366/by/12" 
# first value daily max (11:00), 2nd value daily min(23:00) Pakistan time
# 'param'   : "167" - temperature snapshot
# show individual forecast series
# script version 2012-01-17 (minor improvements in chart)

library(sqldf)

# change settings here --------------------------------------------------
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Forecasts/Tigge/')
command <- "C:/Tools/ndfd/degrib/bin/degrib"
pntFile <- "Mangla.pnt"
TiggeOutput <- "2007_ncep_tmp_v2a" # change the file name (without file extension)!
#egrr # UK met office
#cwao # Canada
#ncep # NCEP USA
#ecmf # ECMF Europe
npt <- 6 # number of points to probe
p1 <- 1 ; p2 <- 15 # reduce overlap (forecast days) of plotting
ff <- 0.94 ; fa <- 10.79 # to adjust from forecasted data to reference station
tf <- "temp_interpol.txt" # verfication data, temperature
# end of code section with editable settings ----------------------------

# function for making good tick marks -----------------------------------
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
# -----------------------------------------------------------------------

comProbe <- "-P -pntFile"
comUnit <- "-Unit m" # metric
comOutput <- "-out"
Fin <- paste(TiggeOutput,"grib",sep=".")
Fout <- paste(TiggeOutput,"prb",sep=".")

degrib <- paste(command,Fin,comProbe,pntFile,comOutput,Fout,comUnit)
shell(cmd=degrib) # execute the command

grb.df <- read.table(Fout, header=T, sep = ",")
grb.df[,3] <- as.POSIXct(as.character(grb.df[,3]),format="%Y%m%d%H%M", origin="1960-01-01", tz="GMT")
grb.df[,4] <- as.POSIXct(as.character(grb.df[,4]),format="%Y%m%d%H%M", origin="1960-01-01", tz="GMT")

# some statistics
fcs1 <- as.Date(min(grb.df[,3]))
fcs2 <- as.Date(max(grb.df[,3]))
fce1 <- as.Date(min(grb.df[,4]))
fce2 <- as.Date(max(grb.df[,4]))
nd <- nrow(subset(grb.df,refTime==grb.df[1,3]))
fdays <- nd%/%2
nr <- nrow(grb.df) # total number of rows of original table
nper <- nr/fdays/2 # number of forecast periods
# delete half-day records
if (fdays != nd/2) {
    for (f in 1: nper) grb.df <- grb.df[-(f*nd-f+1),] # remove last layer for each forecast
}
nr <- nrow(grb.df) # total number of rows of original table
nper <- nr/fdays/2 # number of forecast periods

# define log file
logger <- "tigge_temp.log"
write(as.character(Sys.time()),logger,append=T)
write(getwd(),logger,append=T)
write(Fin,logger,append=T)
write(paste("forecasts issued from",fcs1,"to",fcs2),logger,append=T)
write(paste("forecasts extend from",fce1,"to",fce2),logger,append=T)
write(paste(nper,"forecast periods with",fdays,"days length each"),logger,append=T)

# take values from selected points
crbd <- grb.df[,7:8] # center points only
grb.df <- cbind(grb.df, rowMeans(crbd))
names(grb.df)[5+npt] <- "MeanC"

# Combine the 2 records (daily min, max) into single record
dtmp.df <- data.frame(rdate=as.Date(1,origin="1970-01-01"),vdate=as.Date(1,origin="1970-01-01"), tmin=numeric(1),tmax=numeric(1),tmean=numeric(1))
nr <- nrow(grb.df)
ri <- 0
for (rw in seq(1,nr,2)){
    ri <- ri +1
    dtmp.df[ri,1] <- as.Date(grb.df[rw,3]) 
    dtmp.df[ri,2] <- as.Date(grb.df[rw,4])
    dtmp.df[ri,4] <- grb.df[rw,5+npt]
    dtmp.df[ri,3] <- grb.df[rw+1,5+npt]
    dtmp.df[ri,5] <- ((dtmp.df[ri,3]+dtmp.df[ri,4])/2)*ff + fa
}

# get verification data
t.df <- read.table(tf, header=T)
t.df[,2] <- as.Date(t.df[,2])
tlim <- c(min(dtmp.df$vdate),max(dtmp.df$vdate))
t.df <- subset(t.df, date>= tlim[1] & date<=tlim[2])
t.df <- na.omit(t.df) # remove NA in reference time series
t.df[,6] <- t.df[,4]-t.df[,3]
names(t.df)[6] <- "dt"

# plot formatting
par(mar=c(4.5,4,4,10)+0.1) # resizes plot box window
par(cex=0.75)
tmain <- paste("Alternate Temperature Reforecasts - Series",TiggeOutput)
ticks <- myticks(tlim)
var <- "TMP"
ylab <- "Temperature in °C"
colors <- c(rgb(0,0,255,192,maxColorValue=255),rgb(0,255,0,192,maxColorValue=255))
tred <- rgb(255,0,0,192,maxColorValue=255)

ylim <- c(min(t.df$tmin),max(t.df$tmax))
lwd <- c(1,1,2)
lty <- c(3,3,1)

for (s in 3:5){
    if (s>3) par(new=T)  
    plot (x=t.df$date, y=t.df[,s], type="l", xlim=tlim,ylim=ylim, col=tred, lty=lty[s-2],lwd=lwd[s-2], ylab=ylab,xlab="", main=tmain)
} # end plotting of reference data
grid(nx=NA, ny=NULL)
axis(1, tck=1, at=ticks, col="grey80", lty=3, labels=F)

for (per in 1:nper){
    d_ref <- dtmp.df[((per-1)*(fdays)+1),1]
    ci.df <- subset (dtmp.df,rdate==d_ref)
    par(new=T)
    plot (x=ci.df[p1:p2,2], y=ci.df[p1:p2,5], type="l",xlim=tlim,ylim=ylim, col=colors[(per%%2+1)], lwd=2, ylab="",xlab="")
    Tout <- paste(TiggeOutput,sprintf("%02d",per),"txt",sep=".")
    write.table(ci.df, Tout, row.names=F)  
} # end loop over forecast series

tsource <-"Data sources: NOAA Srinagar observations for reference and TIGGE/NCEP-Global Ensemble Forecast System"
mtext(tsource, side=1, line=3, outer=FALSE, cex=0.75)
par(xpd=T) # for legend outside of plot area
tleg <- c("ref. mean", "ref. extreme","forecast 1", "forecast 2")
legend (title="time series", max(dtmp.df$vdate)+nper/2, ylim[2], tleg, lty=c(1,3,1,1), lwd=c(2,1,2,2), col=c("red","red","blue","green"))
par(xpd=F) 
# output of chart to file
outfile.g <-paste(TiggeOutput,"png",sep=".")
dev.copy(png, width=1000, height=625, units="px", pointsize=18, outfile.g)
dev.off()
