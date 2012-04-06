# probe and export temperature data from TIGGE grib file
# calculate correlation forecast to observed
# 'step'    : "6/18" 
# first value daily max (11:00), 2nd value daily min(23:00) Pakistan time
# 'param'   : "167" - temperature snapshot
# script version 06/01/2012

library(sqldf)

# change settings here --------------------------------------------------
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Forecasts/Tigge/')
command <- "C:/Tools/ndfd/degrib/bin/degrib"
pntFile <- "Mangla.pnt"
TiggeOutput <- "ncep_tmp_v2a-calib" # change the file name (without file extension)!
#egrr # UK met office
#cwao # Canada
#ncep # NCEP USA
#ecmf # ECMF Europe
npt <- 6 # number of points to probe
pm1 <- 2 ; pm2 <- 2 # reduce forecast days for linear model
tf <- "temp_interpol.txt" # verfication data, temperature
# end of code section with editable settings ----------------------------

comProbe <- "-P -pntFile"
comUnit <- "-Unit m" # metric
comOutput <- "-out"
Fin <- paste(TiggeOutput,"grib",sep=".")
Fout <- paste(TiggeOutput,"prb",sep=".")
fctxt <- if (pm1==1) "same" else "next"

degrib <- paste(command,Fin,comProbe,pntFile,comOutput,Fout,comUnit)
shell(cmd=degrib) # execute the command

grb.df <- read.table(Fout, header=T, sep = ",")
grb.df[,3] <- as.POSIXct(as.character(grb.df[,3]),format="%Y%m%d%H%M", origin="1960-01-01", tz="GMT")
grb.df[,4] <- as.POSIXct(as.character(grb.df[,4]),format="%Y%m%d%H%M", origin="1960-01-01", tz="GMT")

# define log file
logger <- "tigge_temp.log"
write(as.character(Sys.time()),logger,append=T)
write(getwd(),logger,append=T)
write(Fin,logger,append=T)

# check for missing layers
nd <- nrow(subset(grb.df,refTime==grb.df[1,3]))
fdays <- nd%/%2
if (fdays != nd/2) {
    for (f in 1: nper) grb.df <- grb.df[-(f*nd-f+1),] # remove last layer for each forecast
}

# take values from selected points
crbd <- grb.df[,7:8] # center points only
grb.df <- cbind(grb.df, rowMeans(crbd))
names(grb.df)[5+npt] <- "MeanC"

# calculate statistics from 2 observations per day

dtmp.df <- data.frame(rdate=as.Date(1,origin="1970-01-01"),vdate=as.Date(1,origin="1970-01-01"), tmin=numeric(1),tmax=numeric(1),tmean=numeric(1))
nr <- nrow(grb.df)
ri <- 0
for (rw in seq(1,nr,2)){
    ri <- ri +1
    dtmp.df[ri,1] <- as.Date(grb.df[rw,3]) 
    dtmp.df[ri,2] <- as.Date(grb.df[rw,4])
    dtmp.df[ri,4] <- grb.df[rw,5+npt]
    dtmp.df[ri,3] <- grb.df[rw+1,5+npt]
    dtmp.df[ri,5] <- (dtmp.df[ri,3]+dtmp.df[ri,4])/2
}

# get verification data
t.df <- read.table(tf, header=T)
t.df[,2] <- as.Date(t.df[,2])
tlim <- c(min(dtmp.df$vdate),max(dtmp.df$vdate))
t.df <- subset(t.df, date>= tlim[1] & date<=tlim[2])
t.df <- na.omit(t.df) # remove NA in reference time series
t.df[,6] <- t.df[,4]-t.df[,3]
names(t.df)[6] <- "dt"

# calculate linear model
dtmp.df[,6] <- as.integer(dtmp.df[,2]-dtmp.df[,1]+1) # forecast period
names(dtmp.df)[6] <- "fcd"
tsql <- paste("SELECT 'dtmp.df'.vdate, 't.df'.tmean ref, 'dtmp.df'.tmean fc FROM 'dtmp.df' INNER JOIN 't.df' ON 'dtmp.df'.vdate = 't.df'.date WHERE fcd <=",pm2,"AND fcd >=",pm1)
dm.df <- sqldf(tsql)
clip <- -20
dm.df <- subset(dm.df, fc> clip)
fm <- lm(dm.df[,2]~dm.df[,3]) # general

c1 <- summary(fm)$coefficients[1]
c2 <- summary(fm)$coefficients[2]
r2 <- summary(fm)$r.squared
dm.df[,4] <- dm.df[,3]*c2 + c1
names(dm.df)[4] <- "tadj"

# plot formatting
par(mar=c(6.5,4,4,3)+0.1) # resizes plot box window
xlim <- c(-15,20)
ylim <- c(-5,30)
xlab <- "Forecast temperature (average of 2 grid cells) in °C"
ylab <- "Observed temperature at Srinagar in °C"
par(cex=0.85)
tred <- rgb(255,0,0,96,"",255)
tmain <- paste("Comparison between NCEP forecast (",fctxt,"day) and observed mean daily temperatures")
plot(x=dm.df[,3],y=dm.df[,2],col="lightblue",xlim=xlim,ylim=ylim, xlab=xlab, ylab=ylab, main=tmain)
par(new=T)
curve(c2*x+c1, from = -20, to = 25, col=tred,xlim=xlim,ylim=ylim, lwd=2, xlab="", ylab="")
grid()
ftext <- paste("Y=",sprintf("%.2f",c2),"*","X +", sprintf("%.2f",c1))
rtext <- paste("correlation coefficient R² =", sprintf("%.3f",r2))
mtext(ftext, side=1, line = -13.5, at=15, cex=0.8)
mtext(rtext, side=1, line = -12, at=15, cex=0.8)
tsource <-"Data sources: NOAA Srinagar observations for reference and TIGGE/NCEP-Global Ensemble Forecast System, March 2007 - August 2011"
mtext(tsource, side=1, line=5, outer=FALSE, cex=0.7)

# output of chart to file
outfile.g <-paste(TiggeOutput,"png",sep=".")
dev.copy(png, width=1000, height=625, units="px", pointsize=18, outfile.g)
dev.off()
write.table(grb.df,paste(TiggeOutput,"_in.txt",sep=""),row.names=F)
write.table(dm.df,paste(TiggeOutput,"_out.txt",sep=""),row.names=F)

