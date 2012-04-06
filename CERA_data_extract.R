# probe and export data from CERA grib files
# script version 2012-03-13 

library(sqldf)

# change settings here --------------------------------------------------
setwd('C:/geo_data/CERA/MPEH5/')
command <- "C:/Tools/ndfd/degrib/bin/degrib"
pntFile <- "Mangla_MPEH5.pnt"
TiggeOutput <- "MPEH5_SRA2_3_G_pr_1-1200" # change the file name (without file extension)!
#SRA2 = Scenario A2
#3 = run number
#G = grib (not NCEP)
#pr = precipitation
#tas = temperature near surface
npt <- 2 # number of points to probe
p1 <- 1 ; p2 <- 15 # reduce overlap (forecast days) of plotting
tf <- "temp_interpol.txt" # verfication data, temperature
oro.f <- "MPEH5_oro.prb" # altitudes of cells
tgrad <- 0.65 # decrease in temperature per 100m
href <- 1587  # altitude of reference station Srinagar
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

# get altitude correction factors
oro.df <- read.table(oro.f, header=T, sep = ",")
for (c in 1:npt) oro.df[2,4+c] <- (oro.df[1,4+c]-href)/100*tgrad

comProbe <- "-P -pntFile"
comUnit <- "-Unit m" # metric
comOutput <- "-out"
Fin <- paste(TiggeOutput,"grb",sep=".")
Fout <- paste(TiggeOutput,"prb",sep=".")

degrib <- paste(command,Fin,comProbe,pntFile,comOutput,Fout,comUnit)
shell(cmd=degrib) # execute the command

# Error message of degrib:
# Problems with ReadGrib1Record called by ReadGrib2Record
# Inside ReadGrib1Record
# Grid projection number is 4
# Don't know how to handle this grid projection.


grb.df <- read.table(Fout, header=T, sep = ",")
grb.df[,3] <- as.Date(as.POSIXct(as.character(grb.df[,3]),format="%Y%m%d%H%M", origin="1960-01-01", tz="GMT"))
grb.df[,4] <- as.POSIXct(as.character(grb.df[,4]),format="%Y%m%d%H%M", origin="1960-01-01", tz="GMT")
tmin.df <- grb.df[0,3:(npt+4)] # df for minimum temperatures
tmin.df[,2] <- as.Date(tmin.df[,4], origin="1960-01-01", tz="GMT")
tmax.df <- tmin.df
tavg.df <- tmin.df

# some statistics
fcs1 <- min(grb.df[,3])
fcs2 <- max(grb.df[,3])
fce1 <- as.Date(min(grb.df[,4]))
fce2 <- as.Date(max(grb.df[,4]))
nd <- nrow(subset(grb.df,refTime==grb.df[1,3]))
fdays <- nd/(nday*2) # 2 records per time period
nr <- nrow(grb.df) # total number of rows of original table
nper <- (nr/fdays)%/%(nday*2) # number of forecast periods
fdays <- (fdays+1)%/%1

tsql <- "SELECT DISTINCT refTime FROM 'grb.df'"
rf.df <- sqldf(tsql)
rf.df[,1] <- as.Date(rf.df[,1])

# delete days with incomplete forecasts?

# get min-max for every day and cell
r <- 1
for (per in 1:nper){
    fc.df <- subset(grb.df, as.Date(refTime)==rf.df[per,1])
    fc.df[,npt+5] <- as.Date(fc.df[,4])
    names(fc.df)[npt+5] <- "fday"
    tsql <- "SELECT DISTINCT fday FROM 'fc.df'"
    fd.df <- sqldf(tsql)
    for (fc in 1:fdays){
        d.df <- subset(fc.df, fday==fd.df[fc,1])
        tmin.df[r,1] <- d.df[1,3]
        tmin.df[r,2] <- d.df[1,npt+5]
        tavg.df[r,1:2] <- tmax.df[r,1:2] <- tmin.df[r,1:2]
        #tavg.df[r,1:2] <- tmin.df[r,1:2]
        for (c in 1:npt){
            tmin.df[r,2+c] <- min(d.df[,4+c]) + oro.df[2,4+c]
            tmax.df[r,2+c] <- max(d.df[,4+c]) + oro.df[2,4+c]
            tavg.df[r,2+c] <- (tmin.df[r,2+c]+ tmax.df[r,2+c])/2
        }
        r <- r+1
    }
}

# define log file
logger <- "tigge_temp.log"
write(as.character(Sys.time()),logger,append=T)
write(getwd(),logger,append=T)
write(Fin,logger,append=T)
write(paste("forecasts issued from",fcs1,"to",fcs2),logger,append=T)
write(paste("forecasts extend from",fce1,"to",fce2),logger,append=T)
write(paste(nper,"forecast periods with",fdays,"days length each"),logger,append=T)

# # take values from selected points
# crbd <- grb.df[,7:8] # center points only
# grb.df <- cbind(grb.df, rowMeans(crbd))
# names(grb.df)[5+npt] <- "MeanC"
# 
# # Combine the nday records into single record
# dtmp.df <- data.frame(rdate=as.Date(1,origin="1970-01-01"),vdate=as.Date(1,origin="1970-01-01"), tmin=numeric(1),tmax=numeric(1),tmean=numeric(1))
# nr <- nrow(grb.df)
# ri <- 0
# for (rw in seq(1,nr,nday)){
#     ri <- ri +1
#     dtmp.df[ri,1] <- as.Date(grb.df[rw,3]) 
#     dtmp.df[ri,2] <- as.Date(grb.df[rw,4])
#     dtmp.df[ri,4] <- grb.df[rw,5+npt]
#     dtmp.df[ri,3] <- grb.df[rw+1,5+npt]
#     dtmp.df[ri,5] <- ((dtmp.df[ri,3]+dtmp.df[ri,4])/2)*ff + fa
# }

write.table(tmin.df, paste(TiggeOutput,"min","txt",sep="."), row.names=F)
write.table(tmax.df, paste(TiggeOutput,"max","txt",sep="."), row.names=F)
write.table(tavg.df, paste(TiggeOutput,"avg","txt",sep="."), row.names=F)

# get verification data
t.df <- read.table(tf, header=T)
t.df[,2] <- as.Date(t.df[,2])
tlim <- c(fcs1,fce2)
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

# for (per in 1:nper){
#     d_ref <- dtmp.df[((per-1)*(fdays)+1),1]
#     ci.df <- subset (dtmp.df,rdate==d_ref)
#     par(new=T)
#     plot (x=ci.df[p1:p2,2], y=ci.df[p1:p2,5], type="l",xlim=tlim,ylim=ylim, col=colors[(per%%2+1)], lwd=2, ylab="",xlab="")
#     Tout <- paste(TiggeOutput,sprintf("%02d",per),"txt",sep=".")
#     write.table(ci.df, Tout, row.names=F)  
# } # end loop over forecast series

tsource <-"Data sources: NOAA Srinagar observations for reference and TIGGE/ECMF-Global Ensemble Forecast System"
mtext(tsource, side=1, line=3, outer=FALSE, cex=0.75)
par(xpd=T) # for legend outside of plot area
tleg <- c("ref. mean", "ref. extreme","forecast 1", "forecast 2")
legend (title="time series", fce2+nper/2, ylim[2], tleg, lty=c(1,3,1,1), lwd=c(2,1,2,2), col=c("red","red","blue","green"))
par(xpd=F) 
# output of chart to file
outfile.g <-paste(TiggeOutput,"png",sep=".")
dev.copy(png, width=1000, height=625, units="px", pointsize=18, outfile.g)
dev.off()
