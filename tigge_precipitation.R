# spatial grid from probed precipitation data
# 'step'    : "24/to/360/by/24"
# 'param'   : "228228" - cumulative precipitation starting at time 0
# show individual forecast series
# script version 2012-01-21

library(sp)
library(rgdal)
library(raster)

# change settings here --------------------------------------------------
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Forecasts/Tigge/')
command <- "C:/Tools/ndfd/degrib/bin/degrib"
pntFile <- "Mangla_tprec.pnt"
comProbe <- "-P -pntFile"
comUnit <- "-Unit m"
comOutput <- "-out"
TiggeOutput <- "2007_ecmf_tprc_la" # change the file name (without file extension)!
#egrr # UK met office
#cwao # Canada
#ncep # NCEP USA
#ecmf # ECMF Europe
var <- "TPRATE"
oro.df <- read.table(pntFile, header=F, skip=2, sep=",")
coref <-CRS("+proj=longlat +a=6371229 +b=6371229 +no_defs")
SRM_zones <- "catchmnt0.tif" # contains raster definition (projection, limits, cellsize)
glistm <- list.files(pattern="ezmask_0_") # get list of masks for elevation zones
p_ref <- "rainfall_00.txt" # daily rainfall from FEWS

p1 <- 1 
p2 <- 15 # reduce overlap of plotting
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

Fin <- paste(TiggeOutput,"grib",sep=".")
Fout <- paste(TiggeOutput,"prb",sep=".")
Year <- substr(TiggeOutput,1,4)

degrib <- paste(command,Fin,comProbe,pntFile,comOutput,Fout,comUnit)
shell(cmd=degrib) # execute the command

# read complete table from degrib command
grb.df <- read.table(Fout, header=T, sep = ",")
grb.df[,3] <- as.Date(as.character(grb.df[,3]),format="%Y%m%d%H%M", origin="1960-01-01", tz="GMT")
grb.df[,4] <- as.Date(as.character(grb.df[,4]),format="%Y%m%d%H%M", origin="1960-01-01", tz="GMT")
nr <- nrow(grb.df) # total number of rows of original table
fdays <- nrow(subset(grb.df,grb.df[,3]==grb.df[1,3])) # number of forecast days
nper <- nr/fdays # number of forecast periods

ez.r <- raster (SRM_zones)
em.r <- stack(glistm) # add all masks to a raster stack
ezn <- nlayers(em.r)
e.df <- data.frame(1:fdays) 
for (c in 1:(ezn-1)) e.df <- cbind (e.df, data.frame(1:fdays))
names(e.df) <- seq(1:ezn)

dmin <- min(grb.df[,4])
dmax <- max(grb.df[,4])
dref <- as.integer(dmax-dmin+1)

# get observed precipitation data
ref.df <- read.table(p_ref, header=T)
ref.df[,1] <- as.Date(as.character(ref.df[,1]),format="%Y-%m-%d", origin="1960-01-01", tz="GMT")
ref.df <- subset(ref.df, date >= dmin & date <= dmax)
pref.df <- data.frame(date=integer(0))
for (ez in 1: ezn) pref.df <- cbind (pref.df, data.frame(ez=numeric(0)))
names(pref.df)[2:(ezn+1)] <- seq(1:ezn)
pref.df[,1] <- as.Date(pref.df[,1],origin="1960-01-01", tz="GMT")
d <- 1
pref.df [d,1] <- ref.df[1,1]
for (d in 1:dref){
    rn <- ((d-1)*ezn)+1
    pref.df[d,1] <- ref.df[rn,1]
    pref.df[d, 2:(ezn+1)] <- ref.df[rn:(rn+ezn-1),3]
}   

# process forecasted data
for (per in 1:nper){
    l1 <- (per-1)*fdays+1
    l2 <- (per*fdays)
    pgrb.df <- subset(grb.df[l1:l2,])
    pez.df <- pgrb.df[,3:4]
    pez.df <- cbind(pez.df,e.df)
    for (d in 1:fdays){
        dt <- l1-2 +d
        if (d!=1) pgrb.df[d,5:13] <-pgrb.df[d,5:13]-grb.df[dt,5:13]
        d.sp <- cbind(oro.df[,2:3], as.numeric(pgrb.df[d,5:13]))
        names(d.sp) <- c("Y","X","Z")
        prc.spdf <- SpatialPointsDataFrame(coords=d.sp[,2:1], data=d.sp[,3,drop=F], proj4string=coref)
        gridded(prc.spdf) <- T # convert spatial-points to spatial-grid
        prc.rr <- raster(prc.spdf)
        # writeRaster(prc.rr,"prc.tif") - test only
        # reproject raster to geographic properties of model grid
        prcd.rr <- projectRaster(from=prc.rr, to=ez.r) 
        # writeRaster(prcd,"prcd.tif") - test only
        for (ez in 1:ezn){  # range of ez 
            c <- subset(em.r,ez) * prcd.rr # make subset of raster using mask 
            # get statistics, convert to cm
            pez.df[d,ez+2] <- max(as.integer(cellStats(c, 'mean')*10 )/100,0) 
        } # end loop on elevation zones
    } # end of days within a forecast series
    if (per==1) peza.df <- pez.df else peza.df <- rbind(peza.df, pez.df)
    fname <- paste(TiggeOutput,sprintf("%02d",per),"txt",sep=".")
    write.table(pez.df, file = fname, row.names=F)
} # end of forecast series

# plot comparisons of forecasted and observed data
par(mar=c(4.5,4,4,10)+0.1) # resizes plot box window
par(cex=0.8)
ylab <- c("Daily Rainfall in mm")
colors <- c("blue","green")
tlim <- c(dmin,dmax)
ylim <- c(0,50)
ticks <- myticks(tlim)

for (ez in 1:ezn){  # range of ez
    tmain <- paste("Comparison of observed (FEWS) and forecasted rainfall - elevation zone",ez,"- Year",Year)
    plot(x=pref.df[,1], y=pref.df[,1+ez]*10, ylab=ylab, xlab="", col="red", ylim=ylim, xlim=tlim, main=tmain)
    grid(nx=NA, ny=NULL)
    axis(1, tck=1, at=ticks, col="grey80", lty=3, labels=F)
    for (per in 1:nper){
        par(new=T)
        pp1 <- (per-1)*fdays+p1
        pp2 <- (per-1)*fdays+p2
        plot(x=peza.df[pp1:pp2,2]+(per%%2+1)*0.5, y=peza.df[pp1:pp2,(ez+2)]*10, type="h", ylab="", xlab="", col=colors[(per%%2+1)], ylim=ylim, xlim=tlim)
    } # end of forecast series
    tsource <-"Data sources: NOAA/FEWS rainfall raster for reference and TIGGE/ECMF-Global Ensemble Forecast System"
    mtext(tsource, side=1, line=3, outer=FALSE, cex=0.75)
    par(xpd=T) # for legend outside of plot area
    tleg <- c("reference","forecast 1", "forecast 2")
    legend (title="time series", tlim[2]+nper/2, ylim[2], lty=1,tleg, col=c("red","blue","green"))
    par(xpd=F)
    # output of chart to file
    outfile.g <-paste(TiggeOutput,ez,"png",sep=".")
    dev.copy(png, width=1000, height=625, units="px", pointsize=18, outfile.g)
    dev.off()
} # end of ez

