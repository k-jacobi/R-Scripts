# Accumululation of snow-water equivalent
# Use of Tcrit and a with values obtained through calibration
# script version: 18/10/2011

library(raster)
library(rgdal)

options(warn=-2)

decade <- function(d) {
    if (d > 20) dc = 3 else if (d>10) dc = 2 else dc = 1
    return(dc)
}

setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Precipitation/NOAA/') # set working directory

year1 <- 2009 # first year for which SWE accumulation is to be calculated (prec data start in May 2001)
year2 <- 2009
d1 <- "09-01" # start of snowfall season, previous year
d2 <- "08-01" # end of snowfall season

Tgrad <- -0.60 # Degrees per 100 m (Temperature lapse rate or gradient)
L0 <- 1587 # Altitude Srinagar met station
srtm <- "srtm_utm.tif" # file name of elevation model in m
ezones <- "catchmnt0.tif" # file name of raster with elevation zones
tTcrit <- read.table("Tcrit.txt", header=TRUE, as.is=T) # monthly Tcrit values for Mangla
tfile <- "swe_db.txt"
a_tab <- read.table("an.txt", header=TRUE, as.is=T) # 10-day a-values for Mangla, per ez and year
a_tab[,10] <- as.integer(substr(a_tab[,1],7,10))
a_tab[,11] <- as.integer(substr(a_tab[,1],4,5))
a_tab[,12] <- as.integer(substr(a_tab[,1],1,1))+1
names(a_tab)[10:12] <- c("year","month","decade")
write(c("date","ez","area[km2]","volume[MCM]","max[cm]"), file=tfile, ncolumns=5, append = FALSE,  sep = " ")

dem <- raster(srtm)
ez.raster <- raster(ezones)
par(cex=0.7) # reduce font scale for plots
par(mar=c(4,10,4,8)+0.1) # modifies plot box window
plot (ez.raster, col=terrain.colors(11), main="Elevation Zones") # for control only
Tadj <- (dem-L0)*Tgrad/100 # Temperature adjustment by elevation (assuming no seasonal variation)

# load complete file list of reprojected rainfall TIFs into a R character vector
glistf <- list.files(pattern="....-..-.._m.tif") 
tdf <- read.table("temp_interpol.txt", header=TRUE, as.is=T) # daily temperature data
tdf[,5] <- (tdf[,3]+tdf[4])/2
names(tdf)[5] <- "tmean"

# setting filenames, assigning files to raster layer objects
glistm <- list.files(pattern="ezmask_0") # get list of masks for elevation zones - total catchment
e <- stack(glistm) # add all masks to a raster stack
R0 <- dem*0 # empty raster
a.df <- data.frame(ez=4:11, a=numeric(8)) # dataframe to store a-values for different ez
D.old <-0

for (y in year1:year2){
    cat("year:", y, " time:", format(Sys.time(),"%X"), "\n")
    dd1 <- as.Date(paste(y-1,d1,sep="-"))
    dd2 <- as.Date(paste(y,d2,sep="-"))
        
    Ras <- R0 # empty raster to store accumulated snow-water equivalent
    
    for (d in dd1:dd2){
        dc <- as.character(as.Date(d,origin="1970-01-01"))
        Y <- as.integer(substr(dc,1,4))
        M <- as.integer(substr(dc,6,7))
        D <- decade(as.integer(substr(dc,9,10)))

        Tcrit <- subset(tTcrit[,2], tTcrit[,1]==M, select=tTcrit[,2])
        if (D!=D.old){
            aa <- subset(a_tab, a_tab[,10]==Y & a_tab[,11]==M & a_tab[,12]==D) # a-values for all ez
            a.df[1:8,2] <- as.numeric(aa[1,2:9])
            a.raster <- subs(ez.raster,a.df) # substitute ez with a-values in raster           
        }
        D.old <-D
        cat("date:", dc, " time:", format(Sys.time(),"%X"), "\n")
        RfName <- paste(dc,"m.tif",sep="_")
        if (length(list.files(pattern=RfName))==0) next # error trapping if no file of this name
        prec <- raster(RfName) # load daily precipitation raster
        if (count(prec, NA)>30000) {
            cat(dc, " missing cells:", count(prec, NA) , "\n")
            next 
            }
        Td <- subset(tdf[,5], tdf[,2]==dc, select=tdf[,5])  # daily mean temperature at reference station
        Rtd <- Tadj+Td # daily temperature raster adjusted to elevation
        Rts <- Rtd<Tcrit # raster for cells < threshold Tcrit
        Rdd <- (Rtd>0)*Rtd # Degree-Days - °C > 0
        # daily snow-water equivalent
        Rds <- Rts*max(prec,0)/10 # from interpolation, prec may contain negative values
        Ras <- max(Ras+Rds-a.raster*10*Rdd, R0) # SWE accumulation - melt balance - factor 10 because of cm
        
        # statistics for each elevation zone
        for (ez in 4:11){ # range for which a-values have been obtained
            a <- aa[ez-3] # a-value for ez
             # make new vector
            dlist <- c(dc,ez, 0, 0, 0)
            c <- subset(e,ez) * Ras  # make subset of raster using mask
            # get statistics
            cCount <- cellStats(c>0, 'sum')  # put number of cells with snow 
            cSum <- cellStats(c, 'sum') # total sum of cell values
            dlist[3] <- cCount/4  # put snow area in km² into vector
            dlist[4] <- sprintf("%.2f", cSum*cCount/4000000)  # put total volume in MCM into vector
            dlist[5] <- cellStats(c, 'max')/10  # put peak swe [cm] into vector
            # write vector contents to disk
            write(dlist, file = tfile, ncolumns=5, append = TRUE,  sep = " ") 
        } # end of loop over elevation zones
        
    } # end of loop over days
    
    writeRaster(Ras,paste("amSWE_",y,".tif",sep=""),overwrite=T, datatype="INT2U")

} # end of loop over years
