# Generate TIFs with elevation zones per catchment
# script version: 02/08/2011
# generalized and adapted for Shigar catchment

# sources: 
# SRTM data (TIF, 3 arcseconds, Geographical coordinates)
# Textfile with definition of elevation zones
# Template raster (TIF) for reading geospatial definitions for target TIF

# sequence of processing: resample - classify - mask

library(rgdal)
library(raster)
options(warn=0)

setwd ("C:/AHT.PRO/Pakistan_Modelling/GIS_factory/500m/")
#setwd ("C:/AHT.PRO/Pakistan_Modelling/GIS_factory/Shigar/")

dsn <- "C:/AHT.PRO/Pakistan_Modelling/GIS_factory" # for OGR without final slash
#dsn <- "C:/AHT.PRO/Pakistan_Modelling/GIS_factory/Shigar" # for OGR without final slash
cdf <- data.frame(catchment=character(0),ez=integer(0),zmin=integer(0),zmax=integer(0),
                  ncells=integer(0),hmean=integer(0))

# setting filenames, read all data to R
# name of shapefile without extension - required for OGR - must include XML if available
shapef <- "Watersheds" 
#shapef <- "Boundary_P"

srtmf <- list.files(pattern="srtm_utm.tif")
if (length(srtmf)==0) { 
    # original file in geographical coordinates, clipped to wider area of interest
    srtm_ll <- raster("srtm_ll.tif") 
    # template is required for spatial definitions, the individual pixel information is not relevant
    e <- raster("template.tif") # original MODIS output
    srtm_utm <- projectRaster(from=srtm_ll, to=e) 
    writeRaster(srtm_utm,"srtm_utm.tif", datatype="INT2S", overwrite=TRUE)
}
srtm_utm <- raster("srtm_utm.tif") # 'else' not accepted
c <- srtm_utm * 0   # make copy of raster, all values set to 0

ezd <- read.table("ez_def.txt", header=TRUE)  # load definition of elevation zones into data frame
nez <- dim(ezd)[1] # number of elevation zones
    
shpg <- readOGR(dsn=dsn, layer=shapef)
plot(shpg) # for control only
nrec <- length(rownames(as(shpg, "data.frame")))
atab <- as(shpg, "data.frame")

# altitude classification
for (ez in 1:nez){ # elevation zones
    zmin <- ezd[ez,2] # row, column
    zmax <- ezd[ez,3]
    c <- c + (round(srtm_utm) >= zmin & round(srtm_utm) <= zmax) * ez # & = "logical and"
} 

for (n in 0:nrec) { # sub-catchments
    # either whole catchment or subcatchment - subsetting record by record, starting from 0
    if (n==0) sst <- shpg else  sst <- shpg[row.names(shpg)==n-1,]
    if (n==0) cname <- "Total" else cname <- atab[n,5]
    mr <- c * rasterize(sst,c,field=-1) #  using mask from polygon
    fname <- paste("catchmnt",n,".tif",sep="")
    writeRaster(mr,fname, datatype="INT1U", overwrite=TRUE)  
    
    for (ez in 1:nez){
        ce <- ((mr!=ez)*254)+1 # set all cells outside of elevation zone to nodata-value
        rname <- paste("ezmask_",n,"_",sprintf("%02d",ez),".tif",sep="")
        # write file, in 8-bit integer 255 becomes "NA"
        writeRaster(ce,rname, datatype="INT1U", overwrite=TRUE) 
        ce <- raster(rname) # by loading again, 255 becomes NA
        cee <- ce*srtm_utm
        ncells <- count(ce,1)
        hmean <- round(cellStats(cee,"mean"))
        # append data to dataframe cdf
        cdf <- rbind(cdf,data.frame(catchment=cname,ez=ez,zmin=ezd[ez,2],
                                    zmax=ezd[ez,3],ncells=ncells,hmean=hmean))
    } # end loop for elevation zones
        
} # end loop for sub-catchments
    
write.table(cdf, "clist.txt", row.names=FALSE)
