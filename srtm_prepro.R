# SRTM file preparation
# 02/08/2011

library(raster)
# setwd('C:/AHT.PRO/Pakistan_Modelling/GIS_factory/SRTM/') # adjust this to your own directory structure
setwd('C:/AHT.PRO/TZ_WPM/SRTM/') # adjust this to your own directory structure
rname <- "srtm_ll.tif"

# for Shigar catchment (north of Mangla)
# r1 <- raster("srtm_51_05.tif")
# r2 <- raster("srtm_52_05.tif")

# For Tanzania
r1 <- raster("srtm_42_13.tif")
r2 <- raster("srtm_42_14.tif")
r3 <- raster("srtm_42_15.tif")
r4 <- raster("srtm_43_13.tif")
r5 <- raster("srtm_43_14.tif")
r6 <- raster("srtm_43_15.tif")
r7 <- raster("srtm_44_13.tif")
r8 <- raster("srtm_44_14.tif")
r9 <- raster("srtm_44_15.tif")
r10 <- raster("srtm_45_13.tif")
r11 <- raster("srtm_45_15.tif")


#r1c <- crop(r1, extent(74.5,75,35,36.5)) # crop to area of interest - xmin, xmax, ymin, ymax
#r2c <- crop(r2, extent(75,77,35,36.5)) # crop to area of interest

m <- mosaic(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11, fun=mean) # mosaic the 2 (or more) rasters
mc <- crop(m, extent(29,41,-12,-1))

writeRaster(mc,rname, datatype="INT2S", overwrite=TRUE) # INT2S: values -32,767 to 32,767
