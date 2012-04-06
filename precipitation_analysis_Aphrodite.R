# Precipitation analysis of daily rainfall predictions (NOAA)
# script version: 12/07/2011 - cloudnumbers

library(rgdal)
library(raster)
options(warn=-1)

cat("Rainfall analysis - start routine:", format(Sys.time(),"%X"), "\n")

# set working directory = location of TIF files
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Precipitation/Aphrodite/') 
#setwd('Precipitation/')

# load complete file list of reprojected rainfall TIFs into a R character vector
glistf <- list.files(pattern="....-..-.._m.tif") 

# setting filenames, assigning files to raster layer objects
glistm <- list.files(pattern="ezmask_2_") # get list of masks for elevation zones
e <- stack(glistm) # add all masks to a raster stack

# please adjust the following numbers for each run
minf <- 1 # first file to read
maxf <- 365 # last file to be read

for (i in minf:maxf){
  day <- paste(substr(glistf[i],1,10)) 
  r <- raster (glistf[i]) # remember: integer raster with mm*10, model input in cm
  for (ez in 2:10){  # range of ez for Kunhar
    # make new vector
    dlist <- c(day, ez, 0, 0, 0)
    c <- subset(e,ez) * r  # make subset of raster using mask
    # get statistics
    dlist[3] <- round(cellStats(c, 'mean'))/100  # put mean rainfall into vector
    dlist[4] <- cellStats(c, 'min')/100  # put mean rainfall into vector
    dlist[5] <- cellStats(c, 'max')/100  # put mean rainfall into vector
    write(dlist, file = "rainfall_2A.txt", ncolumns=5, append = TRUE,  sep = " ") # write vector contents to disk
  } # end loop on levation zones
} # end of loop for daily TIFs

cat("end routine:", format(Sys.time(),"%X"), "\n")
options(warn=0)
