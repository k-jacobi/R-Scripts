# Precipitation analysis of daily rainfall predictions (NOAA)
# script version: 2012-01-16

library(rgdal)
library(raster)

# change settings here --------------------------------------------------
options(warn=-1)
# set working directory = location of TIF files
#setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Precipitation/Aphrodite/') 
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Precipitation/NOAA/')

# load complete file list of reprojected rainfall TIFs into a R character vector
glistf <- list.files(pattern="....-..-.._m.tif") 

# setting filenames, assigning files to raster layer objects
glistm <- list.files(pattern="ezmask") # get list of masks for elevation zones
# end of code section with editable settings ----------------------------

# interactive entry of file range to process
nf <- length(glistf)
cat ("\n")
cat("Rainfall analysis", "\n")
cat (nf, "tif files with daily rainfall grids in current directory", "\n")
minf <- readline("First file number to read? :")
maxf <- readline("Last file number to read? :")

cat("Start routine:", format(Sys.time(),"%X"), "\n")
e <- stack(glistm) # add all masks to a raster stack

for (i in minf:maxf){
  day <- paste(substr(glistf[i],1,10)) 
  r <- raster (glistf[i]) # remember: integer raster with mm*10, model input in cm
  for (ez in 1:11){  # presently ez 12 is outside of rainfall raster
    # make new vector
    dlist <- c(day, ez, 0, 0, 0)
    c <- subset(e,ez) * r  # make subset of raster using mask
    # get statistics
    dlist[3] <- round(cellStats(c, 'mean'))/100  # put mean rainfall into vector
    dlist[4] <- cellStats(c, 'min')/100  # put mean rainfall into vector
    dlist[5] <- cellStats(c, 'max')/100  # put mean rainfall into vector
    write(dlist, file = "rainfall.txt", ncolumns=5, append = TRUE,  sep = " ") # write vector contents to disk
  } # end loop on levation zones
} # end of loop for daily TIFs

cat("end routine:", format(Sys.time(),"%X"), "\n")
options(warn=0)
