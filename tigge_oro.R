# probe and export data from TIGGE grib file
# script version 09/12/2011

library(raster)
library(rgdal)
library(sp)

# change settings here --------------------------------------------------
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Forecasts/Tigge/')
command <- "C:/Tools/ndfd/degrib/bin/degrib"
pntFile <- "Mangla.pnt"
comProbe <- "-P -pntFile"
comUnit <- "-Unit m"
comOutput <- "-out"
myGrib <- "ncep_oro"  # dont use file extension!
# end of code section with editable settings ----------------------------

Fin <- paste(myGrib,"grib",sep=".")
Fout <- paste(myGrib,"prb",sep=".")

degrib <- paste(command,Fin,comProbe,pntFile,comOutput,Fout,comUnit)
shell(cmd=degrib) # execute the command

grb.df <- read.table(Fout, header=T, sep = ",")

oro.df <- read.table(pntFile, header=F, sep = ",")
np <- nrow(oro.df)
alt <- grb.df[1,5:(4+np)]
oro.df[,4] <- as.integer(alt)
names(oro.df) <- c("Code","Y","X","Z")
write.table(oro.df, "tigge_xyz.txt", col.names=T, row.names=F)

#oro_raster <- raster(Fin)
#oro_raster <- readGDAL(Fin)

