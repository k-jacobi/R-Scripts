# conversion of MODIS snow files from available HDF
# script version: 01/08/2011

# file numbers for download
d1 <- 1800
d2 <- 1810

# local directories (adjust this to your computer environment)
MRT <- 'C:/java/MRT41/bin/'  # MODIS Reprojection Tool exe files # reactivate for Windows
# MRT <- '/Users/klausjacobi/Downloads/MRT_download_Mactel/MRT/bin/' # for Mac

setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Snowcover/MODIS/HDF/')
HDFd <- 'C:/AHT.PRO/Pakistan_Modelling/Data/Snowcover/MODIS/HDF/'   # storage of HDF files
TIFd <- 'C:/AHT.PRO/Pakistan_Modelling/Data/Snowcover/MODIS/TIFs/'   # storage of TIF files
# setwd('/Users/klausjacobi/Documents/Geo_Data/SRM_prepro/MODIS/HDF/') # on Mac OSX
# HDFd <- '/Users/klausjacobi/Documents/Geo_Data/SRM_prepro/MODIS/HDF/' # on Mac OSX
# TIFd <- '/Users/klausjacobi/Documents/Geo_Data/SRM_prepro/MODIS/TIFs/' # on Mac OSX

# options(download.file.method="auto") # added from original script

# get the list of files
glistf <- list.files(pattern="*.hdf")

# start loop over directories (each directory contains files for a single day)
for(a in d1:d2){ 
  
  # generate MRT parameter file
  filename = file(paste(HDFd, "shigar.prm", sep=""), open="wt") 
  write(paste('INPUT_FILENAME = ', HDFd, glistf[a], sep=""), filename) 
  write('SPECTRAL_SUBSET = ( 1 0 0 0 )', filename, append=TRUE) 
  write('  ', filename, append=TRUE) 
  write('SPATIAL_SUBSET_TYPE = OUTPUT_PROJ_COORDS', filename, append=TRUE) 
  write('  ', filename, append=TRUE)
  write('SPATIAL_SUBSET_UL_CORNER = ( 468000.0 4010000.0 )', filename, append=TRUE) 
  write('SPATIAL_SUBSET_LR_CORNER = ( 675000.0 3900000.0 )', filename, append=TRUE)
  write('  ', filename, append=TRUE) 
  write(paste('OUTPUT_FILENAME = ', TIFd, substr(glistf[a],1,10), '.tif', sep=""), filename, append=TRUE) 
  write('  ', filename, append=TRUE) 
  write('RESAMPLING_TYPE = NEAREST_NEIGHBOR', filename, append=TRUE) 
  write('  ', filename, append=TRUE) 
  write('OUTPUT_PROJECTION_TYPE = UTM', filename, append=TRUE) 
  write('  ', filename, append=TRUE) 
  write('OUTPUT_PROJECTION_PARAMETERS = ( ', filename, append=TRUE) 
  write(' 0.0 0.0 0.0', filename, append=TRUE) 
  write(' 0.0 0.0 0.0', filename, append=TRUE) 
  write(' 0.0 0.0 0.0', filename, append=TRUE)
  write(' 0.0 0.0 0.0', filename, append=TRUE) 
  write(' 0.0 0.0 0.0 )', filename, append=TRUE) 
  write('  ', filename, append=TRUE) 
  write('DATUM = WGS84', filename, append=TRUE) 
  write('  ', filename, append=TRUE) 
  write('UTM_ZONE = 43', filename, append=TRUE) 
  write('  ', filename, append=TRUE)
  write('OUTPUT_PIXEL_SIZE = 500', filename, append=TRUE) 
  write('  ', filename, append=TRUE) 
  close(filename)
  
  # apply MRT (extracting layer, reprojection, clipping to catchment) by calling the parameter file
  
shell(cmd=paste(MRT, 'resample -p ', HDFd, 'shigar.prm', sep="")) # re-activate for Windows
# system(paste(MRT, "resample -p ", HDFd, "shigar.prm", sep="")) # for Mac

   
} # end of loop for daily files
