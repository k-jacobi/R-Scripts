# download and conversion of MODIS snow files
# script version: 27/06/2011

# load required libraries
library(RCurl)

# file numbers for download
d1 <- 523
d2 <- 540

# basic settings ofremote site (this should not change, except for anonymous user name)
MOD10A2 <- "ftp://n4ftl01u.ecs.nasa.gov/MOST/MOD10A2.005/" # ftp data source for MODIS snow files
MOD10A2a <- "ftp://anonymous:k_jacobi@n4ftl01u.ecs.nasa.gov/MOST/MOD10A2.005/" # log-in to ftp server

# local directories (adjust this to your computer environment)
MRT <- 'C:/java/MRT/bin/'  # MODIS Reprojection Tool exe files
HDFd <- 'C:/AHT.PRO/Pakistan_Modelling/Data/Snowcover/MODIS8/HDF/'   # storage of HDF files
TIFd <- 'C:/AHT.PRO/Pakistan_Modelling/Data/Snowcover/MODIS8/TIFs/'   # storage of TIF files
options(download.file.method="auto") # added from original script

# get the list of directories (code thanks to Barry Rowlingson):
items <- strsplit(getURL(MOD10A2), "\n")[[1]] # get listing from ftp server - this may take some time!

# you got the folders (and files) but the folder names are in the form of a unix directory listing
# get the last word of any lines that start with 'd':
folderLines <- items[substr(items, 1, 1)=='d']
# get the components which make the directory names
dirs <- unlist(lapply(strsplit(folderLines, " "), function(x){x[length(x)]})) 
dates <- data.frame(dirname=unlist(strsplit(dirs, "\r"))) 

# get the list of files:
dates$BLOCK1 <- rep(NA, length(dates$dirname))

# start loop over directories (each directory contains files for a single day)
for(a in d1:d2){ 

  getlist <- strsplit(getURL(paste(MOD10A2, dates$dirname[[a]], "/", sep=""), .opts=curlOptions(ftplistonly=TRUE)), "\r\n")[[1]]
  
  # set filter for hdf file (but includes xml also!)
  # ftp://n4ftl01u.ecs.nasa.gov/MOST/MOD10A2.005/2000.02.26/MOD10A2.A2000057.h10v07.005.2006269090421.hdf.xml
  BLOCK1 <- getlist[grep(getlist, pattern="MOD10A2.*.h24v05.*.hdf")[1]]
  if (is.na(BLOCK1)) next # if file does not exist, don't try to download
  # remove "." from the file name:
  dirname1 <- sub(sub(pattern="\\.", replacement="-", dates$dirname[[a]]), pattern="\\.", replacement="-", dates$dirname[[a]])
  
  # this downloads the individual file
  download.file(paste(MOD10A2a, dates$dirname[[a]], "/", BLOCK1,sep=""), destfile=paste(HDFd, dirname1, ".hdf", sep=""), mode='wb', method='internal', quiet=T, cacheOK=FALSE)
  
  # generate MRT parameter file
  filename = file(paste(HDFd, "mangla.prm", sep=""), open="wt") 
  write(paste('INPUT_FILENAME = ', HDFd, dirname1, ".hdf", sep=""), filename) 
  write('SPECTRAL_SUBSET = ( 1 0 0 0 )', filename, append=TRUE) 
  write('  ', filename, append=TRUE) 
  write('SPATIAL_SUBSET_TYPE = OUTPUT_PROJ_COORDS', filename, append=TRUE) 
  write('  ', filename, append=TRUE)
  write('SPATIAL_SUBSET_UL_CORNER = ( 320000.0 3900000.0 )', filename, append=TRUE) 
  write('SPATIAL_SUBSET_LR_CORNER = ( 570000.0 3660000.0 )', filename, append=TRUE)
  write('  ', filename, append=TRUE) 
  write(paste('OUTPUT_FILENAME = ', TIFd, dirname1, '.tif', sep=""), filename, append=TRUE) 
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
  shell(cmd=paste(MRT, 'resample -p ', HDFd, 'mangla.prm', sep=""))
   
} # end of loop for daily files
