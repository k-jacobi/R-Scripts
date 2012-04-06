# Script to delete HDF tiles for which 
# corresponding other tile of the same day is not present

############################################
## CAUTION:- Only Run This script When You##
##### are sure about DELETING the files#####
############################################

# Get list of files in HDF23 folder
b1 <- list.files(path = "F:/test/MODIS/HDF23", pattern="*.hdf")
# Get list of files in HDF24 folder
b2 <- list.files(path = "F:/test/MODIS/HDF24", pattern="*.hdf")

x1 <- setdiff(b1,b2)   # x1= List of all h23 tiles for which h24 tile is not present
x2 <- setdiff(b2,b1)   # x2= List of all h24 tiles for which h23 tile is not present
y1 <- length(x1)       # y1= Total number of items of x1
y2 <- length(x2)       # y2= Total number of items of x2

# Process to delete uncommon files
if(y1==0L && y2==0L){
  print("No uncommon file are there to be deleted")
} else {
  # Remove x1 files
  for (i in 1:y1){
  unlink(paste("F:/test/MODIS/HDF23", '/', x1[i], sep=""))
  }
  # Remove x2 files
  for (j in 1:y2){
  unlink(paste("F:/test/MODIS/HDF24", '/', x2[j], sep=""))
  }
}
  # End of Script