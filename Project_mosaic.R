# file numbers for download
d1 <- 1
d2 <- 748

# location of the mosiacing tool:
MRT <- 'c:/MRT/bin/'
workd <- 'F:/test/MODIS/'
TIFs <- 'F:/test/MODIS/TIFs/'


b1 <- list.files(path = "F:/test/MODIS/HDF23", pattern="*.hdf")
b2 <- list.files(path = "F:/test/MODIS/HDF24", pattern="*.hdf")

for(i in d1:d2){

BLOCK1 <- b1[i]
BLOCK2 <- b2[i]

dt <- unlist (strsplit(BLOCK1, ".", fixed = TRUE))

# mosaic the blocks:
mosaicname = file(paste(MRT, "TmpMosaic.prm", sep=""), open="wt")
write(paste("F:/test/MODIS/HDF23/", BLOCK1, sep=""), mosaicname)
write(paste("F:/test/MODIS/HDF24/", BLOCK2, sep=""), mosaicname, append=T)

close(mosaicname)
# generate temporary mosaic:
shell(cmd=paste(MRT, 'mrtmosaic -i ', MRT, 'TmpMosaic.prm -s "1 0 0 0" -o ', workd, 'TmpMosaic.hdf', sep=""))
# 
# resample
filename = file(paste(workd, "param.prm", sep=""), open="wt")
write(paste('INPUT_FILENAME = ', workd, 'TmpMosaic.hdf', sep=""), filename) 
# write(paste('INPUT_FILENAMES = ( ', workd, BLOCK1, ' ', workd, BLOCK2, ' ', workd, BLOCK3, ' ', workd, BLOCK4, ' ', workd, BLOCK5, ' ', workd, BLOCK6, ' ', workd, BLOCK7, ' ', workd, BLOCK8, ' ', workd, BLOCK9, ' )', sep=""), filename)  # unfortunatelly does not work via command line  :(
write('  ', filename, append=TRUE) 
# write('SPECTRAL_SUBSET = ( 1 0 0 0)', filename, append=TRUE)
write('SPECTRAL_SUBSET = ( 1 )', filename, append=TRUE)
write('  ', filename, append=TRUE)
write('SPATIAL_SUBSET_TYPE = OUTPUT_PROJ_COORDS', filename, append=TRUE)
write('  ', filename, append=TRUE)
write('SPATIAL_SUBSET_UL_CORNER = ( 380000.000 4113659.651 )', filename, append=TRUE) 
write('SPATIAL_SUBSET_LR_CORNER = ( 600000.000 3965000.000 )', filename, append=TRUE)
write('  ', filename, append=TRUE)
write(paste('OUTPUT_FILENAME = ', TIFs, dt[1], '.tif', sep=""), filename, append=TRUE)
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
# 
# # Mosaic the images to get the whole area:
shell(cmd=paste(MRT, 'resample -p ', workd, 'param.prm', sep=""))

}
# end of script!