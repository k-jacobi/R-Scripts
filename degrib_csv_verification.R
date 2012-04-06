# Check extents problem!
# read data from degrib-export
# strage: either 72/144 (=cells) or 73/145 (=outer grid points) but not 73/144
csvfile <- 'tmp_t12.csv'
csv.df <- read.table(csvfile, header=T, sep=",")
csv.df <- subset(csv.df, select = 3:5)
names(csv.df) <- c('y', 'x', 'z')
csv.m <- as.matrix(csv.df[,2:1]) # convert df to matrix and swap sequence of columns
tmp_test.spdf <- SpatialPixelsDataFrame(points=csv.m, data=csv.df)

# Data summary:
#        y             x                 z          
#  Min.   :-90   Min.   :-177.50   Min.   :-44.000  
#  1st Qu.:-45   1st Qu.: -88.12   1st Qu.:-11.000  
#  Median :  0   Median :   1.25   Median :  8.000  
#  Mean   :  0   Mean   :   1.25   Mean   :  4.867  
#  3rd Qu.: 45   3rd Qu.:  90.62   3rd Qu.: 23.000  
#  Max.   : 90   Max.   : 180.00   Max.   : 35.000  

gridded(tmp_test.spdf) <- TRUE # make spatial grid
tmp_test.raster <- raster(tmp_test.spdf, layer=3) # need to tell where z-value is found

writeRaster(tmp_test.raster, 'tmp_test.tif', overwrite=TRUE, datatype="INT2S")