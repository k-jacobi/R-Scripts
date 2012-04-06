# Snowcover - fill gaps in all time series
# script version: 2012-01-27

library(sqldf)
library(timeSeries)

# change settings here --------------------------------------------------
input <- "snowcover_0.txt"

setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Snowcover/MODIS/TIFs/') # set working directory

# end of code section with editable settings ----------------------------

cc <- c("character","integer","integer","integer") # type specification of columns in file 'snowcover.txt'
se <- read.table(input, header=TRUE, colClasses=cc)  # load data into table
names(se) <- c("date","ez","nosnow","snow")
ez1 <- min(se[,2]) # first elevation zone
ez2 <- max(se[,2]) # last elevation zone

output <- paste(substr(input,1,11),"_interp",".txt",sep="")
write(c("date","ez","snow_ratio_i","snow_ratio_o"), file=output, ncolumns=4, append = FALSE,  sep = " ")
      
for (ez in ez1:ez2) { 
	
	tsql <- paste("SELECT date, AVG(1000*snow/(nosnow+snow))/1000 AS r_snow, ez FROM se WHERE ez=",ez," GROUP BY date ORDER BY date", sep="")
	
    sdf <- sqldf(tsql) 
	sdfd <- sdf
	sdfd[,1] <- as.Date(gsub("_", "-",sdfd[,1]))

    d01 <- sdfd[1,1] # first date
    d02 <- sdfd[dim(sdfd[1])[1],1] # last date
	
	# insert missing dates dates
	seqq <- seq(d01, d02, by=1) 
	seqdf <- as.data.frame(seqq)
	tsql <- "SELECT seqdf.seqq, sdfd.r_snow FROM seqdf LEFT OUTER JOIN sdfd ON seqdf.seqq = sdfd.date"
	sdff <- sqldf(tsql)
	
	# interpolate missing values
	sdff[,1] <- as.POSIXct(sdff[,1])
	sdffts <- as.timeSeries(sdff)
	sinterpol <- interpNA(sdffts, method = "linear")  # interpolation
	
	# convert timeSeries back to dataframe
	interpoldf <- as.data.frame(sinterpol)
    interpoldf[,2] <- ez
    interpoldf[,3] <- interpoldf[,1]
    interpoldf[,1] <- as.Date(rownames(interpoldf))
    interpoldf[,4] <- sdff[,2]

    names(interpoldf) <- c("date","ez","r_snow_i","r_snow_o")

	write.table(interpoldf, output, col.names=FALSE, row.names=FALSE, append=TRUE)    # output to file
	
} # end of loop over elevationzones ez

options(warn=0)