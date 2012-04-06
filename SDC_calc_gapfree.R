# SDC processing - plotting observed data and smoothing curves
# querying of melt-seasons per ez and year
# script version: 2012-01-27

library(sqldf)
library (tseries)
library(timeSeries)

# change settings here --------------------------------------------------
options(warn=-1)

y1 <- 2005 # first year
y2 <- 2012 # last year
ez1 <- 4 # first elevation zone
ez2 <- 10 # last elevation zone
dfs <- 7 # degrees of freedom for spline smoothing

setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Snowcover/MODIS/TIFs/') # set working directory
input.f <- "snowcover_0.txt"
output <- "splines_0.txt"
a_seasons <- "meltseasons_0.txt"
classes.f <- "ez_def.txt"
tf.f <- "timeframes.txt"
titlo <- "   Observed/Smoothed Snow Depletion Curves after Cloud Cover Elimination, Mangla Catchment, Zone "
# end of code section with editable settings ----------------------------

cc <- c("character","integer","numeric","numeric") # type specification of columns in file 'snowcover*.txt'
se <- read.table(input.f, header=TRUE, colClasses=cc)  # load data into table
names(se) <- c("date","ez","nosnow","snow")
classes <- read.table(classes.f, header=TRUE)
write(c("EZ","year","start_melt","start_r", "end_melt","end_r"), file=a_seasons, ncolumns=6, append = FALSE,  sep = " ")
write(c("date","SDC","EZ"), file=output, ncolumns=3, append = FALSE,  sep = " ")

# read individual chart-periods for elevation zones into dataframe
cc <- c("character", "character", "character","character","character","numeric")
tf <- read.table(tf.f, header=TRUE, sep=",", row.names="Zone", colClasses=cc) 
nz <- nrow(tf) # number of rows in dataset tf

# valid for all charts
ylm <- c(0,1) # parameters for ylim in plots
pylab <- "snowcover ratio"
# set colors
pcolors <- c("blue", "darkgrey", "green", "red", "darkred", "violet", "cornflowerblue", "darkgreen", "brown", "cyan4", "chartreuse4", "darkmagenta") 
tsource <- 'Datasource: MODIS "MOD10A1" daily grids and own calculations'
ds <- "%Y_%m_%d" # format for converting text to POSIXct (date)
ds0 <- "%Y-%m-%d"
par(mar=c(4.5,4,4,9)+0.1) # resizes plot box window
par(cex=0.8) # relates to fonts of all graphics
      
for (ez in ez1:ez2) { 
    
    i <- 1
    for (a in y1:y2) {
        d01s <- paste(a, substr (tf[ez,1], 5, 10), sep="")
        d02s <- paste(a, substr (tf[ez,2], 5, 10), sep="")
                    
        tsql <- paste("SELECT date, AVG(snow/(nosnow+snow)) AS r_snow FROM se GROUP BY date, ez HAVING (ez=", ez, " AND date >'", d01s, "' AND date <'", d02s, "') ORDER BY date", sep="")
        sdf <- sqldf(tsql) 
        if (dim(sdf)[1]==0) next # exit if no data available

        d01s <- gsub("_", "-", d01s)
        d02s <- gsub("_", "-", d02s)
        
        sdfd <- sdf
        sdfd[,1] <- gsub("_", "-",sdfd[,1])
        sdfd[,2] <- round (sdfd[,2]*1000)/1000
        
        # calculate start, end of season
        
        tsql <- paste("SELECT max(date), max(r_snow) FROM sdfd GROUP BY r_snow ORDER BY r_snow DESC", sep="")  
        smax <- (sqldf(tsql))
        tsql <- paste("SELECT min(date), min(r_snow) FROM sdfd GROUP BY r_snow ORDER BY r_snow ASC", sep="")     
        smin<- sqldf(tsql)
        
        meltseason <- c(ez, a, smax[1,1], smax[1,2], smin[1,1], smin[1,2])
        write(meltseason, a_seasons, ncolumns =6, append=TRUE)    # output to file
                
        # insert missing dates dates
        sdfd[,1] <- as.Date(sdfd[,1])
        seq <- seq(from=as.Date(d01s), to=as.Date(d02s), by=1) 
        seqdf <- as.data.frame(seq)
        tsql <- "SELECT seqdf.seq, sdfd.r_snow FROM seqdf LEFT OUTER JOIN sdfd ON seqdf.seq = sdfd.date"
        sdff <- sqldf(tsql)
        
        # interpolate missing values
        sdff[,1] <- as.POSIXct(sdff[,1])
        sdffts <- as.timeSeries(sdff)
        sinterpol <- interpNA(sdffts, method = "linear")  # interpolation
        
        # convert timeSeries back to dataframe
        sinterpoldf <- as.data.frame(sinterpol)
        sinterpoldf[,2] <- sinterpoldf
        sinterpoldf[,1] <- as.Date(rownames(sinterpoldf))
        
        # remove outer missing values that could not be interpolated
        sinterpolc <- removeNA(sinterpoldf) 
        sinterpolc[,1] <- as.POSIXct(sinterpolc[,1])
        sinterpol_irts <- as.irts(sinterpolc) # irts makes nicer plots than timeSeries
        
        # calc intervals
        d1 <- as.POSIXct(paste(a, substr (tf[ez,3], 5, 10), sep=""), format=ds, tz = "GMT")
        d2 <- as.POSIXct(paste(a, substr (tf[ez,4], 5, 10), sep=""), format=ds, tz = "GMT")
        ix <-seq(from=d1, to=d2, length.out=tf[ez,5]) # calc intervals for vertical grid
        
        d01 <- as.POSIXct(d01s, format=ds0, tz = "GMT") 
        d02 <- as.POSIXct(d02s, format=ds0, tz = "GMT") # tz="" may give NA for certain dates
        tlima <-c(d01,d02) # time frame for individual year
        
        # calculate spline
        fitss <- smooth.spline(sinterpolc, df=dfs) #works now!
        sdfs <- sinterpolc
        sdfs[,1] <- as.POSIXct(sdfs[,1])
        sdfs[2] <- as.vector(fitss [2]) # list of fitted data
        sdfs[,2] <- round(sdfs[,2]*1000)/1000
        sdfs[,2] <- pmin(sdfs[,2],1) # clips values exceeding 1
        sdfs[,2] <- pmax(sdfs[,2],0) # clips values below 0
        sdfs_irts <- as.irts(sdfs)  # bring to irts for plotting
        sdfs[,1] <- as.Date(sdfs[,1]) # bring to date for text output
        sdfs[3] <- ez
        write.table(sdfs, output, col.names=FALSE, row.names=FALSE, append=TRUE)    # output to file
        
        par(xpd=F) # restrict output to plot area
    	if (a == y1) {
		    mtitle <- paste(titlo, ez, sep="")  # set individual chart title
    
			# plot first time series
    		plot(sinterpol_irts, xlim=tlima, ylim=ylm, col=pcolors[i], xaxt = "n",las=1, main=mtitle, xlab="", ylab=pylab, yaxs="i", xaxs="i")
      
            # add tick marks on x-axis
			axis.POSIXct(side=1, at=seq(d01, d02, by="month"), format="%d.%b") 
            # add horizontal grid
			grid(nx=NA, ny=NULL, col="gray", lty="dotted", lwd=1)
			# add vertical grid
			abline(a=NULL, b=0, h=NULL, v=ix, reg=NULL, col="gray", lty="dotted", lwd=1) 

			} # first year
		else {
			par(new=T) # this adds another curve to the existing plot!
			plot(sinterpol_irts, xlim=tlima, ylim=ylm, col=pcolors[i], main="", yaxt="n", xaxt="n", 
				xlab="", ylab="", yaxs="i", xaxs="i")

		} # further years
        
    	# plot spline
		par(new=T) # continue on existing plot
       	plot(sdfs_irts, xlim=tlima, ylim=ylm, col=pcolors[i], xaxt="n", lwd=2, las=1, main="", xlab="", ylab="", yaxs="i", xaxs="i")

        i <- i+1 # counter for color vector
   } # end of loop over years a
   par(xpd=T) # permit legend outside of plot area    
   legend (title="year",tlima[2]+240000, 1, legend=y1:y2, lty=1, lwd=2, col=pcolors[1:i])
   mtext(tsource, side=1, line=3, outer=FALSE, cex=0.75)
    
   # output of charts to file
   dev.copy(png,width=800,height=500, paste("SDC_zone_",sprintf("%02d",ez),".png",sep=""))
   dev.off()

} # end of loop over elevationzones ez

options(warn=0)
