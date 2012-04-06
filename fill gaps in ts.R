library(timeSeries)
library(sqldf)

l <- dim(sdf)[1]
x <- 1:l
sdfc <- sdf
sdfc[,1] <- as.Date(sdf[,1])

seq <- seq(from=as.Date("2010-03-01"), to=as.Date("2010-08-31"), by=1) 
seqdf <- as.data.frame(seq)
tsql <- "SELECT seqdf.seq, sdfc.r_snow FROM seqdf LEFT OUTER JOIN sdfc ON seqdf.seq = sdfc.date"
gfilled <- sqldf(tsql)    

#dlima <- as.Date(tlima)

gfilled[,1] <- as.POSIXct(gfilled[,1])
gfillests <- as.timeSeries(gfilled)
sinterpol <- interpNA(gfillests, method = "linear")  # interpolation
#obsdf <- as.data.frame(gfillests)
#obsdf[,2] < obsdf
#indf <- as.data.frame(sinterpol)
#indf[,1] < as.Date(indf[,1])

plot(sinterpol, xlim=tlima, ylim=ylm, col="red", xaxt = "n", cex.axis=0.8, cex.main=0.9, type="l", las=1, main=mtitle, xlab="", ylab=pylab, yaxs="i", xaxs="i")
par(new=T)
plot(gfillests, xlim=tlima, ylim=ylm, col="blue", xaxt = "n", cex.axis=0.8, cex.main=0.9, type="l",	las=1, main=mtitle, xlab="", ylab=pylab, yaxs="i", xaxs="i")


