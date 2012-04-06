# analysis of input data:
# show daily precipitation per ez
# script update 18/09/2011

library (tseries)

setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Precipitation/NOAA/Mangla/')
# setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Precipitation/Aphrodite/Mangla/')

input <- "rainfall_0.txt" # Mangla

cc <- c("character","integer","numeric","numeric","numeric") # type specification of columns in file 'rainfall*.txt'
ptab <- read.table(input, header=TRUE, colClasses=cc)  # load data into table

d1 <- '2007-06-01'
d2 <- '2007-07-31'
tlima <- c(as.POSIXct(d1),as.POSIXct(d2))
ylm <- c(0, 5)

# set colors
pcolors <- c("blue", "darkgrey", "darkgreen", "red", "darkred", "violet", "cornflowerblue", "green", "brown", "cyan4", "chartreuse4", "darkmagenta")
mtitle <- "daily precipitation per elevation zone"
pylab <- "precipitation in cm"

for (ez in 2:10){
    ezc <- as.character(ez)
    ptabt <- subset(ptab, date>=d1 & date<=d2 & ez==ezc, select = c(date, mean))
    ptabt[,1] <- as.POSIXct(ptabt[,1])
    p_irts <- as.irts(ptabt)

    if (ez == 2) {
        plot(p_irts, xlim=tlima, ylim=ylm, col=pcolors[ez], main=mtitle, ylab=pylab, xlab = "")
    } # first ez
		else {
            par(new=T) # this adds another curve to the existing plot!
            plot(p_irts, xlim=tlima, ylim=ylm, col=pcolors[ez],main="", ylab="", xlab = "")
        }
     mtext(ez, side=1, adj=ez/12, line=3, outer=FALSE, cex=0.75, col=pcolors[ez])
} # end of ez
