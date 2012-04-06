# monthly summaries for NCEP CFS predictions
# Script version 08/12/2011

library(sqldf)

# ---- configurable section -----------
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Forecasts/CFS/dl/')
varid <- "tmp2m" # either tmp2m or prate
date1 <- as.Date("2011-11-23")
date2 <- as.Date("2011-12-02")
#date2 <- as.Date("2011-11-30")
dur <- 280 # duration of forecast-analysis in days
mystats <- 4 #  to extract data from stored d.df. 4 = average, 5 = moving average
stext <- ifelse(mystats==4, "Daily Values","9-Days Moving Averages")

# ---- end of configurable section ----

decade <- function(d) {
    if (d > 20) dc = 3 else if (d>10) dc = 2 else dc = 1
    return(dc)
}

# Read-in data and calculate month and decade numbers
idate <- paste(gsub("-","",as.character(as.Date(date2,origin="1970-01-01"))),"00",sep="")
tfile <- paste(varid, idate, mystats, "txt",sep=".")
d.df <- read.table(tfile, header=T) # load data from disk
nc <- ncol(d.df)
for (i in 2:nc) names(d.df)[i] <- sub(".","_",names(d.df)[i],fixed=T)
sdf <- d.df[,1:4]
sdf[,2] <- as.integer(substr(as.character(sdf[,1]),1,4)) # year
sdf[,3] <- as.integer(substr(as.character(sdf[,1]),6,7)) # month
sdf[,5] <- as.integer(substr(as.character(sdf[,1]),9,10)) # day
names(sdf)[1:5] <- c("date","year","month", "decade","day")
sdf<- cbind(sdf,d.df[,2:nc])
sdf <-na.omit(sdf)
for (r in 1: nrow(sdf)) sdf[r,4] <- decade(sdf[r,5]) # decade


# calculate monthly statistics with SQL
tsql <- "SELECT year, month,"
for (i in 6:nc+4) tsql <- paste(tsql, " AVG(", names(sdf)[i],"),", sep="")
tsql <- paste(substr(tsql,1, nchar(tsql)-1), "FROM sdf WHERE year IS NOT NULL GROUP BY year, month")
sm.df <- sqldf(tsql)

# calculate decadal statistics with SQL
tsql <- "SELECT year, month, decade, "
for (i in 6:nc+4) tsql <- paste(tsql, " AVG(", names(sdf)[i],"),", sep="")
tsql <- paste(substr(tsql,1, nchar(tsql)-1), "FROM sdf WHERE year IS NOT NULL GROUP BY year, month, decade")
dm.df <- sqldf(tsql)

# plot results
par(cex=0.8)
sm.df <- as.data.frame(sm.df[-1,]) # remove first month
x <- seq(1:nrow(sm.df))
nc <- ncol(sm.df)
#ylim <- c(min(sm),max(sm))
ylim <- c(-10,20)
tmain <- "Monthly Temperature Means from NCEP CFS Forecasts"
xlab <- "next months, from December 2011 to August 2012"
plot (x=x, y=sm.df[,3], col="green", ylab="Mean Temperature in °C", xlab=xlab, ylim=ylim, main=tmain)
for (n in 4:nc){
    par(new=T)
    plot (x=x, y=sm.df[,n], col="green", ylab="", xlab="", ylim=ylim)
}
grid()

# statistical analysis
ql <- c(0.2, 0.5, 0.8)
sms.df <- sm.df[,1:2]
for (n in 3:5) sms.df[,n] <-0
nr <- nrow(sm.df)
for (n in 1:nr) {
    sms <- as.numeric(sm.df[n,3:nc])
    sms.df[n,3:5] <- as.numeric(quantile(sms, ql))
}

col <- c("blue","darkgreen","red")
lwd <- c(1,2,1)
for (q in 3:5){
    par(new=T)
    plot (x=x, y=sms.df[,q], type="l", col=col[q-2], lwd=lwd[q-2], ylab="", xlab="", ylim=ylim)
}

par(xpd=T) # for legend outside of plot area
    legend (title="quantile",x=max(x)+0.5, y=ylim[2], rev(ql),lwd=lwd, col=rev(col))
par(xpd=F)
