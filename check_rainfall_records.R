# compare FEWS precipitation with observed records
# script 07/12/2011

library (ncdf)
library(sqldf)

# ---- configurable section -----------
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Precipitation/NOAA/')
ncfile <- "data.cdf"
rfile <- "precipitation.txt"
sfile <- "stations.txt"
y1<- 2001
y2<- 2008
fewso <- c(70, 5) # xy origin of ncdf data
dd <- 0.1 # grid size ncdf data
day1 <- as.Date("2001-05-01", format="%Y-%m-%d") # start of FEWS records
dsource <- "Source for remotely sensed rainfall data: http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.SAsia/.RFEv2/.DAILY/est_prcp/data.cdf"
# ---- end of configurable section ----

decade <- function(d) {
    if (d > 20) dc = 3 else if (d>10) dc = 2 else dc = 1
    return(dc)
}

odat.df <- read.table(rfile, header=T)
nstat <- ncol(odat.df)-1
sdat.df <- read.table(sfile, header=T)
odat.df[,1] <- as.Date(odat.df[,1], format="%d/%m/%y")
odat.df <- subset(odat.df, Date >= day1)
maxd <- max(odat.df[,1]) # max date for observed data
dint <- as.integer(maxd-day1+1) # total duration for days to be compared

for (s in 1: nstat) {
    sname <- as.character(sdat.df[s,1])
    if (sname != names(odat.df)[s+1]) {
        print ("data tables do not fit") 
        stop
    }
    
    di <- as.integer((sdat.df[s,2:3]-fewso)/dd) # cordinate counter for ncdf file
    nc <- open.ncdf(ncfile, write=FALSE, readunlim=FALSE, verbose=FALSE) # works fine, reads file structure
    pdnoaa <- get.var.ncdf(nc, varid="est_prcp", start=c(di[1],di[2],1), count=c(1,1,dint), verbose=FALSE, signedbyte=TRUE, forcevarid=NA)
    
    idat.df <- odat.df[,1:2] # date
    idat.df[,2] <- as.integer(substr(as.character(idat.df[,1]),1,4)) # year
    idat.df[,3] <- as.integer(substr(as.character(idat.df[,1]),6,7)) # month
    for (r in 1: dint) idat.df[r,4] <- decade(as.integer(substr(as.character(idat.df[r,1]),9,10))) # decade
    idat.df[,5] <- pdnoaa
    idat.df[,6] <- odat.df[,s+1]
    names(idat.df)[2:6] <- c("year","month","decade","FEWS",sname)
 
    # plot daily data
    ylim <- c(-100,100)
    par(cex=0.75)

    for (y in y1:y2){
        tlim <- c(as.Date(paste(as.character(y),"01-01",sep="-")),(as.Date(paste(as.character(y+1),"01-01",sep="-"))))
        tmain <- paste("Comparison of remote gridded and locally observed precipitation at",sname,"- Year",y)
        plot (x=idat.df[,1], y=idat.df[,5], col="blue", type="h", xlim=tlim, ylim=ylim, xlab="", ylab="Precipitation in mm", main=tmain)
        ticks <- seq(tlim[1], tlim[2], by = "months")
        axis(1, tck=1, at=ticks, col="grey80", lty=3, labels=F)
        legend (x=tlim[2]-75, y=95, c("FEWS",sname), lwd=2, col=c("blue","darkgreen"))
        grid(nx=NA, ny=NULL)
        par(new=T)
        plot (x=idat.df[,1], y=-idat.df[,6], col="darkgreen", type="h", xlim=tlim, ylim=ylim, xlab="", ylab="")
        abline(h=0)
        mtext(dsource, side=1, line=3, outer=FALSE, cex=0.6)
    } # end of loop over years

    write.table(idat.df, paste("fews-",sname,".txt",sep=""), col.names=T, row.names=FALSE, append=F)    # output to file)
    
    # calc summaries
    tsql <- paste("SELECT year, month, Sum(FEWS) FEWS, Sum (",sname,")",sname,"FROM 'idat.df' GROUP BY year, month")
    rdat.m.df <- sqldf(tsql)
    fm <- lm(rdat.m.df[,3]~rdat.m.df[,4]+0)
    c <- summary(fm)$coefficients[1]
    r2 <- summary(fm)$r.squared
    
    tmain <- paste("Correlation between monthly precipitation totals from remote sensing (FEWS) and ",sname)
    plot (rdat.m.df[,4:3], col="blue", main=tmain)
    abline(fm, col = "red")
    grid()
    ctext <- paste("ratio remote/local rainfall =", sprintf("%.3f",c))
    rtext <- paste("correlation coefficient R² =", sprintf("%.3f",r2))
    xm <- as.integer(max(rdat.m.df[,4]))
    mtext(ctext, side=1, line = -19.5, at=xm*0.8, cex=0.8)
    mtext(rtext, side=1, line = -18, at=xm*0.8, cex=0.8)
    outfile.g <- paste("fews-",sname,"-correlation.png",sep="")
    dev.copy(png, width=1000, height=625, units="px", pointsize=18, outfile.g)
    dev.off()

} # end of loop over stations
