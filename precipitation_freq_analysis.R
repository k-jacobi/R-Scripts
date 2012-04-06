# get statistics of precipitation from FEWS (per ez and month)
# script version: 18/09/2011

library(sqldf)
options(warn=-1)

setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Precipitation/NOAA/') # set working directory

prec.file <- "rainfall_00.txt" # values are in cm!
dst<- read.table(prec.file, header=TRUE, as.is=T) # class definition with total area

# quantile list
ql <- c(0.05, 0.2, 0.5, 0.8, 0.95)
quant <- quantile(dst[,3],probs=ql,names=T, type=6, na.rm=T)
qdt <- as.table(quant)
qdf <- as.data.frame(qdt)
qll <- as.vector(qdf[,1])
qdf <- data.frame(ez=numeric(0), month=numeric(0), Q_1=numeric(0), Q_2=numeric(0), Q_33=numeric(0), Q_4=numeric(0), Q_5=numeric(0), stringsAsFactors =F)
names(qdf)[3:7] <- qll

# calculation of probabilities
for (ez in 1:11){
    tsql <- paste("SELECT substr(date,1,4) year, substr(date,6,2) month, sum(mean) total FROM dst WHERE dst.ez =", as.character(ez)," GROUP BY substr(date,1,4), substr(date,6,2)")
    mst <- sqldf(tsql)
    mst[,1]<- as.integer(mst[,1])
    mst[,2]<- as.integer(mst[,2])
    
    # calculate cumulative frequencies for each month
    for (m in 1:12) {
        # monthly subset
        mstm <- subset(mst, mst[,2]==m)
        #mstm.v <- sort(mstm[,3]) # sort by column 3
        #mstm.vc <- transform(mstm.v, Cfreq=0)
        #mstm.vc[,2] <- as.numeric(rownames(mstm.vc))/(nrow(mstm.vc)+1)
        quant <- quantile(mstm[,3],probs=ql,names=T, type=6)
        qdf[(ez-1)*12+m,] <- c(ez,m,quant)
    } # end of month
} # end of ez
write.table(qdf,"precipitation_monthly_freq.txt",col.names=T,row.names=F)

# plotting of probabilities per month to compare different ez
tsource <- 'Data source: "FEWS/NOAA daily rainfall predictions" 2001 - 2011 and own calculations'
ylab <- "monthly precipitation in cm"
par(cex=0.8) # relates to fonts of all graphics elements
par(xpd=T) # for legend outside of plot area

par(mar=c(4.5,4,4,8)+0.1) # resizes plot box window
# mar: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot. The default is c(5, 4, 4, 2) + 0.1.

# set colors
pcolors <- rev(c("blue", "cornflowerblue",  "green", "red", "magenta"))
xlabels <- substr(month.name,1,3)
ptab <- data.frame(month =1:12, prec = numeric(12))
par(new=F)

for (e in 1:11){
    main <- paste("Comparison of precipitation per elevation zone and different months, elevation zone =", as.character(e))
    pmax <- 50
    ylim <- c(0,pmax) # range of precipitation total/month in cm
    etab <- subset(qdf, qdf[,1]==e)
    for (q in 5:1) {
        ptab[,2] <- etab[,2+q]
        if (q == 5) plot(ptab,type="b",ylim=ylim,main=main,ylab=ylab,xlab="",lwd=2,col=pcolors[q],labels=F)
            else {
                par(new=T) # this adds another curve to the existing plot!
                if (q==3) lwd=4 else lwd=2
                plot(ptab,type="b",ylim=ylim,ylab="",xlab="",lwd=lwd,col=pcolors[q],labels=F)
            }
    } # end of q
    axis(1, at=(1:12), tck=1, col="grey40", lty=3, labels=xlabels)
    axis(2, tck=1, col="grey40", lty=3, labels=T)
    legend (title="cum. P",12.7, pmax, qll[5:1], lty=1, lwd=c(2,2,4,2,2), col=pcolors[5:1])
    mtext(tsource, side=1, line=3, outer=FALSE, cex=0.75)
    # output of charts to file
    dev.copy(png, width=800, height=500, paste("M_precipitation_ez", e, ".png", sep=""))
    dev.off()
} # end of zones (1 chart per zone)

pcolors <- terrain.colors(12)
#pcolors <- c(rgb(255,255,125,1,"",1),rgb(255,170,127),rgb(213,142,106),rgb(170,85,0),rgb(143,69,0),rgb(108,49,7),rgb(118,0,0))

for (q in 1:5){
    main <- paste("Comparison of monthly precipitation for different elevation zones, cum. probability =", qll[q])
    pmax <- max(qdf[,2+q])
    ylim <- c(0,pmax) # range of precipitation total/month in cm
    for (e in 1:11) {
        ptab <- subset(qdf, qdf[,1]==e)
        if (e ==1) plot(ptab[,2+q],type="o",ylim=ylim,main=main,ylab=ylab,xlab="",lwd=2,col=pcolors[e],labels=F)
            else {
                par(new=T) # this adds another curve to the existing plot!
                plot(ptab[,2+q],type="o", ylim=ylim,ylab="",xlab="",lwd=2,col=pcolors[e],labels=F)
            }
    } # end of months
    axis(1, at=(1:12), tck=1, col="grey40", lty=3, labels=xlabels)
    axis(2, tck=1, col="grey40", lty=3, labels=T)
    legend (title="ez",12.7, pmax, c(1:11), lty=1, lwd=2, col=pcolors[1:11])
    mtext(tsource, side=1, line=3, outer=FALSE, cex=0.75)
    # output of charts to file
    dev.copy(png, width=800, height=500, paste("M_precipitation_q", q, ".png", sep=""))
    dev.off()
} # end of quantiles
        
