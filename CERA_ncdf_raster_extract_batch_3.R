# Reading CERA dataset and extracting time series for selected coordinates
# file format must be netcdf
# script version: 2012-04-10

library(rgdal)
library(raster)
library(ncdf)
library(xts)
library(timeSeries)
library(TTR) # for moving average function
options(warn=-1)

# change settings here --------------------------------------------------

wkdir <- "C:/geo_data/CERA"
slist <- "scenarios.txt"
blist <- "CERA_sets.txt"
xlist <- "Mangla_CERA_coordinates.txt"
vars <- c("tas","pr") # surface temperature in K, precipitation rate in mm/s
ivls <- 1200 # number of intervals

Tgrad <- -0.65 # Degrees per 100 m (Temperature lapse rate or gradient)
L0 <- 1587 # Altitude Srinagar met station
r2t <- 24*3600*30
winter <- c(1,2,11,12) # definition of winter season
spring <- c(4,5,6) # definition of spring season
monsoon <- c(7,8,9)

# end of code section with editable settings ----------------------------

# conversion of K to °C and adjustment of temperature
degC <- function(K, Tgrad, L0, hm) K-273.15+(hm-L0)/100*Tgrad

sd <- function (dpy){
    if (dpy==365) {
        d1<- as.Date("2001-01-16") 
        d2<- as.Date("2100-12-16")
    } else{
        d1<- as.Date("2000-01-15") 
        d2<- as.Date("2099-12-15")       
    }
    sd <- seq(d1, d2, "months")
}

sd2 <- function(cnty) seq(as.Date(paste(cnty-1,"01-01-15",sep="")), as.Date(paste(cnty,"00-12-15",sep="")), "months")

# end of functions ---------------------------------------

setwd (wkdir)

slist.df <- read.table(slist, header=T)
ns <- nrow(slist.df)

for (s in 1: ns){
    wks <- paste(wkdir,as.character(slist.df[s,1]),sep="/")
    setwd (wks)
    
    century <- slist.df[s,2]
    blist.df <- read.table(blist, header=T)
    xlist.df <- read.table(xlist, header=T)
    
    nmod <- nrow(blist.df)
    nvars <- length(vars)
    ncols <- nrow(xlist.df)*nvars
    tlist <- data.frame(tmp=numeric(ivls)) # define data frame
    
    for (m in 1:nmod){ # models
        rr <- paste(blist.df[m,4],"nc",sep=".")
        ts <- blist.df[m,5]/12*86400 # interval
        mysd <- sd(blist.df[m,5])
        myp <- subset(xlist.df, model==blist.df[m,1])
        spl <- SpatialPoints(myp[,3:4])
        
        for (v in 1:nvars){ # model variables
            fn <- paste(blist.df[m,1], blist.df[m,2], blist.df[m,3], "N", vars[v], rr, sep="_")
            print(fn)
            mbrick <- brick(fn, varname=vars[v]) # works!
            mlist <- data.frame(t(extract(mbrick, spl, nl=ivls)))
            for (p in 1:nrow(myp)){
                #if (vars[v]=="tas") mlist[,p] <- degC(mlist[,p],Tgrad,L0, myp[p,5])
                if (vars[v]=="tas") mlist[,p] <- degC(mlist[,p],Tgrad,L0, 0)
                if (vars[v]=="pr") mlist[,p] <- mlist[,p]*r2t
                names(mlist)[p] <- paste(blist.df[m,1], blist.df[m,2], blist.df[m,3],vars[v],myp[p,2],sep="_")
            }
            tlist <- cbind(tlist,mlist)
        }
    }
    
    # dataframe for mdate, year, month
    tlist[,1] <- sd2(century)
    tlist.t <- data.frame(sd2(century))
    tlist.t[,2] <- as.integer(substr(tlist.t[,1],1,4))
    tlist.t[,3] <- as.integer(substr(tlist.t[,1],6,7))
    tlist.t[,1] <- as.Date(tlist.t[,1])
    names(tlist.t) <- c("mdate","year","month")
    
    tlist.tas <- cbind(tlist.t,tlist[,grep("tas",names(tlist))])
    tlist.pr <- cbind(tlist.t,tlist[,grep("pr",names(tlist))])
    
    # subset for winter months
    tlist.pr.w <- subset(tlist.pr, month==winter)
    tlist.tas.w <- subset(tlist.tas, month==winter)
    
    i <- 120 # (interval of moving average)
    i2 <- as.integer(i/2)
    m0row <- nrow(tlist.pr.w)
    mrow <- m0row - i2
    nc <- nmod*p
    tlist.pr.w.ma <- tlist.pr.w
    tlist.tas.w.ma <- tlist.tas.w
    for (m in 1:nc) {
        tlist.pr.w.ma[,3+m] <- SMA(tlist.pr.w[,3+m],n=i) # moving average, at end of period
        for (r in i2:mrow) tlist.pr.w.ma[r,3+m] <- tlist.pr.w.ma[r+i2,3+m] # average shifted to center of period
        tlist.pr.w.ma[(mrow+1):m0row,3+m] <- NA
        tlist.tas.w.ma[,3+m] <- SMA(tlist.tas.w[,3+m],n=i) # moving average, at end of period
        for (r in i2:mrow) tlist.tas.w.ma[r,3+m] <- tlist.tas.w.ma[r+i2,3+m] # average shifted to center of period
        tlist.tas.w.ma[(mrow+1):m0row,3+m] <- NA
    }
    
    # graphic output
    par(cex=0.7)
    pmax <- 400#1200
    
    ylim <- c(-30,20)
    for (m in 1:nmod){
        a <- (m-1)*p
        title <- paste("Temperature - Model",blist.df[m,1],"Scenario",blist.df[m,2])
        plot(tlist.tas.w.ma[1:pmax,4+a], ylim=ylim, type="l", lwd=2, col="red", ylab="°C", xlab="months", main=title)
        par(new=T)  
        plot(tlist.tas.w.ma[1:pmax,5+a], ylim=ylim, type="l", lwd=2, col="blue", ylab="", xlab="")
        par(new=T)  
        plot(tlist.tas.w[1:pmax,4+a], ylim=ylim, type="l", col="red", ylab="", xlab="")
        par(new=T)  
        plot(tlist.tas.w[1:pmax,5+a], ylim=ylim, type="l", col="blue", ylab="", xlab="")
        legend (pmax*0.9, ylim[2], c(as.character(myp[1,2]),as.character(myp[2,2])), lty=1, col=c("red","blue"))
        grid()
    }
    
    ylim <- c(0,120)
    for (m in 1:nmod){
        a <- (m-1)*p
        title <- paste("Precipitation - Model",blist.df[m,1],"Scenario",blist.df[m,2])
        plot(tlist.pr.w.ma[1:pmax,4+a], ylim=ylim, type="l", lwd=2,col="red", ylab="mm", xlab="months", main=title)
        par(new=T)  
        plot(tlist.pr.w.ma[1:pmax,5+a], ylim=ylim, type="l", lwd=2,col="blue", ylab="",xlab="")
        par(new=T)  
        plot(tlist.pr.w[1:pmax,4+a], ylim=ylim, type="l", col="red", ylab="", xlab="")
        par(new=T)  
        plot(tlist.pr.w[1:pmax,5+a], ylim=ylim, type="l", col="blue", ylab="", xlab="")
        legend (pmax*0.9, ylim[2], c(as.character(myp[1,2]),as.character(myp[2,2])), lty=1, col=c("red","blue"))
        grid()
    }
    
} # end of loop over scenarios



