# Mean daily temperature per elevation zone, adjusted by daylength

# last update: 11/07/2011

library(geosphere) # for calculation of daylength
library(sqldf)
library(timeSeries)

lat = 34 # central latitude Mangla catchment
TC0 <- 3.0 # Tcrit on March 21
Tgrad <- -0.65 # Degrees per 100 m (Temperature lapse rate or gradient)
dl0 <- 12.2 # daylength at T0
TCgrad <- -0.64 # Tcrit adjustment per 1 hour difference in daylength
L0 <- 1587 # Altitude Srinagar met station
fname <- "Tmean.txt"
clist <- "clist.txt"
tlist <- "temp_interpol.txt"
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Temperatures/GHCND/') # set working directory

write(c("catchment","ez","date","Tmean","day_degree"), file=fname, ncolumns=5, append = FALSE,  sep = " ")

ttlist <- read.table(tlist, header=T)
ttlist <- removeNA(ttlist)
tclist <- read.table(clist, header=T)

tsql <- "SELECT catchment FROM tclist GROUP BY catchment"
tclisti <- sqldf(tsql)
cn <-  dim(tclisti[1]) [1]
dn <- dn <- dim(ttlist[1]) [1]

for (c in 1:cn){
    catchment <- as.character(tclisti[c,1])
    tsql <- paste("SELECT * FROM tclist WHERE catchment ='",catchment,"' AND ncells > 0",sep="")
    tclistit <- sqldf(tsql)
    ez1 <- min(tclistit[,2])
    ez2 <- max(tclistit[,2])
    
    # make empty dataframe to put the daily data
    tdf <- data.frame(catchment=character(0), ez=integer(0), date=character(0),Tmean=numeric(0), DDF=numeric(0), stringsAsFactors=F)
    tdfd <- tdf # dataframe for daily data
    
    for (ez in ez1:ez2){
        tsql <- paste("SELECT hmean FROM tclistit WHERE ez=",ez,sep="")
        hm <- sqldf(tsql)[1,] # hypsometric mean for ez and catchment
        hadj <- (hm - L0)/100*Tgrad # additive correction °C for level of ez relative to station
        tdfd[1:dn,1] <- catchment # catchment name
        tdfd[1:dn,2] <- ez
        for (d in 1:dn){
            dl <- daylength(lat, as.Date(ttlist[d,2]))
            tadj <- (dl-dl0)*TCgrad  # additive adjustment °C considering daylength
            t <- (ttlist[d,3]+ttlist[d,4])/2 # mean temperature at station
            t <- t+tadj+hadj
            dd <- max(t,0) # day degree- only if > 0
            tdfd[d,3] <- as.character(ttlist[d,2]) # add date
            tdfd[d,4:5] <- c(t, dd) # add temperatures
        } # end of timeseries rainfall 
        tdf <- rbind(tdf, tdfd) # append block of daily values to main dataframe
    } # end of elevation zone
    write.table(tdf,fname, row.names=F, col.names=F, append=T, quote = F)
} # end of catchments
    
