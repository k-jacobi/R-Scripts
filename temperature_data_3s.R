# reading temperature data from Excel
# script version: 27/06/2011

library(RODBC) # also under R-2.13 odbcConnectExcel is still limited to 32 bit 

setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Temperatures/')

channel <- odbcConnectExcel("climatedata(3 stations).xls")

output <- "dailyclim.txt"
ds <-"%Y-%m-%d"

# station specific data
ws <- c("shogran daily","saif ul muluk","pir chinasi") # worksheet name
station <- c("Shogran","Saif ul Muluk","Pir Chenazi")
vy1 <- c(2000,2000,2004)
vy2 <- c(2009,2009,2009)

rd1 <- 3 # first row with data
rd2 <- 368 # last row with data
c1 <- 3 # first column with data
nc <- 3 # number of columns per day (tmin, tmax, prec)
yrow <- rd2-rd1+1

for (s in 1:3) {
    
    exceldf <- sqlFetch(channel, ws[s])
    # make empty dataframe to put the daily data
    cdf <- data.frame(station=character(0), date=character(0),tmin=numeric(0),tmax=numeric(0),
                      prec=numeric(0), stringsAsFactors =F )
 
    ddf <- as.character(exceldf[rd1:rd2,1]) # datefield
    y1 <- vy1[s]
    y2 <- vy2[s]
    nrow <- (y2-y1+1)*yrow
    cdf[1:nrow,1] <- station[s]

    for (y in y1:y2) {
        yi <- y-y1+1
        ci <- c1+(yi-1)*nc
        ydf <- exceldf[rd1:rd2,ci:(ci+nc-1)]
        cdf[((yi-1)*yrow+1):(yi*yrow),3:5] <- ydf
        ddfy <- paste(y,substr(ddf,6,11),sep="-")
        if (floor(y/4) == ceiling(y/4)) ddfy[60] <- paste(y,"02-29",sep="-") else ddfy[60] <- NA # leapyear
        cdf[((yi-1)*yrow+1):(yi*yrow),2] <- ddfy
    } # last year
    
    write.table(cdf,output,append=T,col.names=F,row.names=F)
        
} # last worksheet

