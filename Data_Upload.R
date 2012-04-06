# Export from text files to Access mdb for use by WinSRM
# last update 26/06/2011

library(sqldf)
library(RODBC)

#General
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/') # set working directory for Access mdb
mdb <- "mangla_data.mdb"
db_table_name <-"PhysicalData"
idx <- (c("Zone","Date")) # required, case sensitive - Access MDB must get primary keys!
con <- odbcConnectAccess(mdb, uid="") # directly connect to MS-Access DB
dbt <- sqlFetch(con, db_table_name) # load complete table as dataframe
dbt[,2] <- as.character(dbt[,2])

# Temperature data
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Temperatures/GHCND/') # set working directory 

tsouti <- read.table("temp_interpol.txt",header=T) # time series from a single station
tsql <- "SELECT dbt.Zone, dbt.Date, tsouti.tmin as Tmin, tsouti.tmax as Tmax, (tsouti.tmin+tsouti.tmax)/2 as Tavg FROM dbt INNER JOIN tsouti ON dbt.Date = tsouti.date"
t_up <- sqldf(tsql)
sqlUpdate(con, dat=t_up, tablename=db_table_name, index=idx)

# Precipitation Data
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Precipitation/NOAA/')# set working directory
cc <- c("character","integer","numeric","numeric","numeric")
tprec <- read.table("rainfall.txt",header=F,colClasses=cc) # time series with data per ez and day
tsql <- "SELECT dbt.Zone, dbt.Date, tprec.V3 as Precip FROM dbt INNER JOIN tprec ON dbt.Date = tprec.V1 AND dbt.Zone = tprec.V2"
t_up <- sqldf(tsql)
sqlUpdate(con, dat=t_up, tablename=db_table_name, index=idx)

# Discharge data - read directly from Excel file
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Discharges/') # set working directory
channel <- odbcConnectExcel("DailyDischargeData_edited.xls")
wse <- sqlTables(channel) # to verify worksheet names
ws <- c("DOMEL","MANGLA","KOTLI","KUNHAR@GHARI-HABBIB-ULLAH","NEELUM@MUZAFFARABAD") # worksheet names

for (w in 1:5){
    exceldf <- sqlFetch(channel, ws[w], rownames=F, as.is=T)
    tQ <- exceldf[,1:2]
    tQ[,1] <- ws[w]
    tQ[,2:3] <- exceldf[,1:2]
    tQ[,2] <- as.character(as.Date(tQ[,2]))
    names(tQ) <- c("station","date","Q")
    if (w==1) tQout <- tQ else tQout <- rbind(tQout, tQ) # append block of station data to main dataframe
}
write.table(tQout,"DailyDischarges.txt",row.names=F)

tsql <- "SELECT dbt.Zone, dbt.Date, tQout.Q AS Runoff FROM dbt INNER JOIN tQout ON dbt.Date = tQout.date WHERE tQout.station = 'MANGLA'"
t_up <- sqldf(tsql)
sqlUpdate(con, dat=t_up, tablename=db_table_name, index=idx)

odbcClose(channel)
odbcClose(con)