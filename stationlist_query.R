# load and filter word-wide station data

library(sqldf)

setwd('C:/AHT.PRO/Pakistan_Modelling/NOAA/Stations/') # set working directory 

stattab <- read.table("ghcnd-inventory.txt")
tsql <- "SELECT * FROM stattab WHERE V6 = 2011 AND (V4 = 'TMIN' OR V4 = 'TMAX')"
stattab_11 <- sqldf(tsql)
tsql <- "SELECT V1 as 'code', V2 as 'Y', V3 as 'X' FROM stattab_11 GROUP BY V1 HAVING COUNT(V1) = 2"
stattab2_11 <- sqldf(tsql)

write.table(stattab2_11,"statlist.txt", row.names=F)

tsql <- "SELECT V1 as 'code', V2 as 'Y', V3 as 'X' FROM stattab GROUP BY V1 HAVING V6 > 2000"
stattab_alt <- sqldf(tsql)

write.table(stattab_alt,"statlist_alt.txt", row.names=F)