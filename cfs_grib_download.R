# download of daily grib files from NCEP-CFS
# script version 01/12/2011

options(timeout=180)
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Forecasts/CFS/dl/')
url <- 'ftp://ftpprd.ncep.noaa.gov/pub/data1/nccf/com/cfs/prod/'
today <- Sys.Date()
vars <- c("tmp2m","prate")
glistf <- list.files(pattern=".daily") 


for (d in (-8:-2)){
    d.str <- paste(url,"forecast.",gsub("-","",as.character(today+d)),sep="")
    for (s in 1:4){
        ds.str <- paste(d.str,"/daily_grib_", sprintf("%02d",s),sep="")
        for (v in 1:2){
           fname <- paste(vars[v],".",sprintf("%02d",s),".",gsub("-","",as.character(today+d)),"00.daily",sep="")
           isource <- paste(ds.str,fname,sep="/")

           if (fname %in% glistf) next # no download if file exists already
           download.file(isource, fname, quiet=F, method="curl", mode="wb")
        }  # end loop over v
    } # end loop over s
} # end loop over d

