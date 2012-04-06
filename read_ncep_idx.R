# reading idx files from NCEP
# script version 29/11/2011

setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Forecasts/GFS/')
glistf <- list.files(pattern=".idx")

my_idx <- read.table(glistf[1],sep=":")
my_idx <- my_idx[0,1:6]

for (f in glistf){
   t_idx <- read.table(f,sep=":")
   #TMP_idx <- subset(t_idx, V4=='TMP' & V5=='2 m above ground')
   TMP_idx <- subset(t_idx, V4=='TMP', select = 1:6)
   PCP_idx <- subset(t_idx, subset= grepl("PCP", V4), select = 1:6)
   PRATE_idx <- subset(t_idx, V4=='PRATE', select = 1:6)
   
   if (nrow(TMP_idx)>0) my_idx <- rbind(my_idx, TMP_idx)
   if (nrow(PCP_idx)>0) my_idx <- rbind(my_idx, PCP_idx)  
   if (nrow(PRATE_idx)>0) my_idx <- rbind(my_idx, PRATE_idx)
     
}
