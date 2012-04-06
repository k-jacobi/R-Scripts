# calculation of moving averages and their statistics

library(TTR) # for moving average function
library(sqldf)

setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Temperatures/GHCND/') # set working directory

Ts.file <- "Temp_monthly.txt"
Ts.df <- read.table(Ts.file, header=T)

i <- 3 # (interval of moving average)
Ts.df[,4] <- SMA(Ts.df[,3],n=i) # moving average, at end of period
mrow <- nrow(Ts.df)-1
for (r in 1:mrow) Ts.df[r,4] <- Ts.df[r+1,4] # average shifted from begin of period

names(Ts.df)[4] <- "3_months"
use <- complete.cases(Ts.df) # delete rows with NAs
Ts.df <- Ts.df[use,]

# calculate individual quantile for each row

# save results
write.table(Ts.df,"Temp_amov3m.txt", row.names=FALSE)

# quantile list
#ql <- c(0.05, 0.2, 0.333, 0.5, 0.667, 0.8, 0.95)
ql <- c(0.05, 0.2, 0.5, 0.8, 0.95)
quant <- quantile(Ts.df[,4],probs=ql,names=T, type=6)
qdt <- as.table(quant)
qdf <- as.data.frame(qdt)
qll <- as.vector(qdf[,1])
qdf <- data.frame(Period=character(0), Q_1=numeric(0), Q_2=numeric(0), Q_3=numeric(0), Q_4=numeric(0), Q_5=numeric(0), stringsAsFactors =F)
names(qdf)[2:6] <- qll

# calculate cumulative frequencies for each month
for (m in 1:12) {
    tstmi <- subset(Ts.df, (Ts.df[,2]==m))
    quant <- quantile(tstmi[,4],probs=ql,names=F, type=6)
    qdf[m,] <- c(m,quant)
  }
qdf[,1] <-seq(1:12)
write.table(qdf,"Temp_amov3_stats.txt", row.names=FALSE)

Ts.df.p <- data.frame(Year=integer(0), Month=integer(0), P =numeric(0), Tmean =numeric(0))

for (m in 1:12) {
    Ts.df.m <- subset(Ts.df, Ts.df[,2]==m) # monthly subset
    names(Ts.df.m)[3] <- 'cumP'
    Ts.df.m <- Ts.df.m[order(Ts.df.m[,4]),] # sorting by column
    n <- nrow(Ts.df.m)+1
    Ts.df.m[,3] <- row(Ts.df.m[1])/n
    Ts.df.m <- Ts.df.m[order(Ts.df.m[,1]),] # sorting back by column
    Ts.df.p <- rbind(Ts.df.p, Ts.df.m)
  }

write.table (Ts.df.p,"Temp_amov3mp.txt", append = FALSE, row.names=FALSE)

# # testing:
# y1 <- 2006
# y2 <- 2011
# ylim=c(0,28)
# xlim=c(1:12)
# label <- c("ind. months","mov.averages","movav 80%","movav 50%","movav 820%" )
# 
# for (y in y1:y2){
#     plot(qdf[,3], type='l',col="blue", lty=2, lwd=2 , ylab="", xlab="", ylim=ylim)
#     par(new=T)
#     plot(qdf[,4], type='l',col="blue", lwd=2 , ylab="", xlab="", ylim=ylim)
#     par(new=T)
#     plot(qdf[,5], type='l',col="blue", lty=2, lwd=2, ylab="", xlab="", ylim=ylim)
#     par(new=T)
#     Ts.plot <- subset(Ts.df,Ts.df[,1]==y)
#     plot(Ts.plot[,3], type='l', lwd=2, col="green", ylab="Tmean °C", xlab="months", ylim=ylim, main=y)
#     par(new=T)
#     plot(Ts.plot[,4], type='l',col="red",lwd=2, ylab="", xlab="", ylim=ylim)   
#     grid()
# }
