# Floodflow Statistics
# Script 06/12/2011

library (RODBC)
library (lmomco) # Pearson

# --- configurable section ----
setwd('C:/AHT.PRO/Pakistan_Modelling/')

xlfile <- "Flowdata.xls"
dsource <- odbcConnectExcel(xlfile, readOnly = TRUE)
flows.df <- sqlFetch(dsource, "Sheet1", colnames = F, rownames = T)
thresholds <- c(50,200,250,300,350,400) # reduce sample size by values below
# ---- end of configurable section

nr <- length(thresholds)
forecast <- data.frame(A=c(2,5,10,50,100,500,1000,5000,10000))
forecast[,2] <- 1-1/forecast[,1]
names(forecast)[2] <- "P"

or <- nrow(flows.df)
ylim <- c(0,1)
xlim <- c(100,1000)
par(cex=0.75)

for (r in 1:nr){
    flows.df <- subset(flows.df, Q>thresholds[r])
    nobs <- nrow(flows.df)
    flows.df[,3] <- pp(flows.df[,2], a=0, sort=F) # plotting position
    names(flows.df)[3] <- "P"
    
    lmom <-lmom.ub(flows.df[,2]) # Unbiased Sample L-moments by Direct Sample Estimators
    parapear <- parpe3(lmom, checklmom=T) # Estimate the Parameters of the Pearson Type III Distribution
    flows.df[,4] <- cdfpe3(flows.df[,2], parapear)
    
    tmain <- paste("Plotting of observed annual flood peaks above ",as.character(thresholds[r]),"m³/s -", as.character(nobs), "from",as.character(or),"observations")
    plot(flows.df[,2:3],ylim=ylim, xlim =xlim, main=tmain)
    s.flows.df <- flows.df[order(flows.df$Q),]
    par(new=T)
    plot(x=s.flows.df[,2], y=s.flows.df[,4], type="l", col="red", ylim=ylim, xlim=xlim, xlab="",ylab="")
    grid()
    forecast[,2+r] <- as.integer(quape3(forecast[,2], parapear, paracheck=TRUE))
    names(forecast)[2+r] <- as.character(thresholds[r])
}
write.table(forecast,"forecast.txt", row.names=F)
