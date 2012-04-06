# find gaps in file list (here: cloudfree TIFs)
# script version: 17/06/2011

library(tseries)

setwd('C:/AHT.PRO/Pakistan_Modelling/Data/TIFs/') # set working directory = location of TIF files
glistf <- list.files(pattern="...._.._...tif") # load complete file list of cloudfree TIFs into a R character vector

chartw <- 365 # chartwidth

tifs <- substr(glistf,1,10)
tifs <- as.POSIXct(tifs, format="%Y_%m_%d", tz = "GMT")
tifs <- as.data.frame(tifs, stringsAsFactors = FALSE)
tifs[2] <- 0
names(tifs) <- c("date", "interval")
maxd <- dim(tifs)[1]
nmax <- ceiling(maxd/chartw)

gapl <- tifs[0,] # <- makes empty dataframe with same structure like tifs

l <- 1

for (d in 2:maxd){
    tifs[d,2] <- as.integer((tifs[d,1] - tifs[d-1,1])) # row, column
    if (tifs[d,2] > 2) {
        gapl [l,] <- tifs [d,]
        l <- l+1
    } # end if
}

write.table(gapl, "gaps_cloudfree.txt")

# for (n in 0:nmax){
#     n1 <- n*chartw+1
#     n2 <- (n+1)*chartw
#     tifss <-tifs[n1:n2,] # row, column
#     from <- tifss[1,1]
#     to <- tifss[(chartw-1),1]
#     title <- paste("Availability of snow-free tif files from ", from, "to", to)
#     tifts <- as.irts(tifss)
#     tiftxs <- as.xts(tifts)
#     plot(tifts, type="p", main=title)
# }