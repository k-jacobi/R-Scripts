# TCrit daily
# last update: 11/07/2011

library(geosphere) # for calculation of daylength

lat = 34 # central latitude Mangla catchment
TC0 <- 3.0 # Tcrit on March 21
dl0 <- 12.2 # daylength at T0
TCgrad <- -0.64 # Tcrit adjustment per 1 hour difference in daylength
fname <- "Tcrit.txt"
setwd('C:/AHT.PRO/Pakistan_Modelling/Data/Temperatures/') # set working directory

write("DOY daylength Tcrit",file=fname)

for (d in 1:365){
    dl <- daylength(lat, d)
    TCi <- TC0 + (dl-dl0) *TCgrad # threshold for snowfall
    write(c(d, dl, TCi), file = fname, ncolumns=3, append = TRUE, sep = " ") 
}
