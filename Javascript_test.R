library(canvas)

# very simple plot
canvas(600, 600, file="plot.js")
plot(rnorm(4000),rnorm(4000),col="#ff000018",pch=19,cex=2) # semi-transparent red
dev.off() # creates a file "plot.js" with the above plot

library(HTMLUtils)
JSCPATH <- 'C:/Tools/JSC/'
InstallJSC(JSCPATH)
