library(tidyverse)

scaleVector = function(x) {x/1000}
# from https://www.irs.gov/newsroom/irs-provides-tax-inflation-adjustments-for-tax-year-2020
singleX <- c(0, 9875, 40125, 85525, 163300, 207350, 518400, 600000) %>%
  scaleVector

singleY <- c(0, 987.5, 4617.5, 14605.5, 33271.5, 47367.5, 156235, 186427) %>%
  scaleVector

marriedFilingJointX = c(0, 19750, 80250, 171050, 326600, 414700, 622050, 700000) %>%
  scaleVector
marriedFilingJointY = c( 0, 1975, 9235, 29211, 66543, 94735, 167307.5, 196149) %>%
  scaleVector

#create new graphics device
png(filename="static/2020brackets.png")

# plot empty axes
# plot(0,0,xlim = c(0,100000),ylim = c(0,20000),xlab="income", ylab="tax", type = "n")
plot(0,0,xlim = c(0,600),ylim = c(0,200),xlab="income (000)", ylab="tax (000)", type = "n")

lines(singleX, singleY, type='l',col= "blue")
lines(10000, 0, type='l',col= "blue")
lines(marriedFilingJointX, marriedFilingJointY, type='l',col= "red")

# close graphics device
dev.off()
