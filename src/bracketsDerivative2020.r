library(tidyverse)

scaleVector = function(x) {x/1000}
# from https://www.irs.gov/newsroom/irs-provides-tax-inflation-adjustments-for-tax-year-2020
singleX <- c(0
             , 9875 , 9875
             , 40125 , 40125
             , 85525 , 85525
             , 163300 , 163300
             , 207350 , 207350
             , 518400 , 518400
             , 700000
             ) %>%
  scaleVector

bracketRates <- c(10, 10
                  ,12 ,12
                  ,22 ,22
                  ,24 ,24
                  ,32 ,32
                  ,35 ,35
                  ,37 ,37)

marriedFilingJointX = c(0
                        , 19750 , 19750
                        , 80250 , 80250
                        , 171050 , 171050
                        , 326600 , 326600
                        , 414700 , 414700
                        , 622050 , 622050
                        , 700000
                        ) %>%
  scaleVector

#create new graphics device
png(filename="static/2020bracketsDerivative.png")

# plot empty axes
# plot(0,0,xlim = c(0,100000),ylim = c(0,20000),xlab="income", ylab="tax", type = "n")
plot(0,0,xlim = c(0,600),ylim = c(0,40),xlab="income (000)", ylab="marginal tax rate (000)", type = "n")

lines(singleX, bracketRates, type='l',col= "blue")
lines(marriedFilingJointX, bracketRates, type='l',col= "red")

# close graphics device
dev.off()

