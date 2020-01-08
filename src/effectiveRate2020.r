library(tidyverse)

scaleVector = function(x) {x/1000}
# from https://www.irs.gov/newsroom/irs-provides-tax-inflation-adjustments-for-tax-year-2020
bracketBoundaries <- c(0, 9875, 40125, 85525, 163300, 207350, 518400, 600000)
# %>% scaleVector
bracketRates <- c(10 ,12 ,22 ,24 ,32 ,35 ,37)

head_ = function(xs){
  head(xs,1)
}

tail_ = function(xs){
  tail(xs,length(xs) - 1)
}

zip_ = function(xs,ys){
  print("length xs")
  print(xs)
  matrix(c(xs,ys),length(xs))
#  acc=list()
#  for(i in 1:length(xs)){
#    acc = append(acc, c(xs[i],ys[i]))
#  }
#  acc
}

# print(head_(1:4))
# print(tail_(1:4))
zipped = zip_(1:3,5:7)

for(i in 1:nrow(zipped)) {
        print(zipped[i,])
      }

singleRate = function(income){
  for(i in 1:length(bracketRates)) {
  }
  print(paste("top bracket: ", income))
}
singleRate(90000)



quit()



#marriedFilingJointX = c(0, 19750, 80250, 171050, 326600, 414700, 622050, 700000) %>%
#  scaleVector
#marriedFilingJointY = c( 0, 1975, 9235, 29211, 66543, 94735, 167307.5, 196149) %>%
#  scaleVector

#create new graphics device
png(filename="static/2020effectiveBrackets.png")

# plot empty axes
# plot(0,0,xlim = c(0,100000),ylim = c(0,20000),xlab="income", ylab="tax", type = "n")
plot(0,0,xlim = c(0,600),ylim = c(0,200),xlab="income (000)", ylab="tax (000)", type = "n")

lines(singleX, bracketRates, type='l',col= "blue")
#lines(marriedFilingJointX, marriedFilingJointY, type='l',col= "red")

# close graphics device
dev.off()
