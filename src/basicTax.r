library(tidyverse)

# R program to plot line graph
x = c(0,2,10)

#create new graphics device
png(filename="static/basicTax.png")

# plot empty axes
plot(0,0,xlim = c(0,10),ylim = c(0,2),xlab="income ($10,000)", ylab="tax ($10,000)", type = "n")

flatTax = function(x){ 0.1 * x }
tax1 = map(x, flatTax)
lines(x, tax1, type='l',col= "blue")

progressiveTax = function(i){
  if(i<2){
    0
  }else{
    (i-2) * 0.2
  }
}
tax2 <- map(x, progressiveTax)
lines(x, tax2, type='l',col= "red")

# close graphics device
dev.off()
