library(tidyverse)

rates <- data.frame(
c("Rate", "Unmarried", 	"Married Filing Joint" , "Heads of Households"),
c(.10, 0, 	0, 	0),
c(.12, 9700 	,19400 	,13850),
c(.22, 39475 	,78950 	,52850),
c(.24, 84200 	,168400 	,84200),
c(.32, 160725 	,321450 	,160700),
c(.35, 204100 	,408200 	,204100),
c(.37, 510300 	,612350 	,510300)
)

#10% 	$0 	$0 	$0
#12% 	$9,700 	$19,400 	$13,850
#22% 	$39,475 	$78,950 	$52,850
#24% 	$84,200 	$168,400 	$84,200
#32% 	$160,725 	$321,450 	$160,700
#35% 	$204,100 	$408,200 	$204,100
#37% 	$510,300 	$612,350 	$510,300
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
