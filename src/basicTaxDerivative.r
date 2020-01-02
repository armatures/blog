# R program to plot line graph
x = c(0,2,2,10)

#create new graphics device
png(filename="static/basicTaxDerivative.png")

# plot empty axes
plot(0,0,xlim = c(0,10),ylim = c(0,2),xlab="income ($10,000)", ylab="tax ($10,000)", type = "n")

flatTax = c(0.1,0.1,0.1,0.1)

# lines(x, income, type='l')
lines(x, flatTax, type='l',col= "blue")

tax2 <- c(0,0,0.2,0.2)
lines(x, tax2, type='l',col= "red")

# tax1 = c(0,2,1)
# lines(tax1, type='b',col= colors[1])

# close graphics device
dev.off()
