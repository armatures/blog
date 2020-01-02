---
title: Rendering Math
date: "January 3, 2020"
---

In my last post, I wanted to display two types of content I didn't yet support in this blog: a piecewise math function, and the graph of this function. I decided the best options were probably LaTeX and R, respectively. I started by working on rendering each style of graphic outside of Hakyll, and coding in image tags referencing the rendered images in by hand. If it turns out I often want to render things like this, I'll streamline this process.

## Rendering math

Given a file `piecewise.tex` containing some LaTeX math like this:
```
\usepackage{amsmath}
\title{piecewise}

\[ \begin{cases}
      0 & x\leq 20,000 \\
      {0.2} (x - 20,000) & 20,000\leq x
   \end{cases}
\]
```

Pandoc can render it by running: `pandoc piecewise.tex -s --mathml  -o piecewise.html`. I installed pandoc on my mac using `brew`.

## Rendering the graph

I decided this would be best accomplished using R. I wrote the following R in a file named `line.r`:
```r
x = c(0,10)
income = c(0,10)
tax = c(0,1)

#create new graphics device
png(filename="basicTax.png")

# plot empty axes
plot(0,0,xlim = c(0,10),ylim = c(0,10),xlab="income", ylab="tax", type = "n")

lines(x, income, type='l')
lines(x, tax, type='l',col= "blue")

x2 <- c(0,2,12)
tax2 <- c(0,0,2)
lines(x2, tax2, type='l',col= "red")

# tax1 = c(0,2,1)
# lines(tax1, type='b',col= colors[1])

# close graphics device
dev.off()
```
This can be converted into a png by running: `r -f line.r`. It would be nice to decouple the contents of the file from the name of the file it writes to, but having written approximately no R before this, I decided this was enough trouble to call it a success. 
