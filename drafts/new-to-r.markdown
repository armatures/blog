---
date: "1/10/2020"
title: "Learning R"
---

I have lots of friends that write R. Because I'm curious about my friends, and because it seems popular, and because I don't know R yet, I figured I should learn some of the language. Also, I had a need to plot some functions for a post about taxes, and this seemed very well-suited to the language.

I've been having a rough go with R. Haskell gives me some expectations about functions that don't translate to R. In functional programming, it's very common to split the first element of a list from the rest of the list. I haven't figured out how to do this in R, but in doing so I discovered some functions with familiar names and unfamiliar behavior. 

# expressions that work differently between R and Haskell

I am hesitant to publish a list like this, because I don't want to complain. I _do_ want to figure out the language's paradigm, though: documenting problems as I go is the best way to develop my thinking about R's sensibilities.

### R code:
```r
drop(1, 1:5) # Error in drop(1, 1:5) : unused argument (1:5)
head(1:5) # 1 2 3 4 5
tail(1:5) # 1 2 3 4 5
zip(1:5, 2:6) # Error in zip(1:3, 4:6) : 'files' must a character vector specifying one or more filepaths
```

### Haskell code:

```haskell
drop 1 [1..5] -- [2..5]
head [1..5] -- 1
tail [1..5] -- [2..5]
zip [1..3] [4..6] -- [(1,4),(2,5),(3,6)]
```

I'm not sure yet why `head` and `tail` have different definitions in R and Haskell. Maybe this should be a red flag to me that I am doing something unusual, or that the patterns I'm bringing to the language might not be performant. Half the reason I learned Haskell was to inform my approaches to programming in other languages, so I'm not too worries about the former concern. The latter concern is something to keep an eye on. For the time being, I'll roll my own definition for alternate `head` and `tail` functions, and add to this post with ongoing ideas about R.
