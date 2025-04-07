# Day 26 - 4/7/2025
# Notes 12
# Note: See Code to Copy file!

library(tidyverse)
#----------------------------------------------------

#--------------------------------------------------------

set.seed(2320)
x <- rnorm(1000, 100, 5)
mean(x)
sd(x)
trans.x <- 5*x
mean(trans.x)
sd(trans.x)

#1
trans.x <- 7*x
mean(trans.x)
sd(trans.x)


#3
trans.x <- x + 10
mean(trans.x)

#5
#Revised with a different Normal Distribution
x <- rnorm(1000, 100, 1)
mean(x)
sd(x)
trans.x <- 5*x
mean(trans.x)

#1
trans.x <- 7*x
mean(trans.x)

#3
trans.x <- x + 10
mean(trans.x)

#Revised with a different Normal Distribution
x <- rnorm(1000, 108, 2)
mean(x)
sd(x)
trans.x <- 5*x
mean(trans.x)

#1
trans.x <- 7*x
mean(trans.x)


#3
trans.x <- x + 10
mean(trans.x)



#7
x <- rnorm(1000,50,7)
trans.x <- 3*x+100
mean(trans.x)

#8
#A
x <- rnorm(1000,100,15)
mean(x)
sd(x)
var(x)

#B
trans.x <- 5*x
var(trans.x)

#C
5*225
var(trans.x)/225
5*5*225


#9
#A
x <- rnorm(1000,100,15)
mean(x)
sd(x)
var(x)

#B
trans.x <- x+10
var(trans.x)
var(x)


#----------------------------------------------
# Notes 13!

# Investigation #1 
#A

x1 <- rnorm(5000)
x2 <- rnorm(5000)
y <- x1 + x2

hist(y)
mean(y)
sd(y)
var(y)

#B

x1 <- rnorm(5000)
x2 <- rnorm(5000,5,3)
y <- x1 + x2

hist(y)
mean(y)
sd(y)
var(y)

#C

x1 <- rnorm(5000,5,3)
x2 <- rnorm(5000,5,3)
y <- x1 + x2

hist(y)
mean(y)
sd(y)
var(y)
