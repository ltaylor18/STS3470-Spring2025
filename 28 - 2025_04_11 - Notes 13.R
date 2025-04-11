# Day 28 - 4/11/2025
# Notes 13
# Note: See Code to Copy file!

library(tidyverse)
#----------------------------------------------------

# Exercise

set.seed(2320)
bottles <- rnorm(9,12,.05)
bottles

mean(bottles)

#2c
1-pnorm(mean(bottles),12,.05/sqrt(9))

#3d
1-pnorm((mean(bottles)-12)/(.05/sqrt(9)))

#4a
x <- rnorm(100000,12,.05/sqrt(9))
1-sum(x <= mean(bottles))/100000
sum(x >= mean(bottles))/100000

x <- rnorm(500000,12,.05/sqrt(9))
sum(x >= mean(bottles))/500000


