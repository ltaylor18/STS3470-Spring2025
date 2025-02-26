# Lesson 7 - Activity #3
# Code to copy!
library(readxl)
library(tidyverse)
tsa <- read_excel("_________________")

tsa <- mutate(tsa, close_amount = as.numeric(`Close Amount`))

#1
trueTSAmean <- __________________

#2
mysamp <- sample(___________$_________________, ___, replace=FALSE)

#3
t.test()

#4
out <- replicate(___,t.test(sample(tsa$close_amount,__,replace=FALSE),mu=__)$p.value)

#5
sum(out < 0.05)/10000