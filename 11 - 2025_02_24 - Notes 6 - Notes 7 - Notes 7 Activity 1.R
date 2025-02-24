# Day 11 - 2/24/2025
# Finish Notes 6
# Start Notes 7

#------------------------------------------------

castings <- c(2541, 
              2543, 
              2544, 
              2552, 
              2553, 
              2553, 
              2559, 
              2560, 
              2562, 
              2620)

1 - pt(1.21, 9)
pt(1.21, 9)


#------------------------------------------------
# Lesson 7 - Mini-Activity #1
library(tidyverse)
View(ToothGrowth)

mypigs <- filter(ToothGrowth,supp=="VC",dose==0.5)

summarize(mypigs,mean(len),sd(len),n())

ggplot(mypigs, aes(x=len)) + geom_boxplot()

t.test(mypigs$len, alternative="greater", mu=8)
t.test(mypigs$len, alternative="less", mu=8)
t.test(mypigs$len, alternative="two.sided", mu=8)

t.test(mypigs$len)
