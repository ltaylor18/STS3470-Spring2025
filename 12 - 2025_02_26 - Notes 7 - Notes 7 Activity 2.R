# Day 12 - 2/26/2025
# Notes 7
# Notes 7 - Activity 2

#------------------------------------------------

# Lesson 7 - Mini-Activity #2
library(tidyverse)
#View(ToothGrowth)

mypigs <- filter(ToothGrowth,supp=="VC",dose==0.5)
t.test(mypigs$len)
out <- t.test(mypigs$len)
names(out)

#95%
ci95U <- t.test(mypigs$len)$conf.int[2]
ci95L <- t.test(mypigs$len)$conf.int[1]

(ci95U+ci95L)/2 #number in middle
mean(mypigs$len) #sample mean!

ci95U - ci95L #width

ci95U - mean(mypigs$len) #MOE

#90%
t.test(mypigs$len,conf.level=0.90)
ci90U <- t.test(mypigs$len,conf.level=0.90)$conf.int[2]
ci90L <- t.test(mypigs$len,conf.level=0.90)$conf.int[1]

(ci90U+ci90L)/2 #number in middle
mean(mypigs$len) #sample mean!

ci90U - ci90L #width

ci90U - mean(mypigs$len) #MOE

#99%
t.test(mypigs$len,conf.level=0.99)
ci99U <- t.test(mypigs$len,conf.level=0.99)$conf.int[2]
ci99L <- t.test(mypigs$len,conf.level=0.99)$conf.int[1]

(ci99U+ci99L)/2 #number in middle
mean(mypigs$len) #sample mean!

ci99U - ci99L #width

ci99U - mean(mypigs$len) #MOE


#-------------------------------------------------
# How does sample size impact hypothesis testing

t1 <- (101.5-100)/(5.28/sqrt(16))
t2 <- (101.5-100)/(5.28/sqrt(49))
t3 <- (101.5-100)/(5.28/sqrt(225))
t4 <- (101.5-100)/(5.28/sqrt(900))

1-pt(t1,15)
1-pt(t2,48)
1-pt(t3,224)
1-pt(t4,899)

#-------------------------------------------------
# How does sample size impact confidence intervals

# critical values
abs(qt(0.025, 15))
abs(qt(0.025, 48))
abs(qt(0.025, 224))
abs(qt(0.025, 899))

# margin of errors
qt(0.025, 15)*(5.28/sqrt(16))
qt(0.025, 48)*(5.28/sqrt(48))
qt(0.025, 224)*(5.28/sqrt(225))
qt(0.025, 899)*(5.28/sqrt(900))