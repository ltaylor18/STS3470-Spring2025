# Day 13 - 2/28/2025
# Notes 7 - Activity 3
# Notes 8

#-----------------------------------------------
# See files:
# Notes 7 Activity 3 Code to Copy
# Notes 8 - Code to Copy
# for reference code to minimize typing!
# ----------------------------------------------

#-----------------------------------------------
# Lesson 7 - Activity 2 Solutions
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

#------------------------------------------------

#------------------------------------------------

# Lesson 7 - Activity #3
library(readxl)
library(tidyverse)
tsa <- read_excel("~/STS 347/0_Spring 2025/New Data/tsa_data_2015.xlsx")
View(tsa)

tsa <- mutate(tsa, close_amount = as.numeric(`Close Amount`))

#1
trueTSAmean <- mean(tsa$close_amount, na.rm=T)
trueTSAmean

#2
mysamp <- sample(tsa$close_amount, 30, replace=FALSE)
mysamp

#3
t.test(mysamp,alternative="two.sided",mu=trueTSAmean)

#4
out <- replicate(5000,t.test(sample(tsa$close_amount,30,replace=FALSE),mu=trueTSAmean)$p.value)

#5
sum(out < 0.05)/5000

ggplot(tsa,aes(x=close_amount)) + geom_histogram(color="white")

#9 - Repeat for n=100
out100 <- replicate(5000,t.test(sample(tsa$close_amount,100,replace=FALSE),mu=trueTSAmean)$p.value)
sum(out100 < 0.05)/5000


#----------------------------------------------------
# Notes 8

#if_else()
myseq <- -10:10
if_else(myseq > 0, "Positive", "Other")

newseq <- if_else(myseq < 0, -1*myseq, myseq)
newseq

?if_else

#case_when()

myscores <- seq(60,80,2)
case_when(myscores >= 70 ~ "Pass", .default = "Fail")

grades <- case_when(myscores >= 70 ~ "Pass", .default = "Fail")
grades

classgrades <- data.frame(myscores,grades)
classgrades

case_when(myscores >= 90 ~ "A",
          myscores >= 80 ~ "B",
          myscores >= 70 ~ "C",
          myscores >= 60 ~ "D",
          myscores < 60 ~ "F")
#Assigns grades correctly

case_when(myscores < 60 ~ "F",
          myscores >= 60 ~ "D",
          myscores >= 70 ~ "C",
          myscores >= 80 ~ "B",
          myscores >= 90 ~ "A")
#Everyone gets a D!
