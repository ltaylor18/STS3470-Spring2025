# Day 22 - 3/28/2025
# Notes 10 - Activities 2 and 3
# Note: See Code to Copy file!

# Notes 11 (unlikely!)

#----------------------------------------------------

# Updated Notes 10 - Activity 2!
library(tidyverse)

library(readxl)
CLTwait <- read_excel("~/STS 347/0_Spring 2025/New Data/CLT CBP Wait times.xlsx")
truemean <- mean(CLTwait$TotalPassengerCount)
truemean
hist(CLTwait$TotalPassengerCount)

#1
TypeISim <- function(n=10){
  out <- replicate(1000,t.test(sample(CLTwait$TotalPassengerCount,n,replace=TRUE),mu=truemean)$p.value)
  prop <- sum(out <= 0.05)/1000
  return(prop)
}

TypeISim()

#2
TypeISim(n=15)

#3
set.seed(2320)
TypeISim(n=15)

#4
set.seed(1001)

#5
myn <- seq(30,150,5)
myresults <- rep(NA,length(myn))

#6
for(i in myn){
  myresults[i/5 - 5] <- TypeISim(n = i)
}

#7
# Answer in notes

#8
TypeISimResults <- data.frame(myn=myn, myresults=myresults)
TypeISimResults
mean(TypeISimResults$myresults)

#9
library(tidyverse)
ggplot(TypeISimResults,aes(x=myn,y=myresults)) + 
  geom_point() +
  geom_line() +
  labs(x="Sample Size", y="Type I Error Rate") +
  geom_hline(yintercept=0.05,color="red")

mean(TypeISimResults$myresults)

# Regardless of sample size, the type I error rate remains around the
# nominal value of 0.05!



#------------------------------------------------------
# Lesson 10 - Activity 1 Part 3

#Type II Error Code:
out <- replicate(5000,t.test(sample(boxhouses$rooms,10,replace=FALSE),mu=4,alternative="two.sided")$p.value)
sum(out >= 0.05)/5000 #Type II Error
sum(out <= 0.05)/5000 #Power of Test

#1
# Answer in notes

#2
TypeIISim <- function(n=10, mualt=4){
  out <- replicate(5000,t.test(sample(boxhouses$rooms,n,replace=FALSE),mu=mualt,alternative="two.sided")$p.value)
  typeII <- sum(out >= 0.05)/5000 #Type II Error
  power <- sum(out <= 0.05)/5000 #Power of Test
  myreturn <- list(power=power, typeII=typeII)
  return(myreturn)
}

#3
#A
TypeIISim(n=15, mualt=5)

#B
TypeIISim(n=5, mualt=5)

#C
# Answer in notes

#D
TypeIISim(n=15, mualt=0)

#E
# Answer in notes

#4
myn <- 5:30
myresultsII <- matrix(NA, nrow=length(myn), ncol=2)

#5
#Note: Depending on how you coded your function, you may need to modify
#the following code.
out2 <- NULL
for(i in myn){
  print(i)
  out2 <- TypeIISim(n=i)
  myresultsII[i - 4, 1] <- out2$power 
  myresultsII[i - 4, 2] <- out2$typeII 
}

#6
TypeIISimResults <- data.frame(myn = myn, 
                               power = myresultsII[,1], 
                               typeII = myresultsII[,2])

#7
library(tidyverse)
ggplot(TypeIISimResults, aes(x=myn, y=power)) +
  geom_point() +
  geom_line() +
  labs(x="Sample Size", y="Power of Test")

#8
#a
myseq <- 0:15
myresultsIIB <- matrix(NA, nrow=length(myseq), ncol=2)

#b
out3 <- NULL
for(j in myseq){
  out3 <- TypeIISim(n = 10, mualt = j + 0.42)
  myresultsIIB[j + 1, 1] <- out3$power
  myresultsIIB[j + 1, 2] <- out3$typeII
}

#9
TypeIISimResultsB <- data.frame(myseq = myseq,
                                power = myresultsIIB[,1],
                                typeII = myresultsIIB[,2])


#10
library(tidyverse)
ggplot(TypeIISimResultsB, aes(x=myseq, y=power)) + 
  geom_point() +
  geom_line() +
  labs(x="Value of mu in Alternative Hypothesis", 
       y="Power of Test", caption="True value of mu is 7.42")


