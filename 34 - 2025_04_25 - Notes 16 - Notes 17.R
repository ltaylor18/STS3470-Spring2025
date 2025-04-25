# Day 34 - 4/25/2025
# Notes 16
# Notes 17
# Note: See Code to Copy file!

library(tidyverse)

#-------------------------------------------------------

# Take Home #2 Solutions

# Consider the exponential distribution with a rate of 5.
# Recall that E(X) = 1/5 and Var(X) = (1/5)^2
# What is the distribution of X1 - X2

## 1 ##
# Use the rules of expectation to determine the E(X1-X2)
# E(X1-X2) = E(X1) - E(X2) = 1/5 - 1/5 = 0

## 2 ##
# Use the rules of variance to determine the Var(X1-X2)
# Var(X1-x2) = Var(X1) + (-1)^2 * Var(X2) = Var(X1) + Var(X2)
# = (1/5)^2 + (1/5)^2 = 2/25 = 0.08

## 3 ##
# Randomly generate x1 and x2 and calcualte the transformed
# variable y.
x1 <- rexp(1000,5)
x2 <- rexp(1000,5)
w <- x1-x2

# Verify your expected value and variance calculations using
# your randomly generated w's.
mean(w) # approximately 0!
var(w) # approximately 0.08!

## 5 ##
# Draw a density histogram. Do you believe the difference X1-X2 follows
# ... an Exponential distribution also?
# ... a Normal distribution?
# Why or why not?
mydata <- data.frame(w)
ggplot(mydata,aes(x=w)) + geom_histogram(aes(y=after_stat(density)),color="white")
# This does not look well described by the Exponential distribution because
# it is not right-skewed.
# It could potentially be a Normal distribution since it seems like
# it is a symmetric, uni-modal distribution.

## 6 ##
# Install and load the nimble library. (Only include the code to
# load the library).
library(nimble) 

## 7 ##
# Add the probability density function for the Double Exponential
# Distribution (the function is ddexp) with a rate of 5.
ggplot(mydata,aes(x=w)) + geom_histogram(aes(y=after_stat(density)),color="white") +
  stat_function(fun=ddexp,args=list(location=0,rate=5))

# Do you think the difference between two Exponential random variables
# with a rate of lambda each (X1 - X2) follows a 
# Double Exponential Distribution with a rate of lambda also?

# The curve seems to do a good job of describing the pattern
# of the density histogram.
# It may be a little tall at the center around 0.


## 8 ##
x1_x2 <- function(myrate = 5){
  x1 <- rexp(1000,myrate)
  x2 <- rexp(1000,myrate)
  w <- x1-x2
  mydata <- data.frame(w=w)
  print("The mean of X1-X2 is")
  print(mean(w))
  print("The variance of X1-X2 is")
  print(var(w))
  ggplot(mydata,aes(x=w)) + geom_histogram(aes(y=after_stat(density)),color="white") +
    stat_function(fun=ddexp,args=list(rate=myrate))
}

## 9 ##
x1_x2(myrate=10)
x1_x2(myrate=0.1)
x1_x2(myrate=1)

# The Double Exponential distribution seems to mimic
# the sharp point (super steep peak of the curve)
# very well for each of the scenarios investigated.

#-------------------------------------------------------------
# Notes 17 Activity 1

CIsim <- function(nreps=1000,n=30){
  #Simulate generating 30 observations from the Normal distribution and using t.test #to calculate the lower bound and upper bound of the confidence interval
  mysim <- replicate(nreps,t.test(rnorm(n,100,10),
                                  alternative="two.sided",
                                  mu=100,
                                  conf.level=0.95)$conf.int[1:2])
  
  #Store the lower bounds in the object lower and upper bounds in the object upper
  lower <- mysim[1,]
  upper <- mysim[2,]
  
  width <- upper-lower
  
  #Create an indicator for if the true value of mu is in the interval
  library(dplyr)
  indicator <- if_else(lower <= 100 & upper >= 100, 1, 0)
  
  #How many intervals correctly captured the value of mu?
  print(paste("There were", 
              sum(indicator), 
              "simulations that contained the population mean of 100 out of",
              nreps, 
              "simulations."))
  print(paste("The proportion of ", 
              0.95*100,
              "% intervals that captured the mean of 100 was ", 
              sum(indicator)/nreps,
              sep=""))
  print(paste("The average width of these intervals was",
              round(mean(width),2)))
}


CIsim()


#3
n <- 30
nreps <- 1000
mysim <- replicate(nreps,t.test(rnorm(n,100,10),
                                alternative="two.sided",
                                mu=100,
                                conf.level=0.95)$conf.int[1:2])
lower <- mysim[1,]
upper <- mysim[2,]
indicator <- if_else(lower <= 100 & upper >= 100, 1, 0)
mysim[,indicator==0]




CIsim2 <- function(nreps=1000,n=30,level=0.95){
  #Simulate generating 30 observations from the Normal distribution and using t.test #to calculate the lower bound and upper bound of the confidence interval
  mysim <- replicate(nreps,t.test(rnorm(n,100,10),
                                  alternative="two.sided",
                                  mu=100,
                                  conf.level=level)$conf.int[1:2])
  
  #Store the lower bounds in the object lower and upper bounds in the object upper
  lower <- mysim[1,]
  upper <- mysim[2,]
  
  width <- upper-lower
  
  #Create an indicator for if the true value of mu is in the interval
  library(dplyr)
  indicator <- if_else(lower <= 100 & upper >= 100, 1, 0)
  
  #How many intervals correctly captured the value of mu?
  print(paste("There were", 
              sum(indicator), 
              "simulations that contained the population mean of 100 out of",
              nreps, 
             "simulations."))
  print(paste("The proportion of ", 
              0.95*100,
              "% intervals that captured the mean of 100 was ", 
              sum(indicator)/nreps,
              sep=""))
  print(paste("The average width of these intervals was",
              round(mean(width),2)))
}


#4
CIsim2(level=0.90)

#5
CIsim2(level=0.99)

#6
CIsim2(n=10,level=0.95)
CIsim2(n=10,level=0.90)
CIsim2(n=10,level=0.99)

#7
CIsim2(n=50,level=0.95)
CIsim2(n=50,level=0.90)
CIsim2(n=50,level=0.99)

#8
CIsim3 <- function(nreps=1000,n=30,level=0.95){
  #Simulate generating 30 observations from the Normal distribution and using t.test #to calculate the lower bound and upper bound of the confidence interval
  mysim <- replicate(nreps,t.test(rexp(n,rate=1/100),
                                  alternative="two.sided",
                                  mu=100,
                                  conf.level=level)$conf.int[1:2])
  
  #Store the lower bounds in the object lower and upper bounds in the object upper
  lower <- mysim[1,]
  upper <- mysim[2,]
  
  width <- upper-lower
  
  #Create an indicator for if the true value of mu is in the interval
  library(dplyr)
  indicator <- if_else(lower <= 100 & upper >= 100, 1, 0)
  
  #How many intervals correctly captured the value of mu?
  print(paste("There were", 
              sum(indicator), 
              "simulations that contained the population mean of 100 out of",
              nreps, 
              "simulations."))
  print(paste("The proportion of ", 
              0.95*100,
              "% intervals that captured the mean of 100 was ", 
              sum(indicator)/nreps,
              sep=""))
  print(paste("The average width of these intervals was",
              round(mean(width),2)))
}

#n=30
CIsim3(n=30,level=0.95)
CIsim3(n=30,level=0.90)
CIsim3(n=30,level=0.99)

#n=10
CIsim3(n=10,level=0.95)
CIsim3(n=10,level=0.90)
CIsim3(n=10,level=0.99)

#n=50
CIsim3(n=50,level=0.95)
CIsim3(n=50,level=0.90)
CIsim3(n=50,level=0.99)



#-----------------------------------------------------------------------
# Example

library(ggplot2)
library(dplyr)
library(RColorBrewer) #You may need to install this package!
loc <- 0.95
mysim <- replicate(100,t.test(rnorm(30,100,10),
                              alternative="two.sided",
                              mu=100,
                              conf.level=loc)$conf.int[1:2])
lower <- mysim[1,]
upper <- mysim[2,]
indicator <- if_else(lower <= 100 & upper >= 100, 1, 0)
nsim <- length(lower)
mydata <- data.frame(row=1:length(mysim[1,]),
                     point.estimate=apply(mysim,2,mean),
                     lower.bound=lower,
                     upper.bound=upper,
                     check=indicator)
g <- ggplot(mydata,aes(x=row,
                       y=point.estimate,
                       ymin=lower.bound,
                       ymax=upper.bound,
                       color=factor(check))) +  
  geom_pointrange() + 
  coord_flip() +
  geom_hline(yintercept=100) +
  labs(title=paste("Simulation of ",loc*100,"% confidence",sep=""),
       subtitle=paste("Captured:",sum(indicator),"out of",nsim, "simulations"),
       x="",
       y="Confidence Intervals",
       color="Captured?")+
  scale_color_brewer(palette="Dark2")   
plot(g)







#-----------------------------------------------------------------------
# Notes 17 - Activity 2

ttestpower <- function(n1=30,n2=10,mudiff=0,sdiff=10){
  equalP <- NULL
  unequalP <- NULL
  
  for(i in 1:1000){
    x1 <- rnorm(n1,100,5)
    x2 <- rnorm(n2,100+mudiff,5+sdiff)
    equalP[i] <- t.test(x1,x2,var.equal=TRUE)$p.value
    unequalP[i] <- t.test(x1,x2,var.equal=FALSE)$p.value
  }
  
  typeIEqual <- sum(equalP <= 0.05)/1000
  typeIUnequal <- sum(unequalP <= 0.05)/1000
  results <- data.frame(typeIEqual,typeIUnequal)
  
  return(results)
}


#4
ttestpower(30,10,0,10)
ttestpower(30,10,0,0)
ttestpower(30,30,0,10)
ttestpower(30,30,0,0)
ttestpower(10,30,0,10)
ttestpower(10,30,0,0)
ttestpower(10,10,0,10)
ttestpower(10,10,0,0)
ttestpower(100,10,0,10)
ttestpower(100,10,0,0)

#6
powerEqual <- NULL
powerUnequal <- NULL

mudiffseq <- seq(-15,15,.5)
seqlength <- length(mudiffseq)
for(i in 1:seqlength){
  results <- ttestpower(mudiff=mudiffseq[i])
  powerEqual[i] <- results$typeIEqual
  powerUnequal[i] <- results$typeIUnequal
}

mypowerdata <- data.frame(mudiffseq,powerEqual,powerUnequal)
ggplot(mypowerdata,aes(x=mudiffseq)) +
  geom_point(aes(y=powerEqual)) +
  geom_line(aes(y=powerEqual)) +
  geom_point(aes(y=powerUnequal),color="red") +
  geom_line(aes(y=powerUnequal),color="red",linetype=2) +
  labs(title="When the Equal Variance assumption is not met (Satterthwaite is correct test)",
       subtitle="Pooled = Black/Solid, Satterthwaite = Red/Dashed",
       y="Power of Test")


#6 Extension
# Repeat with n1=n2=30
#6
powerEqual <- NULL
powerUnequal <- NULL

mudiffseq <- seq(-15,15,.5)
seqlength <- length(mudiffseq)
for(i in 1:seqlength){
  results <- ttestpower(n1=30,n2=30,mudiff=mudiffseq[i])
  powerEqual[i] <- results$typeIEqual
  powerUnequal[i] <- results$typeIUnequal
}

mypowerdata <- data.frame(mudiffseq,powerEqual,powerUnequal)
ggplot(mypowerdata,aes(x=mudiffseq)) +
  geom_point(aes(y=powerEqual)) +
  geom_line(aes(y=powerEqual)) +
  geom_point(aes(y=powerUnequal),color="red") +
  geom_line(aes(y=powerUnequal),color="red",linetype=2) +
  labs(title="When the Equal Variance assumption is not met (Satterthwaite is correct test)",
       subtitle="Pooled = Black/Solid, Satterthwaite = Red/Dashed",
       y="Power of Test")


# Repeat with n1=n2=10
#6
powerEqual <- NULL
powerUnequal <- NULL

mudiffseq <- seq(-15,15,.5)
seqlength <- length(mudiffseq)
for(i in 1:seqlength){
  results <- ttestpower(n1=10,n2=10,mudiff=mudiffseq[i])
  powerEqual[i] <- results$typeIEqual
  powerUnequal[i] <- results$typeIUnequal
}

mypowerdata <- data.frame(mudiffseq,powerEqual,powerUnequal)
ggplot(mypowerdata,aes(x=mudiffseq)) +
  geom_point(aes(y=powerEqual)) +
  geom_line(aes(y=powerEqual)) +
  geom_point(aes(y=powerUnequal),color="red") +
  geom_line(aes(y=powerUnequal),color="red",linetype=2) +
  labs(title="When the Equal Variance assumption is not met (Satterthwaite is correct test)",
       subtitle="Pooled = Black/Solid, Satterthwaite = Red/Dashed",
       y="Power of Test")


# Repeat with n1=10 and n2=30
#6
powerEqual <- NULL
powerUnequal <- NULL

mudiffseq <- seq(-15,15,.5)
seqlength <- length(mudiffseq)
for(i in 1:seqlength){
  results <- ttestpower(n1=10,n2=30,mudiff=mudiffseq[i])
  powerEqual[i] <- results$typeIEqual
  powerUnequal[i] <- results$typeIUnequal
}

mypowerdata <- data.frame(mudiffseq,powerEqual,powerUnequal)
ggplot(mypowerdata,aes(x=mudiffseq)) +
  geom_point(aes(y=powerEqual)) +
  geom_line(aes(y=powerEqual)) +
  geom_point(aes(y=powerUnequal),color="red") +
  geom_line(aes(y=powerUnequal),color="red",linetype=2) +
  labs(title="When the Equal Variance assumption is not met (Satterthwaite is correct test)",
       subtitle="Pooled = Black/Solid, Satterthwaite = Red/Dashed",
       y="Power of Test")



#7 Repeat #6 with sdiff=0 to make the pooled test the correct test
powerEqual <- NULL
powerUnequal <- NULL

mudiffseq <- seq(-15,15,.5)
seqlength <- length(mudiffseq)
for(i in 1:seqlength){
  results <- ttestpower(mudiff=mudiffseq[i],sdiff=0)
  powerEqual[i] <- results$typeIEqual
  powerUnequal[i] <- results$typeIUnequal
}

mypowerdata <- data.frame(mudiffseq,powerEqual,powerUnequal)
ggplot(mypowerdata,aes(x=mudiffseq)) +
  geom_point(aes(y=powerEqual)) +
  geom_line(aes(y=powerEqual)) +
  geom_point(aes(y=powerUnequal),color="red") +
  geom_line(aes(y=powerUnequal),color="red",linetype=2) +
  labs(title="When the Equal Variance assumption is met (Pooled is correct test)",
       subtitle="Pooled = Black/Solid, Satterthwaite = Red/Dashed",
       y="Power of Test")


#6 Extension
# Repeat with n1=n2=30
powerEqual <- NULL
powerUnequal <- NULL

mudiffseq <- seq(-15,15,.5)
seqlength <- length(mudiffseq)
for(i in 1:seqlength){
  results <- ttestpower(n1=30,n2=30,mudiff=mudiffseq[i],sdiff=0)
  powerEqual[i] <- results$typeIEqual
  powerUnequal[i] <- results$typeIUnequal
}

mypowerdata <- data.frame(mudiffseq,powerEqual,powerUnequal)
ggplot(mypowerdata,aes(x=mudiffseq)) +
  geom_point(aes(y=powerEqual)) +
  geom_line(aes(y=powerEqual)) +
  geom_point(aes(y=powerUnequal),color="red") +
  geom_line(aes(y=powerUnequal),color="red",linetype=2) +
  labs(title="When the Equal Variance assumption is met (Pooled is correct test)",
       subtitle="Pooled = Black/Solid, Satterthwaite = Red/Dashed",
       y="Power of Test",
       caption="n1=30 & n2=30")

# Repeat with n1=n2=10
powerEqual <- NULL
powerUnequal <- NULL

mudiffseq <- seq(-15,15,.5)
seqlength <- length(mudiffseq)
for(i in 1:seqlength){
  results <- ttestpower(n1=10,n2=10,mudiff=mudiffseq[i],sdiff=0)
  powerEqual[i] <- results$typeIEqual
  powerUnequal[i] <- results$typeIUnequal
}

mypowerdata <- data.frame(mudiffseq,powerEqual,powerUnequal)
ggplot(mypowerdata,aes(x=mudiffseq)) +
  geom_point(aes(y=powerEqual)) +
  geom_line(aes(y=powerEqual)) +
  geom_point(aes(y=powerUnequal),color="red") +
  geom_line(aes(y=powerUnequal),color="red",linetype=2) +
  labs(title="When the Equal Variance assumption is met (Pooled is correct test)",
       subtitle="Pooled = Black/Solid, Satterthwaite = Red/Dashed",
       y="Power of Test",
       caption="n1=10 & n2=10")

# Repeat with n1=n2=10
powerEqual <- NULL
powerUnequal <- NULL

mudiffseq <- seq(-15,15,.5)
seqlength <- length(mudiffseq)
for(i in 1:seqlength){
  results <- ttestpower(n1=10,n2=30,mudiff=mudiffseq[i],sdiff=0)
  powerEqual[i] <- results$typeIEqual
  powerUnequal[i] <- results$typeIUnequal
}

mypowerdata <- data.frame(mudiffseq,powerEqual,powerUnequal)
ggplot(mypowerdata,aes(x=mudiffseq)) +
  geom_point(aes(y=powerEqual)) +
  geom_line(aes(y=powerEqual)) +
  geom_point(aes(y=powerUnequal),color="red") +
  geom_line(aes(y=powerUnequal),color="red",linetype=2) +
  labs(title="When the Equal Variance assumption is met (Pooled is correct test)",
       subtitle="Pooled = Black/Solid, Satterthwaite = Red/Dashed",
       y="Power of Test",
       caption="n1=10 & n2=30")




