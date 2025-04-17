# Homework #6 - Dr. Taylor's Solutions
# rm(list=ls())

library(tidyverse)
library(patchwork)
set.seed(2320)

#---------------------------------------------------------

#1

CLTsim <- function(popmean=100,popsd=25,n1=5,n2=20,n3=100){
#Notes 13 p 8 Code

x.axis <- seq(popmean-4*popsd,popmean+4*popsd,.1) #update x-axis
y.pop <- dnorm(x.axis,popmean,popsd) #update mean and sd

A <- ggplot(NULL,aes(x=x.axis,y=y.pop)) +
  geom_line()+
  labs(title=paste("Population, Normal(",popmean,",",popsd,")")) #update title
A

#samples of size n1
mean.n1 <- replicate(5000,mean(rnorm(n1,popmean,popsd))) #update
out.n1 <- data.frame(mean.n1=mean.n1)
B <- ggplot(out.n1,aes(x=mean.n1))+
  geom_histogram(aes(y=after_stat(density)), color="white", bins=50) +     
  labs(y="density", title=paste("Mean of samples of size",n1))+
  stat_function(fun=dnorm, args=list(mean=popmean,sd=popsd/sqrt(n1)), linewidth=1.5, color="red")+
  xlim(c(min(x.axis),max(x.axis)))
B

#samples of size n2
mean.n2 <- replicate(5000,mean(rnorm(n2,popmean,popsd))) #update
out.n2 <- data.frame(mean.n2=mean.n2)
C <- ggplot(out.n2,aes(x=mean.n2))+
  geom_histogram(aes(y=after_stat(density)), color="white", bins=50) +     
  labs(y="density", title=paste("Mean of samples of size",n2))+
  stat_function(fun=dnorm, args=list(mean=popmean,sd=popsd/sqrt(n2)), linewidth=1.5, color="red")+
  xlim(c(min(x.axis),max(x.axis)))
C

#samples of size n3
mean.n3 <- replicate(5000,mean(rnorm(n3,popmean,popsd))) #update
out.n3 <- data.frame(mean.n3=mean.n3)
D <- ggplot(out.n3,aes(x=mean.n3))+
  geom_histogram(aes(y=after_stat(density)), color="white", bins=50) +     
  labs(y="density", title=paste("Mean of samples of size",n3))+
  stat_function(fun=dnorm, args=list(mean=popmean,sd=popsd/sqrt(n3)), linewidth=1.5, color="red")+
  xlim(c(min(x.axis),max(x.axis)))
D

A/B/C/D

}

CLTsim()

#2
CLTsim(0,1,10, 15, 30)

# We repeatedly took samples of specified sizes from a Normal Population with a 
# mean of 0 and a standard deviation of 1. We observe that for each of the sample
# sizes the distribution of the sample means follows a bell-shaped distribution
# as predicted by the CLT and they are centered at 0 (the population mean) also
# as predicted by the CLT. Further, we see that as the sample size increases,
# the spread of the curve decreases as predicted by the CLT.


#------------------------------------------------------------------------
#3a
myrand <- rexp(10000, rate=2)
mean(myrand)
# The mean is about 0.5, which is 1/rate = 1/2!

#3b
myrandb <- rexp(10000, rate=3)
mean(myrandb)
# The mean is about 0.33, which is 1/rate = 1/3!

#3c
myrandc <- rexp(10000, rate=10)
mean(myrandc)
# The mean is about 0.1, which is 1/rate = 1/10!

#3d
myrandd <- rexp(10000, rate=1/7)
mean(myrandd)
# The mean is about 7, which is 1/rate = 1/(1/7) = 7!

#3e
# It appears that the mean of the Exponential distribution is 1/rate or 1/lambda!

#3f
myrandsd <- NULL
myrate <- c(0.1, 0.25, 0.5, 1, 1.5, 2, 10)
for(i in 1:length(myrate)){
  myrandsd[i] <- sd(rexp(10000,rate=myrate[i]))
}

data.frame(myrate,myrandsd)

# As the value of the rate, lambda, increases, the spread of the Exponential
# distribution decreases!

# Recreating myrand
myrand <- rexp(10000, rate=2)

#3g
sum(myrand <= qexp(0.5,2))/length(myrand)
sum(myrand <= qexp(0.3,2))/length(myrand)
sum(myrand <= qexp(0.75,2))/length(myrand)

# qexp calculates the quantile. In the first example, it calculates the median.
# Therefore, when we count the proportion of the randomly generated values that
# are less than the median, we get 50% --- because of the definition of the median.
# Similarly, if we calculate the 30th percentile and the count the proportion 
# of randomly generated values that are less than the 30th percentle, we get 30%!
# The same thing happens for the 75th percentile, cutting off 75% of the randomly
# generated values. 
# This helps illustrate that the rexp() function is generating random data
# that indeed follows the Exponential Distribution.

#3h
pexp(qexp(0.75,2), 2)
pexp(qexp(1, 2), 2)
pexp(qexp(0.2493, 2), 2)

# These examples are identical the previous example, but in the previous example
# we were using simulated data from the distribution. Here we are using the 
# exact values. qexp() calculates the 75th percentile and then the pexp() function
# calculate the proportion of the curve that is to the left of the 75th percentile
# which would be 75% of the curve.
# Similarly 100% of the curve is to the left of the 100th percentile and
# 24.93% of the curve is to the left of the 24.93th percentile!

#3i
x <- seq(0.01, 3, 0.01)
y1 <- dexp(x, 2)
y2 <- dexp(x, 5)
y3 <- dexp(x, 10)
plot(x,y1, typ="l", col="red")
lines(x,y2, col="black")
lines(x,y3, col="purple")

# As the rate increases from 2, 5, 10, we see that the tail of the curve
# is thicker for a greater distance across the x-axis suggesting that the
# distribution has more variability. This supports our findings in #3f
# where we determined that as the rate increases, the spread increases.


#-------------------------------------------------------------------------

#4

CLTsimDist <- function(popmean=100,popsd=25,n1=5,n2=20,n3=100,dist="N"){
  #Notes 13 p 8 Code
  
  if(dist=="N"){
  x.axis <- seq(popmean-4*popsd,popmean+4*popsd,.1) #update x-axis
  y.pop <- dnorm(x.axis,popmean,popsd) #update mean and sd
  
  A <- ggplot(NULL,aes(x=x.axis,y=y.pop)) +
    geom_line()+
    labs(title=paste("Population, Normal(",popmean,",",popsd,")")) #update title
  A
  
  #samples of size n1
  mean.n1 <- replicate(5000,mean(rnorm(n1,popmean,popsd))) #update
  out.n1 <- data.frame(mean.n1=mean.n1)
  B <- ggplot(out.n1,aes(x=mean.n1))+
    geom_histogram(aes(y=after_stat(density)), color="white", bins=50) +     
    labs(y="density", title=paste("Mean of samples of size",n1))+
    stat_function(fun=dnorm, args=list(mean=popmean,sd=popsd/sqrt(n1)), linewidth=1.5, color="red")+
    xlim(c(min(x.axis),max(x.axis)))
  B
  
  #samples of size n2
  mean.n2 <- replicate(5000,mean(rnorm(n2,popmean,popsd))) #update
  out.n2 <- data.frame(mean.n2=mean.n2)
  C <- ggplot(out.n2,aes(x=mean.n2))+
    geom_histogram(aes(y=after_stat(density)), color="white", bins=50) +     
    labs(y="density", title=paste("Mean of samples of size",n2))+
    stat_function(fun=dnorm, args=list(mean=popmean,sd=popsd/sqrt(n2)), linewidth=1.5, color="red")+
    xlim(c(min(x.axis),max(x.axis)))
  C
  
  #samples of size n3
  mean.n3 <- replicate(5000,mean(rnorm(n3,popmean,popsd))) #update
  out.n3 <- data.frame(mean.n3=mean.n3)
  D <- ggplot(out.n3,aes(x=mean.n3))+
    geom_histogram(aes(y=after_stat(density)), color="white", bins=50) +     
    labs(y="density", title=paste("Mean of samples of size",n3))+
    stat_function(fun=dnorm, args=list(mean=popmean,sd=popsd/sqrt(n3)), linewidth=1.5, color="red")+
    xlim(c(min(x.axis),max(x.axis)))
  D
  }
  if(dist == "E"){
    x.axis <- seq(0,3*popmean,.1) #update x-axis
    y.pop <- dexp(x.axis,1/popmean) #update mean and sd
    
    rate = 1/popmean #added this in!
    
    A <- ggplot(NULL,aes(x=x.axis,y=y.pop)) +
      geom_line()+
      labs(title=paste("Population, Exponential(",rate,")")) #update title
    A
    
    #samples of size n1
    mean.n1 <- replicate(5000,mean(rexp(n1,1/popmean))) #update
    out.n1 <- data.frame(mean.n1=mean.n1)
    B <- ggplot(out.n1,aes(x=mean.n1))+
      geom_histogram(aes(y=after_stat(density)), color="white", bins=50) +     
      labs(y="density", title=paste("Mean of samples of size",n1))+
      stat_function(fun=dnorm, args=list(mean=popmean,sd=popmean/sqrt(n1)), linewidth=1.5, color="red")+
      xlim(c(min(x.axis),max(x.axis)))
    B
    
    #samples of size n2
    mean.n2 <- replicate(5000,mean(rexp(n2,1/popmean))) #update
    out.n2 <- data.frame(mean.n2=mean.n2)
    C <- ggplot(out.n2,aes(x=mean.n2))+
      geom_histogram(aes(y=after_stat(density)), color="white", bins=50) +     
      labs(y="density", title=paste("Mean of samples of size",n2))+
      stat_function(fun=dnorm, args=list(mean=popmean,sd=popmean/sqrt(n2)), linewidth=1.5, color="red")+
      xlim(c(min(x.axis),max(x.axis)))
    C
    
    #samples of size n3
    mean.n3 <- replicate(5000,mean(rexp(n3,1/popmean))) #update
    out.n3 <- data.frame(mean.n3=mean.n3)
    D <- ggplot(out.n3,aes(x=mean.n3))+
      geom_histogram(aes(y=after_stat(density)), color="white", bins=50) +     
      labs(y="density", title=paste("Mean of samples of size",n3))+
      stat_function(fun=dnorm, args=list(mean=popmean,sd=popmean/sqrt(n3)), linewidth=1.5, color="red")+
      xlim(c(min(x.axis),max(x.axis)))
    D
    
  }
  
  A/B/C/D
  
}

CLTsimDist(dist="E")

#5
CLTsimDist()

#6
CLTsimDist(popmean=10,n1=10,n2=30,n3=50,dist="E")
#6a
# When the sample size is small (n=10), the graph has a slight skew
# and is slightly off center. For n=30 and n=50, the graphs are quite
# bell-shaped.

#6b
# When the sample size is small (n=10), the cente rof the graph is slightly
# less than 10, but this slight change seems to decrease as we increase our
# sample size. This is due to the fact that the Exponential distribution
# is right-skewed, so we need "larger" n to have the CLT apply.

#6c
# As the sample size increases, the sampling distribution of the sample
# mean becomes less spread out! We can see that the histograms aren't as
# wide.

#--------------------------------------------------------------------------

#7

#7a
# E(3*X + 2*Y + 7) = E(3*X) + E(2*Y) + E(7) = 3*E(X) + 2*E(Y) + 7
#    = 3*2 + 2*10 + 7 = 33
3*2 + 2*10 + 7

#7b
# Var(3*X + 2*Y + 7) = Var(3*X) + Var(2*Y) = 3^2 * Var(X) + 2^2 * Var(Y)
#    = 9*Var(X) + 4*Var(Y) = 9*4 + 4*100 = 436
3*3*2*2 + 2*2*10*10

#7C
# SD = sqrt(VAR) --> SD(3*X + 2*Y + 7) = sqrt(436) = 20.88061
sqrt(3*3*2*2 + 2*2*10*10)

#7d
# Since X and Y are bell-shaped and we are adding two bell-shaped distributions
# together, we might expect the variable W to be bell-shaped.

#7e
X <- rnorm(5000, 2, 2)
Y <- rnorm(5000, 10, 10)
W <- 3*X + 2*Y + 7

mean(W) #Close to 33!
var(W) #Close to 436!
sd(W) #Close to 20.9
hist(W) #Bell-shaped!


#8
X <- rexp(10000, 1/2)
Y <- rexp(10000, 1/10)
W <- 3*X + 2*Y + 7

mean(W) #Close to 33! This matches what the rules of expectation from #7a gave us!
var(W) #Close to 436! This matches what the rules of expectation from #7b gave us!
sd(W) #Close to 20.8 This matches what the rules of expectation from #7c gave us!
hist(W) #Slightly right-skewed! 
# Do you have a hypothesis? Consider the fact that W is a calculation
# based on two right-skewed variables, X and Y!

#------------------------------------------------------------------------

#9a
mymat <- matrix(rnorm(10*10000,98.6, 0.73),nrow=10)

#9b
myt <- (apply(mymat,2,mean) - 98.6)/(apply(mymat,2,sd)/sqrt(10))

#9c
wrong <- 2*pnorm(abs(myt),lower.tail=F)

#9d
sum(wrong <= 0.05)/10000

#9e
correct <- 2*pt(abs(myt),9,lower.tail=F)

#9f
sum(correct <= 0.05)/10000

#9g
# The proportion of tests that incorrectly found evidence of a differnece
# was higher when we used the Standard Normal Distribution. So we would
# have been more likely to detect un-real differences when we used the
# Z distribution instead of the t-distribution. Our Type I Error rate
# is higher than expected.

#9h
wrongdist <- function(n=10){
  mymat <- matrix(rnorm(n*10000,98.6, 0.73),nrow=n)
  myt <- (apply(mymat,2,mean) - 98.6)/(apply(mymat,2,sd)/sqrt(n))

  wrong <- 2*pnorm(abs(myt),lower.tail=F)
  print("Type I Error Rate using Z-distribution")
  prop1 <- sum(wrong <= 0.05)/10000
  print(prop1)

  correct <- 2*pt(abs(myt),n-1,lower.tail=F)
  print("Type I Error Rate using t-distribution")
  prop2 <- sum(correct <= 0.05)/10000
  print(prop2)
  
  return(list=c(wrongprop=prop1,correctprop=prop2))
  
}

#9i
wrongdist()
wrongdist(n=5)
wrongdist(n=7)
wrongdist(n=15)
wrongdist(n=25)
wrongdist(n=99)
wrongdist(n=500)

# For small sample sizes, the Type I Error rate when using the Z
# distribution can greatly exceed the expected rate of 0.05, but the
# t-distribution maintains the error rate of 0.05 regardless of n (because
# it is the correct distribution of the test statistic). However, as
# n increases, using the wrong distribution becomes less concerning
# as the Type I Error rate seems to converge on the correct value 
# of 0.05 for large sample sizes (99 and 500 were really close to 0.05
# and the results of the correct t-distribution.

#9i - BONUS
mysizes <- c(5,7,15,25,99,500)
myresults <- matrix(NA,nrow=2,ncol=length(mysizes))
for(i in 1:length(mysizes)){
  myresults[,i] <- wrongdist(n=mysizes[i])
}
colnames(myresults) <- mysizes
rownames(myresults) <- c("Wrong (Z)","Correct (t)")
myresults
