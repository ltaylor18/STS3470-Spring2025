rm(list=ls())
# Solutions - Homework #7
set.seed(2320)

library(tidyverse)

#---------------------------------------------------

out <- replicate(10000,t.test(rexp(30,1/5))$conf.int)
sum(out[1,] <= 1/(1/5) & out[2,] >= 1/(1/5))/10000

## 1
badassump <- function(n=30,myrate=1/5){
  out <- replicate(10000,t.test(rexp(n,myrate))$conf.int)
  prop <- sum(out[1,] <= 1/(myrate) & out[2,] >= 1/(myrate))/10000
  return(prop)
}

## 2
badassump() #around 93%
badassump(myrate=1/10) #around 93%
badassump(n=10) #around 90%
badassump(n=100, myrate=1/4) #close to 95%

## 3
# When the sample size is smaller, our capture rate is much 
# lower than 95%.
# Even for "large" sample sizes of 30, the capture rate
# is around 93%, a little short of 95%.
# The normality assumption seems most critical when 
# sample size is small.

## 4
badassump2 <- function(n=30,mydf=5){
  out <- replicate(10000,t.test(rchisq(n,mydf))$conf.int)
  prop <- sum(out[1,] <= mydf & out[2,] >= mydf)/10000
  return(prop)
}

## 5
badassump2() #close to 94%
badassump2(n=10) #close to 93%
badassump2(n=10, mydf=10) #close to 94%
badassump2(n=100) #about 95%
badassump2(n=10, mydf=3) #close to 91%

## 6
# The capture rates are all very close to 95%, just slightly
# too low. There does not seem to be a major impact in 
# this scenario of violating the normaly assumption when
# the degrees of freedom are moderately large. 
# The biggest issues is small sample size and low 
# degrees of freedom (this is when Chi-squared is most skewed)
# The normality assumption seems most critical when 
# sample size is small.


#-----------------------------------------------------------

type1fun <- function(n=30,myrate=1/5,alpha=0.05){
  out.norm <- replicate(1000,t.test(rnorm(n,1/myrate,sd=myrate),mu=1/myrate)$p.value)
  type1prop.N <- sum(out.norm < alpha)/1000
  
  out.viol <- replicate(1000,t.test(rexp(n,myrate),mu=1/myrate)$p.value)
  type1prop.V <- sum(out.viol < alpha)/1000
  
  type1s <- c(type1prop.N,type1prop.V)
  return(type1s)
}

## 7
nseq <- seq(10,100,10)
type1seq.N <- NULL
type1seq.V <- NULL
for(i in 1:length(nseq)){
  type1s <- type1fun(n=nseq[i])
  type1seq.N[i] <- type1s[1]
  type1seq.V[i] <- type1s[2]
}

myresults <- data.frame(nseq,type1seq.N,type1seq.V)
myresults

# When the normality assumption is met (type1seq.N), the
# Type I Error rate remains close to 0.05 (slightly above
# for small sample sizes).
# When the assumption is violated (type1seq.V), the simulated
# Type I Error rate is larger than it should be for small 
# sample sizes like n=10 and n=20, but as sample size increases
# it gets close to 0.05, the desired rate.


#8
nseq <- seq(10,100,10)
type1seq.N <- NULL
type1seq.V <- NULL
for(i in 1:length(nseq)){
  type1s <- type1fun(n=nseq[i], alpha=0.10) #update here!
  type1seq.N[i] <- type1s[1]
  type1seq.V[i] <- type1s[2]
}

myresults <- data.frame(nseq,type1seq.N,type1seq.V)
myresults

# We see a similar pattern that the Type I Error rate remains
# close to 0.10 (alpha) when the assumption is not violated
# and above 0.10 (more so in the beginning) when the normality
# assumption is violated.

## 9
myratesseq <- seq(0.25,3,0.25) #create sequence
type1seq.N <- NULL
type1seq.V <- NULL
for(i in 1:length(myratesseq)){
  type1s <- type1fun(n=20, myrate=myratesseq[i]) #update here n, myrate
  type1seq.N[i] <- type1s[1]
  type1seq.V[i] <- type1s[2]
}

myresults <- data.frame(myratesseq,type1seq.N,type1seq.V)
myresults

# When n=20 and the normality assumption is not violated, the
# Type I Error rate stays around 0.05. However, when the
# normality assumption is violated, it hovers above 0.05.

## 10
powerfun <- function(n=30,myrate=1/5,alpha=0.05,mymu=5){
  out.norm <- replicate(1000,t.test(rnorm(n,1/myrate,sd=myrate),mu=mymu)$p.value)
  power.N <- sum(out.norm < alpha)/1000
  
  out.viol <- replicate(1000,t.test(rexp(n,myrate),mu=mymu)$p.value)
  power.V <- sum(out.viol < alpha)/1000
  
  powers <- c(power.N,power.V)
  return(powers)
}

mymuseq <- seq(4.8,5.2,.025)
powerseq.N <- NULL
powerseq.V <- NULL
for(i in 1:length(mymuseq)){
  powers <- powerfun(mymu=mymuseq[i])
  powerseq.N[i] <- powers[1]
  powerseq.V[i] <- powers[2]
}

mypower <- data.frame(mymuseq,powerseq.N,powerseq.V)
mypower

## 11
ggplot(mypower,aes(x=mymuseq,y=powerseq.N)) +
  geom_point() +
  geom_line() +
  geom_point(aes(y=powerseq.V),color="red") +
  geom_line(aes(y=powerseq.V),color="red") +
  labs(x="Value of Mu being Tested",
       y="Power",
       title="One-sample T-test, n=30",
       subtitle="With (Red) and without (Black) Normality Assumption Violation")


# When the Normality Assumption is met (black line), the
# power of the test decreases as we approach the true value of
# mu = 5. As we test values further away from 5, the probability of
# detecting the difference increases.
# When the Normality assumption is violated, the power remains
# around 0.05, even when the difference is "more real"


## 12 (repeating 10 & 11 with n=100)
mymuseq <- seq(4.8,5.2,.025)
powerseq.N <- NULL
powerseq.V <- NULL
for(i in 1:length(mymuseq)){
  powers <- powerfun(mymu=mymuseq[i],n=100)
  powerseq.N[i] <- powers[1]
  powerseq.V[i] <- powers[2]
}

mypower <- data.frame(mymuseq,powerseq.N,powerseq.V)
mypower

ggplot(mypower,aes(x=mymuseq,y=powerseq.N)) +
  geom_point() +
  geom_line() +
  geom_point(aes(y=powerseq.V),color="red") +
  geom_line(aes(y=powerseq.V),color="red") +
  labs(x="Value of Mu being Tested",
       y="Power",
       title="One-sample T-test, n=30",
       subtitle="With (Red) and without (Black) Normality Assumption Violation")


# We see the same patterns, but the black power curve
# (when the normality assumption is met) is steeper.
# That is, we reach higher powers faster for smaller differences
# between the true value of mu and the values of mu
# we are testing.

## 13
# There are some scenarios where violations of the normal
# assumption does not matter. 
# There are also some scenarios where violations of the normal
# assumption really impact results.
# Be safe - check your assumptions!

#-----------------------------------------------------------
## 14
x1 <- rnorm(30,100,9)
x2 <- rnorm(30,100,25)


## 15
# True - since we used rnorm(), we know the normal assumption is met.
# True - mu1 = mu2 = 100, so mu1-mu2=0!
# True - since H0 is true, we should get large p-values.
# True - The population variances are 9^2 = 81 and 25^2 = 625

## 16
set.seed(2320)
t.test(x1, x2, var.equal = TRUE) #pooled
t.test(x1, x2, var.equal = FALSE) #Satterthwaite

# The p-values are 0.088 and 0.091, so we come to the same
# conclusions for each test with this data. It appears
# that perhaps it doesn't matter which test we did here!

## 17
mypvalP <- NULL
mypvalS <- NULL
for(i in 1:1000){
  x1 <- rnorm(30,100,9)
  x2 <- rnorm(30,100,25)
  mypvalP[i] <- t.test(x1, x2, var.equal = TRUE)$p.value #pooled
  mypvalS[i] <- t.test(x1, x2, var.equal = FALSE)$p.value #Satterthwaite
}
sum(mypvalP < 0.05)/1000
sum(mypvalS < 0.05)/1000

# It doesn't seemt hat it matters since the type I error
# rates were nearly identical (0.056 and 0.055)!

## 18
ttestsim <- function(n1=30,n2=30){
  mypvalP <- NULL
  mypvalS <- NULL
  for(i in 1:1000){
    x1 <- rnorm(n1,100,9)
    x2 <- rnorm(n2,100,25)
    mypvalP[i] <- t.test(x1, x2, var.equal = TRUE)$p.value #pooled
    mypvalS[i] <- t.test(x1, x2, var.equal = FALSE)$p.value #Satterthwaite
  }
  type1P <- sum(mypvalP < 0.05)/1000
  type1S <- sum(mypvalS < 0.05)/1000
  type1 <- data.frame(type1P,type1S)
  type1
}

## 19
ttestsim() #Pooled = 0.049 and Satterthwaite = 0.043
ttestsim(50,50) #Pooled = 0.059 and Satterthwaite = 0.057
ttestsim(10,50) #Pooled = 0.001 and Satterthwaite = 0.043
ttestsim(50,10) #Pooled = 0.287 and Satterthwaite = 0.051
ttestsim(30,50) #Pooled = 0.023 and Satterthwaite = 0.056
ttestsim(50,30) #Pooled = 0.118 and Satterthwaite = 0.054

## 20
# When the sample sizes are equal, both tests appear to have
# very similar Type I error rates that are close to the nominal
# rate of 0.05.
# When the larger sample size is associated with the population
# that has a smaller standard deviation, the Type I error rate
# for the Pooled test can be way above 0.05, but the Satterthwaite
# test still maintains 0.05.
# Overall, the Satterthwaite method maintains the Type I error rate
# and the Pooled test sometimes works well, sometimes exceeds
# the 0.05 expectations, and sometimes does awful!

