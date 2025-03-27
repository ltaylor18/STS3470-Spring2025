# Notes 13 Code to Copy

#-------------------------------------------------------------------

#Notes 13 Extension
#Central Limit Theorem

library(tidyverse)
library(patchwork)

set.seed(2320)
x.axis <- seq(0,200)
y.pop <- dnorm(x.axis,100,25)

A <- ggplot(NULL,aes(x=x.axis,y=y.pop)) +
  geom_line()+
  labs(title="Population, Normal(100,25)")
A

#samples of size 5
mean.5 <- replicate(5000,mean(rnorm(5,100,25)))
out.5 <- data.frame(mean.5=mean.5)
B <- ggplot(out.5,aes(x=mean.5))+
  geom_histogram(aes(y=after_stat(density)), color="white", bins=50) +     
  labs(y="density", title="Mean of samples of size 5")+
  stat_function(fun=dnorm, args=list(mean=100,sd=25/sqrt(5)), linewidth=1.5, color="red")+
  xlim(c(0,200))
B

#samples of size 20
mean.20 <- replicate(5000,mean(rnorm(20,100,25)))
out.20 <- data.frame(mean.20=mean.20)
C <- ggplot(out.20,aes(x=mean.20))+
  geom_histogram(aes(y=after_stat(density)), color="white", bins=75) +
  labs(title="Mean of samples of size 20")+
  stat_function(fun=dnorm, args=list(mean=100,sd=25/sqrt(20)), linewidth=1.5, color="red")+
  xlim(c(0,200))
C

#samples of size 100
mean.100 <- replicate(5000,mean(rnorm(100,100,25)))
out.100 <- data.frame(mean.100=mean.100)
D <- ggplot(out.100,aes(x=mean.100))+
  geom_histogram(aes(y=after_stat(density)), color="white",bins=100) +
  labs(title="Mean of samples of size 100")+
  stat_function(fun=dnorm, args=list(mean=100,sd=25/sqrt(100)), linewidth=1.5, color="red")+
  xlim(c(0,200))
D

A/B/C/D


#-------------------------------------------------------------------
# Investigation #2


library(patchwork)
library(tidyverse)

mymean <- ____ #Fill in the blank!
mysd <- ____ #Fill in the blank!

x <- rnorm(5000,mymean,mysd)
xbar <- apply(matrix(x,nrow=1000),1,mean)

myxdata <- data.frame(xbar=xbar)
xbarplot <- ggplot(myxdata,aes(x=xbar))+
  geom_histogram(aes(y=after_stat(density)),
                 color="white",
                 bins=30)+
  stat_function(fun=dnorm,
                args=list(mean=mymean,sd=mysd/sqrt(5)),
                size=1.25)+
  labs(title="Histogram of X-Bar Values")
xbarplot
#Check: length(xbar)
z <- (xbar-mymean)/(mysd/sqrt(5))
myzdata <- data.frame(z=z)
zplot <- ggplot(myzdata,aes(x=z)) + 
  geom_histogram(aes(y=after_stat(density)),
                 color="white",
                 bins=30) + 
  stat_function(fun=dnorm,
                args=list(mean=0,sd=1),
                size=1.25)+
  labs(title="Histogram of Z")
zplot

xbarplot/zplot



#-------------------------------------------------------------------
# Soda Example

bottles <- c(12.01781, 12.02722, 11.95322, 12.04573, 11.97467,
             11.97981, 11.96861, 12.06810,  12.03797)

