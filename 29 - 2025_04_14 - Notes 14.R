# Day 29 - 4/14/2025
# Notes 14
# Note: See Code to Copy file!

library(tidyverse)
#----------------------------------------------------

#------------------------------------------------
#1

library(tidyverse)
library(patchwork)
set.seed(2024)
mydata <- replicate(10000,rnorm(9,100,5))
#Check: What does mydata look like?
dim(mydata)
mydata[,1:5]

samp.means <- apply(mydata,2,mean)
#Check: length(samp.means)

samp.sd <- apply(mydata,2,sd)
#Check: length(samp.sd)

mydata[,1]
mean(mydata[,1])
samp.means[1]



#2
zscores <- (samp.means - 100)/(5/sqrt(9))
tscores <- (samp.means - 100)/(samp.sd/sqrt(9))

#3
mydata.df <- data.frame(samp.means=samp.means,
                        samp.sd=samp.sd,
                        zscores=zscores,
                        tscores=tscores)


#4
A <- ggplot(mydata.df,aes(x=samp.means)) + 
  geom_histogram(aes(y=after_stat(density)),
                 color="white",bins=30) +
  stat_function(fun=dnorm,args=list(mean=100,sd=5/sqrt(9)))
A #Beautiful CLT! :)

#5
B <- ggplot(mydata.df,aes(x=zscores)) + 
  geom_histogram(aes(y=after_stat(density)),
                 color="white",bins=30) +
  stat_function(fun=dnorm,args=list(mean=0,sd=1))
B #Beautiful Z-scores! :)

#6
C <- ggplot(mydata.df,aes(x=tscores))+ 
  geom_histogram(aes(y=after_stat(density)),
                 color="white",bins=30) +
  stat_function(fun=dnorm,args=list(mean=0,sd=1),color="red",size=1.5)
C

#7
D <- ggplot(mydata.df,aes(x=tscores))+ 
  geom_histogram(aes(y=after_stat(density)),
                 color="white",bins=30) +
  stat_function(fun=dt,args=list(df=8),color="blue",size=1.5,linetype=2)
D

#8
E <- ggplot(mydata.df,aes(x=tscores))+ 
  geom_histogram(aes(y=after_stat(density)),
                 color="white",bins=30) +
  stat_function(fun=dnorm,args=list(mean=0,sd=1),color="red",size=1.5)+
  stat_function(fun=dt,args=list(df=8),color="blue",size=1.5,linetype=2)
E

# The t-distribution
x <- seq(-3.25,3.25,.01)
y1 <- dt(x,9)                                     #t9
y2 <- dt(x,15)                                    #t15
y3 <- dt(x,48)                                    #t48
t.df <- data.frame(x=x,y1=y1,y2=y2,y3=y3)
ggplot(t.df,aes(x=x,y=y1)) +
  geom_line(color="pink",linetype=1,size=1.25)+
  geom_line(aes(y=y2),color="purple",linetype=2,size=1.25)+
  geom_line(aes(y=y3),color="orange",linetype=3,size=1.25)

#For comparison
x <- seq(-3.25,3.25,.01)
y1 <- dt(x,9)                                     #t9
y2 <- dt(x,999)                                   #t999
y3 <- dnorm(x)                                    #Z
t.df <- data.frame(x=x,y1=y1,y2=y2,y3=y3)
ggplot(t.df,aes(x=x,y=y1)) +
  geom_line(color="black",linetype=1,size=1.25)+
  geom_line(aes(y=y2),color="blue",linetype=2,size=1.25)+
  geom_line(aes(y=y3),color="red",linetype=3,size=1.25) +
  ggtitle("T(9), T(999), and Z Curves")


#---------------------------------------------
# Note

pnorm(-1.7)
pt(-1.7,9)

qnorm(.975)
qt(.975,9)

#Verification of variance
mydf <- 29
myt <- rt(50000,mydf)
var(myt)
truevar <- mydf/(mydf-2)
truevar

