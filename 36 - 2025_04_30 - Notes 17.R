# Day 36 - 4/30/2025
# Notes 17
# Note: See Code to Copy file!

library(tidyverse)

#-------------------------------------------------------


#-------------------------------------------------------
# Notes 17 - Activity 3

#10
pairedP <- NULL
pairedT <- NULL
pooledP <- NULL
pooledT <- NULL
powerPa <- NULL
powerPo <- NULL

nseq <- seq(10,150,5)
for(j in 1:length(nseq)){
  
  for(i in 1:1000){
    
    x1 <- rnorm(nseq[j], 100, 1)
    error <- rnorm(nseq[j],0.5) #Note: Ha is true!
    x2 <- x1 + error
    diff <- x1 - x2
    
    #Paired t-test is correct!
    out <- t.test(diff)
    pairedP[i] <- out$p.value
    pairedT[i] <- out$statistic
    
    #This test is the wrong choice!
    out2 <- t.test(x1,x2,var.equal=TRUE)
    pooledP[i] <- out2$p.value
    pooledT[i] <- out2$statistic
  }
  powerPa[j] <- sum(pairedP <= 0.05)/1000
  powerPo[j] <- sum(pooledP <= 0.05)/1000  
}

mydata <- data.frame(sample_size=nseq, PairedPower=powerPa, PooledPower=powerPo)
ggplot(mydata,aes(x=sample_size))+ geom_line(aes(y=PairedPower)) +
  geom_line(aes(y=PooledPower),color="red",linetype=2)+
  labs(x="Sample Size",y="Power of Test",
       title="Power of Paired t-test vs. Pooled t-test versus sample size",
       subtitle="Correst test is Paired (Black/Solid).")

#12
pairedP <- NULL
pairedT <- NULL
pooledP <- NULL
pooledT <- NULL
powerPa <- NULL
powerPo <- NULL

nseq <- seq(10,150,5)
for(j in 1:length(nseq)){
  
  for(i in 1:1000){
    
    x1 <- rnorm(nseq[j], 100, 1)
    error <- rnorm(nseq[j],1) #Note: Ha is true!
    x2 <- x1 + error
    diff <- x1 - x2
    
    #Paired t-test is correct!
    out <- t.test(diff)
    pairedP[i] <- out$p.value
    pairedT[i] <- out$statistic
    
    #This test is the wrong choice!
    out2 <- t.test(x1,x2,var.equal=TRUE)
    pooledP[i] <- out2$p.value
    pooledT[i] <- out2$statistic
  }
  powerPa[j] <- sum(pairedP <= 0.05)/1000
  powerPo[j] <- sum(pooledP <= 0.05)/1000  
}

mydata <- data.frame(sample_size=nseq, PairedPower=powerPa, PooledPower=powerPo)
ggplot(mydata,aes(x=sample_size))+ geom_line(aes(y=PairedPower)) +
  geom_line(aes(y=PooledPower),color="red",linetype=2)+
  labs(x="Sample Size",y="Power of Test",
       title="Power of Paired t-test vs. Pooled t-test versus sample size",
       subtitle="Correst test is Paired (Black/Solid).")


# What if the standard deviation of the populations
# were much greater than 1?

#12
pairedP <- NULL
pairedT <- NULL
pooledP <- NULL
pooledT <- NULL
powerPa <- NULL
powerPo <- NULL

nseq <- seq(10,150,5)
for(j in 1:length(nseq)){
  
  for(i in 1:1000){
    
    x1 <- rnorm(nseq[j], 100, 5)
    error <- rnorm(nseq[j],1) #Note: Ha is true!
    x2 <- x1 + error
    diff <- x1 - x2
    
    #Paired t-test is correct!
    out <- t.test(diff)
    pairedP[i] <- out$p.value
    pairedT[i] <- out$statistic
    
    #This test is the wrong choice!
    out2 <- t.test(x1,x2,var.equal=TRUE)
    pooledP[i] <- out2$p.value
    pooledT[i] <- out2$statistic
  }
  powerPa[j] <- sum(pairedP <= 0.05)/1000
  powerPo[j] <- sum(pooledP <= 0.05)/1000  
}

mydata <- data.frame(sample_size=nseq, PairedPower=powerPa, PooledPower=powerPo)
ggplot(mydata,aes(x=sample_size))+ geom_line(aes(y=PairedPower)) +
  geom_line(aes(y=PooledPower),color="red",linetype=2)+
  labs(x="Sample Size",y="Power of Test",
       title="Power of Paired t-test vs. Pooled t-test versus sample size",
       subtitle="Correst test is Paired (Black/Solid).")

#-------------------------------------------------
# Notes 17 - Part 2

w <- rnorm(100,50,5)
ggplot(NULL,aes(sample=w)) +
  stat_qq(distribution=qnorm) +
  geom_qq_line(distribution=qnorm)

# Comparing w to the F 3,96 distribution
ggplot(NULL,aes(sample=w)) +
  stat_qq(distribution=qf,
          dparams=list(df1=3,df2=96)) +
  geom_qq_line(distribution=qf,
               dparams=list(df1=3,df2=96))

#--------------------------------------------------
# Notes 17 - Activity 4

x <- runif(30,60,100)
error <- rnorm(30,0,15)
y <- 3*x + 10 + error

#1
mydata <- data.frame(x,y)
library(tidyverse)
ggplot(mydata,aes(x=x,y=y)) +
  geom_point()

#2
lm(y ~ x)
lm(y ~ x, mydata)
lm(mydata$y ~ mydata$x)

myresiduals <- lm(y~x)$residuals
hist(myresiduals)

#3
ggplot(mydata,aes(x=x,y=y)) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE)


#4
ggplot(NULL,aes(sample=myresiduals)) +
  stat_qq(distribution=qnorm) +
  geom_qq_line(distribution=qnorm)


#5 - Violating the normality assumption
x <- runif(30,60,100)
error <- runif(30,-10,10)
y <- 3*x + 10 + error

#1
mydata <- data.frame(x,y)
ggplot(mydata,aes(x=x,y=y)) +
  geom_point()

#2
lm(y ~ x)

myresiduals <- lm(y~x)$residuals
hist(myresiduals)

#3
ggplot(mydata,aes(x=x,y=y)) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE)

#4
ggplot(NULL,aes(sample=myresiduals)) +
  stat_qq(distribution=qnorm) +
  geom_qq_line(distribution=qnorm)


out <- lm(y ~x)
plot(out)






