# Notes 17 (Part 1) - Code to Copy

#----------------------------------------------------------------

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


# Repeat with n1=n2=10


# Repeat with n1=10 and n2=30





#-----------------------------------------------------
# Paired t-test, t-distribution simulation

myT <- NULL
for(i in 1:5000){
  x1 <- rnorm(30, 100, 1)
  error <- rnorm(30) #E(error) = 0
  x2 <- x1 + error #So E(x2) = E(x1 + error) = E(x1) + E(error) = 100 + 0 = 100 
  diff <- x1 - x2 #So E(diff) = E(x1 - x2) = E(x1) - E(x2) = 100 - 100 = 0
  #Since E(diff) = 0, the null hypothesis that mu_d = 0 is TRUE!
  
  out <- t.test(diff)
  myT[i] <- out$statistic
}

myTdata <- data.frame(myT)
ggplot(myTdata,aes(x=myT)) +
  geom_histogram(aes(y=after_stat(density)),color="white")+
  stat_function(fun=dt,args=list(df=29))



#-------------------------------------------------------
# Notes 17 - Activity 3

pairedP <- NULL
pairedT <- NULL
pooledP <- NULL
pooledT <- NULL
for(i in 1:1000){
  
  x1 <- rnorm(30, 100, 1)
  error <- rnorm(30,0.5) #Note: Ha is true!
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



#2
mydata <- data.frame(test_statistics=c(pairedT,pooledT),
                     p_values=c(pairedP,pooledP),
                     test_used=c(rep("paired",______),rep("pooled",______)))


#3
ggplot(mydata,aes(x=factor(___________________),y=_________________))+
  geom_boxplot() +
  labs(x="Test Used",y="Test Statistic",
       title="Paired t-test vs. Pooled t-test")


#6
ggplot(mydata,aes(x=factor(___________________),y=_________________))+
  geom_boxplot() +
  labs(x="Test Used",y="P-Values",
       title="Paired t-test vs. Pooled t-test")



#7
sum(pairedP <= 0.05)/1000
sum(pooledP <= 0.05)/1000


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


