# Day 35 - 4/28/2025
# Notes 17
# Note: See Code to Copy file!

library(tidyverse)

#-------------------------------------------------------

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
                     test_used=c(rep("paired",length(pairedT)),rep("pooled",length(pooledT))))


#3
ggplot(mydata,aes(x=factor(test_used),y=test_statistics))+
  geom_boxplot() +
  labs(x="Test Used",y="Test Statistic",
       title="Paired t-test vs. Pooled t-test")


#6
ggplot(mydata,aes(x=factor(test_used),y=p_values))+
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
