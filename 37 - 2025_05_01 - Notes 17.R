# Day 37 - 5/1/2025
# Notes 17
# Note: See Code to Copy file!

library(tidyverse)

#-------------------------------------------------------

#-------------------------------------------------------------------
# Notes 17 - Activity 5

myobj <- rnorm(8000)
myobj2 <- myobj*myobj
mymat <- matrix(myobj2,nrow=1000)

#1
mysums <- apply(mymat,1,sum)
length(mysums)
is.vector(mysums)

#3
mydata <- data.frame(mysums)
ggplot(mydata,aes(x=mysums))+
  geom_histogram(aes(y=after_stat(density)),color="purple") +
  stat_function(fun=dchisq,args=list(df=8))

#4
ggplot(NULL,aes(sample=mysums))+
  stat_qq(distribution=qchisq,
          dparams=list(df=8))+
  geom_qq_line(distribution=qchisq,
          dparams=list(df=8))

#1
ggplot(NULL,aes(sample=mysums))+
  stat_qq(distribution=qchisq,
          dparams=list(df=1))+
  geom_qq_line(distribution=qchisq,
               dparams=list(df=1))

#2
ggplot(NULL,aes(sample=mysums))+
  stat_qq(distribution=qnorm)+
  geom_qq_line(distribution=qnorm)


#-------------------------------------------------------------------
# Wald

phat <- x/n
zscore <- (phat-p)/sqrt(p*(1-p)/n)
pval2 <- 2*pnorm(abs(zscore), lower.tail=FALSE) #two.sided p-value
pvalL <- pnorm(zscore) #left tail
pvalR <- pnorm(zscore, lower.tail=FALSE) #right tail

phat <- x/n
moe <- qnorm(.975)*sqrt(phat*(1-phat)/n)
phat-moe
phat+moe


# Example #2
n <- 30
truep <- 0.94
correct <- NULL

for(i in 1:1000){
  x <- rbinom(n,1,truep)
  phat <- sum(x)/n
  moe <- qnorm(.975)*sqrt(phat*(1-phat)/n)
  correct[i] <- if_else(phat-moe <= truep & phat+moe >= truep,1,0)
}

sum(correct)/1000

# Example #3
n <- 30
truep <- 0.51
correct <- NULL

for(i in 1:1000){
  x <- rbinom(n,1,truep)
  phat <- sum(x)/n
  moe <- qnorm(.975)*sqrt(phat*(1-phat)/n)
  correct[i] <- if_else(phat-moe <= truep & phat+moe >= truep,1,0)
}

sum(correct)/1000


# Example #4
n <- 30
truep <- 0.02
correct <- NULL

for(i in 1:1000){
  x <- rbinom(n,1,truep)
  phat <- sum(x)/n
  moe <- qnorm(.975)*sqrt(phat*(1-phat)/n)
  correct[i] <- if_else(phat-moe <= truep & phat+moe >= truep,1,0)
}

sum(correct)/1000

#-------------------------------------------------
# Example

# Wald
phat <- 47/75
moe <- qnorm(.975)*sqrt(phat*(1-phat)/n)
phat-moe
phat+moe

# Agresti-Coull
x <- 47
n <- 75
zstar <- qnorm(0.975) #or other levels of confidence like 0.95 for 90% and .995 for 99%
phat_ac <- (x + (zstar^2)/2)/(n+(zstar^2))
moe_ac <- zstar*sqrt((phat_ac*(1-phat_ac))/(n+(zstar^2)))

phat_ac - moe_ac
phat_ac + moe_ac


#-------------------------------------------------------------------
# Notes 17 - Activity 6

WaldvsAC <- function(n=30, truep=0.94){
  correctW <- NULL
  correctAC <- NULL
  
  for(i in 1:1000){
    x <- rbinom(n,1,truep)
    phatW <- sum(x)/n
    moeW <- qnorm(.975)*sqrt(phatW*(1-phatW)/n)
    correctW[i] <- if_else(phatW-moeW <= truep & phatW+moeW >= truep,1,0)
    
    zstar <- qnorm(0.975) #or other levels of confidence like 0.95 for 90% and .995 for 99%
    phat_ac <- (sum(x) + (zstar^2)/2)/(n+(zstar^2))
    moe_ac <- zstar*sqrt((phat_ac*(1-phat_ac))/(n+(zstar^2)))
    correctAC[i] <- if_else(phat_ac-moe_ac <= truep & phat_ac+moe_ac >= truep,1,0)
    
  }
  
  propW <- sum(correctW)/1000
  propAC <- sum(correctAC)/1000
  prop_return <- data.frame(propW, propAC)
  return(prop_return)
}

#1
WaldvsAC()

#2
WaldvsAC(n=100,truep=0.94)

#3
WaldvsAC(n=10,truep=0.51)

#4
WaldvsAC(n=50, truep=0.51)
