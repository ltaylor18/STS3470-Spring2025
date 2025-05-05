# Day 38 - 5/5/2025
# Notes 17
# Note: See Code to Copy file!

library(tidyverse)

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

#5
TypeIW <- NULL
TypeIAC <- NULL
propseq <- seq(0,1,.01)
seqlen <- length(propseq)

for(j in 1:seqlen){
  out <- WaldvsAC(n=10,truep=propseq[j])
  TypeIW[j] <- 1-out$propW
  TypeIAC[j] <- 1-out$propAC
}

mytypeI <- data.frame(TypeIW, TypeIAC, propseq)
ggplot(mytypeI,aes(x=propseq)) +
  geom_line(aes(y=TypeIAC)) +
  geom_line(aes(y=TypeIW),color="red",linetype=2) +
  labs(title="Type I Error Rate for Wald vs. Agresti-Coull Intervals (n=10)",
       subtitle="Wald = Red/Dashed, Agresti-Coull = Black/Solid",
       x="True Proportion",
       y="Type I Error Rate")

mean(mytypeI$TypeIAC)
mean(mytypeI$TypeIW)


#8
TypeIW <- NULL
TypeIAC <- NULL
propseq <- seq(0,1,.01)
seqlen <- length(propseq)

for(j in 1:seqlen){
  out <- WaldvsAC(n=30,truep=propseq[j])
  TypeIW[j] <- 1-out$propW
  TypeIAC[j] <- 1-out$propAC
}

mytypeI <- data.frame(TypeIW, TypeIAC, propseq)
ggplot(mytypeI,aes(x=propseq)) +
  geom_line(aes(y=TypeIAC)) +
  geom_line(aes(y=TypeIW),color="red",linetype=2) +
  labs(title="Type I Error Rate for Wald vs. Agresti-Coull Intervals (n=30)",
       subtitle="Wald = Red/Dashed, Agresti-Coull = Black/Solid",
       x="True Proportion",
       y="Type I Error Rate")

mean(mytypeI$TypeIAC)
mean(mytypeI$TypeIW)
