# Notes 17 - Part 2 - Code to Copy

#-------------------------------------------------------------------
# Example

w <- ______________________
ggplot(NULL,aes(sample=w)) +
  stat_qq(distribution=________________) +
  geom_qq_line(distribution=________________)

ggplot(NULL,aes(sample=w)) +
  stat_qq(distribution=qf,
          dparams=list(df1=3,df2=96)) +
  geom_qq_line(distribution=qf,
               dparams=list(df1=3,df2=96))




#-------------------------------------------------------------------
# Notes 17 - Activity 4

x <- runif(30,60,100)
error <- rnorm(30,0,15)
y <- 3*x + 10 + error





#-------------------------------------------------------------------
# Notes 17 - Activity 5

myobj <- rnorm(8000)
myobj2 <- myobj*myobj
mymat <- matrix(myobj2,nrow=1000)





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



#-------------------------------------------------------------------
# Agresti-Coull

x <- ____
n <- ____
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


