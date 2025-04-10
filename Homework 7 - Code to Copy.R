# Homework 7 - Code to Copy

out <- replicate(1000,t.test(rexp(30,1/5))$conf.int)
sum(out[1,] <= 1/(1/5) & out[2,] >= 1/(1/5))/1000

#-----------------------------------------------------

type1fun <- function(n=30,myrate=1/5,alpha=0.05){
  out.norm <- replicate(1000,t.test(rnorm(n,1/myrate,sd=myrate),mu=1/myrate)$p.value)
  type1prop.N <- sum(out.norm < alpha)/1000
  
  out.viol <- replicate(1000,t.test(rexp(n,myrate),mu=1/myrate)$p.value)
  type1prop.V <- sum(out.viol < alpha)/1000
  
  type1s <- c(type1prop.N,type1prop.V)
  return(type1s)
}


#7 --------------------------------------------------

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


#11 -------------------------------------------------

ggplot(mypower,aes(x=mymuseq,y=powerseq.N)) +
  geom_point() +
  geom_line() +
  geom_point(aes(y=powerseq.V),color="red") +
  geom_line(aes(y=powerseq.V),color="red") +
  labs(x="Value of Mu being Tested",
       y="Power",
       title="One-sample T-test, n=30",
       subtitle="With (Red) and without (Black) Normality Assumption Violation")


#-----------------------------------------------------

t.test(x1, x2, var.equal = TRUE) #pooled
t.test(x1, x2, var.equal = FALSE) #Satterthwaite


#17 -------------------------------------------------

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

