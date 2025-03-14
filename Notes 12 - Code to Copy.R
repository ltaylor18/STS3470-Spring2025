# Notes 12 - Code to Copy

#10
library(tidyverse)
mydata <- data.frame(____)
ggplot(mydata,aes(x=____)) + 
  geom_histogram(aes(y=after_stat(density)),color="white") +
  stat_function(fun=dnorm) + labs(title="Data from #8")

#11
mydata2 <- data.frame(____)
ggplot(mydata2,aes(x=____)) + 
  geom_histogram(aes(y=after_stat(density)),color="white") +
  stat_function(fun=dnorm,args=list(mean=____,sd=____)) + 
  labs(title="Data from #5/9a")

mydata3 <- data.frame(____)
ggplot(mydata3,aes(x=____)) + 
  geom_histogram(aes(y=after_stat(density)),color="white") +
  stat_function(fun=dnorm,args=list(mean=____,sd=____)) + 
  labs(title="Data from #6/9b")

#--------------------------------------------------------

dice_game1 <- function(){
  roll <- sample(_____________,1)
  out <- if_else(roll %in% c(2,4,6), _____, _______)
  out  
}
results <- replicate(5000,dice_game1())
mean(results)

#--------------------------------------------------------

myfun <- function(x){
  out <- x*dnorm(x)
}

integrate(myfun, -Inf, Inf)

integrate(dnorm,-Inf, Inf)

#--------------------------------------------------------

set.seed(2320)
x <- rnorm(1000, 100, 5)
mean(x)
sd(x)
trans.x <- 5*x
mean(trans.x)
sd(trans.x)

#5
mysim <- function(mymean=100,mysd=5,c1=1,c2=0){
  x <- rnorm(1000, mymean, mysd)
  print(paste("mean of X is:", mean(x)))
  trans.x <- c1*x + c2
  print(paste("mean of", c1,"*X + ",c2," is:",mean(trans.x)))
}


#--------------------------------------------------------


print("Stop scrolling!")
























#--------------------------------------------------------


print("Stop scrolling!")




















#--------------------------------------------------------

mysim2 <- function(mymean=100,mysd=5,c1=1,c2=0){
  x <- rnorm(1000, mymean, mysd)
  print(paste("mean of X is:", mean(x)))
  print(paste("sd of X is:",sd(x)))
  print(paste("var of X is:",var(x)))
  trans.x <- c1*x + c2
  print(paste("mean of", c1,"*X + ",c2," is:",mean(trans.x)))
  print(paste("sd of", c1,"*X + ",c2," is:",sd(trans.x)))
  print(paste("var of", c1,"*X + ",c2," is:",var(trans.x)))
}