# Day 32 - 4/21/2025
# Notes 15 - Activity Review
# Notes 16
# Note: See Code to Copy file!

library(tidyverse)

#-------------------------------------------------------

# Notes 15 - Activity 1

#1
x1 <- rnorm(30, 100, 5)
x2 <- rnorm(20, 100, 5)
x3 <- rnorm(45, 100, 5)

#2
library(tidyverse)
mydata <- data.frame(myx = c(x1,x2,x3),mygroup = c(rep("A",30),rep("B",20),rep("C",45)))
ggplot(mydata,aes(x=factor(mygroup),y=myx)) + geom_boxplot()

group_by(mydata,mygroup) %>% summarize(mean(myx),sd(myx))

#5
out <- summary(aov(myx ~ mygroup, data=mydata))
out

#8
summary(aov(myx ~ mygroup, data=mydata))[[1]][4]$`F value`[1]
# Note: You don't want to use the same myx and mygroup each time
# so update the code in your for() loop or replicate()
# so that these values are generated fresh each time!

myf <- NULL
for(i in 1:1000){
  x1 <- rnorm(30, 100, 5)
  x2 <- rnorm(20, 100, 5)
  x3 <- rnorm(45, 100, 5)
  mydata <- data.frame(myx = c(x1,x2,x3),mygroup = c(rep("A",30),rep("B",20),rep("C",45)))
  myf[i] <- summary(aov(myx ~ mygroup, data=mydata))[[1]][4]$`F value`[1]
}

myfdata <- data.frame(myf=myf)
ggplot(myfdata,aes(x=myf)) + geom_histogram(aes(y=after_stat(density)),color="white") +
  stat_function(fun=df,args=list(df1=2,df2=92))

#11
examplef <- out[[1]][4]$`F value`[1]
sum(myf >= examplef)/1000

1-pf(examplef,2,92)



#------------------------------------------------------
# Notes 15 - Activity 1 function!

fsim <- function(n1=30,n2=20,n3=45){
  out.sim <- replicate(1000,summary(aov(c(rnorm(n1, 100, 5),
                                          rnorm(n2, 100, 5),
                                          rnorm(n3, 100, 5)) ~ c(rep("A",n1),rep("B",n2),rep("C",n3))))[[1]][4]$`F value`[1])
  
  
  
  mydata2 <- data.frame(out.sim=out.sim)
  ggplot(mydata2,aes(x=out.sim)) +
    geom_histogram(aes(y=after_stat(density)),color="white") +
    stat_function(fun=df,args=list(3-1,n1+n2+n3-3))
  
}

fsim(10,10,10)
fsim(10,30,20)
fsim(50,50,50)

#------------------------------------------------------
# Notes 15 - Activity 1 function!

# Investigation #1
out <- rf(20,3,56)
mean(out)
sd(out)
hist(out)

#4
xbar <- replicate(10000,mean(rf(20,3,56)))
mydata <- data.frame(xbar)
ggplot(mydata,aes(x=xbar)) + geom_histogram(aes(y=after_stat(density)),
                                            color="white") +
  stat_function(fun=dnorm,args=list(1.037,0.887/sqrt(20)))

mean(xbar)
sd(xbar)
0.887/sqrt(20)


# Investigation #2
out <- rf(100,3,56)
mean(out)
sd(out)
hist(out)

#4
xbar <- replicate(10000,mean(rf(100,3,56)))
mydata <- data.frame(xbar)
ggplot(mydata,aes(x=xbar)) + geom_histogram(aes(y=after_stat(density)),
                                            color="white") +
  stat_function(fun=dnorm,args=list(1.037,0.887/sqrt(100))) + 
  labs(title="n=100")

mean(xbar)
sd(xbar)
0.887/sqrt(100)


# Investigation #3
# Do on your own!

# Investigation #4
out <- rf(7,3,56)
mean(out)
sd(out)
hist(out)

#4
xbar <- replicate(10000,mean(rf(7,3,56)))
mydata <- data.frame(xbar)
ggplot(mydata,aes(x=xbar)) + geom_histogram(aes(y=after_stat(density)),
                                            color="white") +
  stat_function(fun=dnorm,args=list(1.037,0.887/sqrt(7))) + 
  labs(title="n=100")

mean(xbar)
sd(xbar)
0.887/sqrt(7)
