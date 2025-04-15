# Notes 15 - Code to Copy

#-------------------------------------------------------

# Area under curve

normalDistribution <- data.frame(
  x = seq(-4,4, by = 0.01),
  y = dnorm(seq(-4,4, by = 0.01))
)

lowerValue <- 1
upperValue <- 2

shadeNormal <- rbind(c(lowerValue,0), 
                     subset(normalDistribution, 
                            x > lowerValue & x < upperValue), 
                     c(upperValue,0))

ggplot(normalDistribution, aes(x,y)) +
  geom_line() +
  geom_polygon(data = shadeNormal, 
               aes(x=x, y=y, fill="red")) +
  guides(fill="none") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x = lowerValue, y = 0, 
                   xend = lowerValue, 
                   yend = dnorm(lowerValue))) +
  geom_segment(aes(x = upperValue, y = 0, 
                   xend = upperValue, 
                   yend = dnorm(upperValue)))


#-------------------------------------------------------

# Area under curve

mynewnorm <- rnorm(10000,0,1)
sum(mynewnorm >= 1 & mynewnorm <= 2)/10000


#-------------------------------------------------------

# Activity 1

#1
x1 <- rnorm(30, 100, 5)
x2 <- rnorm(20, 100, 5)
x3 <- rnorm(45, 100, 5)

#2
library(tidyverse)
mydata <- data.frame(myx = c(x1,x2,x3),mygroup = c(rep("A",30),rep("B",20),rep("C",45)))
ggplot(mydata,aes(x=factor(mygroup),y=myx)) + geom_boxplot()

group_by(mydata,mygroup) %>% summarize(mean(myx),sd(myx))


#8
summary(aov(myx ~ mygroup, data=mydata))[[1]][4]$`F value`[1]
# Note: You don't want to use the same myx and mygroup each time
# so update the code in your for() loop or replicate()
# so that these values are generated fresh each time!











# Don't scroll further!
print("Don't scroll further!")







# Don't scroll further!
print("Don't scroll further!")





# Don't scroll further!
print("Don't scroll further!")





# Don't scroll further!
print("Don't scroll further!")





# Don't scroll further!
print("Don't scroll further!")




















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