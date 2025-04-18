# Day 31 - 4/18/2025
# Notes 14 - Investigation #4 Review
# Notes 15
# Note: See Code to Copy file!

library(tidyverse)
#----------------------------------------------------

#------------------------------------------------------
# Investigation #4 Review

#1
set.seed(2320)
myvar <- replicate(5000,var(rnorm(15,sd=5)))

ggplot(NULL,aes(x=myvar))+geom_histogram(color="white",bins=30)


#2
newvar <- (15-1)*myvar/25
myvardata <- data.frame(newvar=newvar)
ggplot(myvardata,aes(x=newvar))+
  geom_histogram(aes(y=after_stat(density)),color="white",bins=30) +
  stat_function(fun=dchisq,args=list(df=14))


#3
# I chose the Exponential distribution with rate=10.
myn <- 20
truevar <- 1/100 #You will need to calculate the theoretical value!
#Recall, Var(t_df) = df/(df-2)
#Recall, var(F_ndf,ddf) = (2*ddf*ddf*(ndf+ddf-2))/(ndf*(ddf-2)*(ddf-2)*(ddf-4))
#Recall, var(Chi^2 _ df) = 2*df
#Recall, var(Exp_rate) = (1/rate)^2


myvar <- replicate(5000, var(rexp(myn, 10)))
newvar <- (myn-1)*myvar/truevar
myvardata <- data.frame(newvar=newvar)
ggplot(myvardata,aes(x=newvar))+
  geom_histogram(aes(y=after_stat(density)),
                 color="white",bins=30) +
  stat_function(fun=dchisq,args=list(df=myn-1))


#------------------------------------------------------
# Notes 15

#1a
1-pnorm(1.5)
pnorm(1.5,lower.tail=FALSE)


#1b
pnorm(-1.5)

#2
1-pt(1.5,3)
1-pt(1.5,29)
1-pt(1.5,99)
1-pt(1.5,999)
1-pnorm(1.5)

#3
pchisq(10,3) - pchisq(5,3)

#4
qnorm(0.975)
qt(0.975,9)

#5
pt(qt(0.25,9),9)


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

#b
1-pnorm(1.96)
#c
sum(mynewnorm >= 1.96)/10000


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