# Day 24 - 4/2/2025
# Notes 11
# Note: See Code to Copy file!
library(tidyverse)
#----------------------------------------------------

1/sqrt(2*pi)
dnorm(0)
dnorm(1)
dnorm(-1)


pnorm(0)
?pnorm

pnorm(-0.674)
1-pnorm(-.674) #or pnorm(-.674,lower.tail=F)

#------------------------------------------------------
# Notes 11 p. 7

#1
x1 <- seq(0,200,1)
y1 <- dnorm(x1,100,15)
mydata2 <- data.frame(x1=x1,y1=y1)
ggplot(mydata2,aes(x=x1,y=y1)) + geom_line()

#2
mydata2 <- mutate(mydata2,y2=dnorm(x1,90,5))
ggplot(mydata2,aes(x=x1,y=y1)) +
  geom_line(color="purple") +
  geom_line(aes(y=y2),color="green")



#--------------------------------------------------------
# Notes 11 Activity 1

#1
mymean <- 1060
mysd <- 195
myx <- seq(mymean-4*mysd,mymean+4*mysd,1)
myy <- dnorm(myx,mymean,mysd)
mydata3 <- data.frame(myx, myy)
ggplot(mydata3,aes(x=myx, y=myy)) + geom_line()


#3
SATheight <- dnorm(1300, 1060, 195)
SATheight

#Add this layer to your graph!
ggplot(mydata3,aes(x=myx, y=myy)) + geom_line() +
  geom_linerange(x=1300, ymin=0, ymax=SATheight, col="red")

#4
1-pnorm(1300, 1060, 195)

#6
simul_SAT <- rnorm(30, 1060, 195)

#7
mean(simul_SAT)
sd(simul_SAT)

#8
set.seed(2320)
simul_SAT <- rnorm(30, 1060, 195)
mean(simul_SAT)
sd(simul_SAT)

#9
set.seed(6493)

out <- replicate(10000, rnorm(30,1060,195))

#10
dim(out)

mean30 <- apply(out,2,mean)

#14
mean(mean30)

#15
sd(mean30)

#16
mymeandata <- data.frame(mean30)
ggplot(mymeandata,aes(x=mean30)) + 
  geom_histogram(aes(y=after_stat(density)), color="white")
min(mean30)
max(mean30)


#17
ggplot(mymeandata,aes(x=mean30)) + 
  geom_histogram(aes(y=after_stat(density)), color="white") +
  stat_function(fun=dnorm, args=list(mean=1060,35.602))

#18
195/sqrt(30)

#19
out100 <- replicate(10000, rnorm(100,1060,195))
mean100 <- apply(out100,2,mean)
mymean100data <- data.frame(mean100)
ggplot(mymean100data,aes(x=mean100)) + 
  geom_histogram(aes(y=after_stat(density)), color="white") +
  stat_function(fun=dnorm, args=list(mean=1060,195/sqrt(100)))

