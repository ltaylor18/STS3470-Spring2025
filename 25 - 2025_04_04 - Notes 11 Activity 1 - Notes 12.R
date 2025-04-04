# Day 25 - 4/4/2025
# Go over Notes 11, Activity 1
# Notes 12
# Note: See Code to Copy file!

library(tidyverse)
#----------------------------------------------------

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
min(mean100)
max(mean100)

#---------------------------------------------------
# Notes 12!

pnorm(1)-pnorm(-1)
pnorm(2)-pnorm(-2)
pnorm(3)-pnorm(-3)

pnorm(-3) + (1-pnorm(3))

#5
pnorm(150,100,25) - pnorm(50,100,25)

#6
pnorm(520,500,10) - pnorm(480,500,10)


#8
out8 <- rnorm(10000)
sum(out8 >= -2 & out8 <= 2)/10000

mean(out8)
sd(out8)

#9
# For you to do!

#10
library(tidyverse)
mydata <- data.frame(out8=out8)
ggplot(mydata,aes(x=out8)) + 
  geom_histogram(aes(y=after_stat(density)),color="white") +
  stat_function(fun=dnorm) + labs(title="Data from #8")

#11
# For you to do!


#--------------------------------------------------
# Expected Value!

dice_game1 <- function(){
  roll <- sample(1:7,1)
  out <- if_else(roll %in% c(2,4,6), 1, 0)
  out  
}
results <- replicate(50000,dice_game1())
mean(results)


#--------------------------------------------------------

myfun <- function(x){
  out <- x*dnorm(x)
}

integrate(myfun, -Inf, Inf)

integrate(dnorm,-Inf, Inf)

