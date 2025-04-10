rm(list=ls())
## Homework #5 - Dr. Taylor
## Solutions

library(tidyverse)
library(readxl)

set.seed(2320)

##1
for(i in 1:5){
  for(j in 1:i){
    print(i)
  }
}

## Prints out the numbers 1 one time, the number 2 two times, etc.

#----------------------------------------------------------

##2
##2a
dice_sim <- function(dicesides, numrolls){
  dice <- 1:dicesides
  myout <- replicate(5000,length(unique(sample(dice,numrolls,replace=TRUE)))==1) 
  prop <- sum(myout)/5000
  prop
}

##2b
dice_sim(dicesides=10,numrolls = 4)

##2c
numdice <- 1:10
yahtzee <- NULL
for(i in numdice){
  yahtzee[i] <- dice_sim(dicesides=4, numrolls=i)
}

##2d
dicedata <- data.frame(numdice,yahtzee)
ggplot(dicedata,aes(x=numdice, y=yahtzee)) +
  geom_point() +
  labs(title="Empirical Probability of Yahtzee for four-sided dice",
       subtitle="Based on the number of dice rolled")
## The more dice we roll, the smaller the probability of rolling
## Yahtzee in one roll is. This is expected as it is more unlikely
## to roll Yahtzee the more dice you use!

#----------------------------------------------------------

##3
myrand <- rnorm(10000, 100, 10)

##4
sum(myrand <= 93.255)/10000
## Approximately 25% of values are less than or equal to 93.255
## So it is approximately the 25th percentile

##5
qnorm(0.25, 100, 10)
## This is the number I had you use in #4!

##6
pnorm(93.255, 100, 10)
## Since this is the 25th percentile, 25% of the curve is to the left!

##7
dnorm(93.255, 100, 10)
## This is the height of the density curvey for the Normal
## distribution with a mean of 100 and a standard deviation
## of 10. It is not the probability as the probability of any
## single value is defined to be 0.

##8
## You could label the following:
## Find the value 93.255 on the x-axis and shade the area to the
## left as 25% and label it as the first quantile.
## You should put a dot on the curve at 93.255 and identify
## the ordered pair for that point as (93.255, 0.0317)
## You could label the area to the right of 93.255 as 75%.

#----------------------------------------------------------

##9
zscores <- (myrand-100)/10

##10
mean(zscores)
sd(zscores)
## mean is close to 0 and standard deviation is close to 1.

##11
zdata <- data.frame(zscores)
ggplot(zdata,aes(x=zscores)) +
  geom_histogram(aes(y=after_stat(density)), color="white") +
  stat_function(fun=dnorm)
## This curve fits the densities of the z-scores incredibly well!
## Based on this example, it is plausible that z-scores follow
## a Standard Normal Distribution

#----------------------------------------------------------

bridges <- read_excel("~/STS 347/0_Spring 2025/Old Data/Lesson 3 - StatewideBridges.xls")

##12

bridgessim <- function(myn=30, munull=36, myalpha=0.05){
outtypeIIc <- replicate(1000, t.test(2011-sample(bridges$YEARBUILT,myn,replace=FALSE),mu=munull,alternative="two.sided")$p.value)
proptypeII <- sum(outtypeIIc > myalpha)/1000
proptypeII
}

##13
set.seed(2320)
bridgessim(10,30,0.05)
## 79.8% of samples lead us to make a Type II Error 
## when n= 10 and Ha: mu != 30 and alpha=0.05.

##14
truemean <- 2011-mean(bridges$YEARBUILT)
set.seed(6493)
1-bridgessim(30,truemean,0.05)
##This proportion is very close to 0.05!

##15
myseq <- 8:68
mytypeII <- NULL
for(i in myseq){
  mytypeII[i-7] <- bridgessim(30,i,0.05)
}

##15a
## We can't create objects inside a for() loop, so any objects
## we plan to "fill in" should exist before the loop. It can
## be helpful to set these objects up as empty objects so that
## the loop will define the dimensions.

##15b
## Since the object we are using to index starts at a value of 8
## and we want to store into the first element of the mytypeII 
## object, we need to subtract 7 to translate the index values
## back to a sequence from 1 to 61.

##15c
mysim <- data.frame(myseq,mytypeII)
ggplot(mysim,aes(x=myseq,y=mytypeII)) + geom_line() + geom_point() +
  labs(title="Proportion of Type II Errors vs. Values of Mu Being Tested",
       subtitle="n=30")

##15d
## When we are really close to the true value of mu (around 38.095)
## we are far more likely to make a mistake of not concluding a difference.
## The further away we move from the true mean, the less likely
## we are to incorrectly fail to detect a difference. In other
## words, bigger differences between the true mean and the value
## we are testing are much more easier to correctly detect.

##16
myseq <- 8:68
mytypeIIb <- NULL
for(i in myseq){
  mytypeIIb[i-7] <- bridgessim(10,i,0.05)
}

mysimb <- data.frame(myseq,mytypeIIb)
ggplot(mysim,aes(x=myseq,y=mytypeIIb)) + geom_line() + geom_point() +
  labs(title="Proportion of Type II Errors vs. Values of Mu Being Tested",
       subtitle="n=10")
##For smaller sample sizes, the "hump" of the graph is much wider
##than when we used a sample of size 30. This suggests that with
##smaller sample sizes that it is much harder to detect smaller
##differences as the proportion of Type II Errors stays higher
##longer as we move away from the true mean of 38.095

##17
ggplot(mysim,aes(x=myseq,y=1-mytypeIIb)) + geom_line() + geom_point() +
  labs(title="Power of Test vs. Values of Mu Being Tested",
       subtitle="n=10")
##As the difference between the true value of the mean and the value
##of mu we are tested increases, the probability of detecing the 
##difference increases, approaching 1 in each direction!

##18
##18a
myseq <- seq(10,150,10)
mypower <- NULL
for(i in myseq){
  mypower[i/10] <- 1-bridgessim(i,truemean,0.05)
}

##18b
plot(myseq,mypower)
mean(mypower)
## On average the probability of detecting a difference when
## there isn't one is 0.05 (the level of significance)!

##19
myseq <- seq(10,150,10)
mypower <- NULL
for(i in myseq){
  mypower[i/10] <- 1-bridgessim(i,45,0.05)
}

plot(myseq,mypower)
## As we increase our sample size, we are more likely to correctly
## detect a difference using our sample when testing Ha: mu=45 
## (which we know to be false).

##20
##power is an argument that takes on 1 if you want to calculate power
##and any other value to calculate type II error
biggersim <- function(n,munull,alpha,power=1){
  if(power == 1){
   temp <- 1-bridgessim(n,munull,alpha) 
  } else{
   temp <- bridgessim(n,munull,alpha)
  }
  temp
}

##21
set.seed(2320)
biggersim(10, 30, 0.05, 0)
set.seed(2320)
bridgessim(10,30,0.05)
## Both yield 0.798

##22
set.seed(2320)
biggersim(10, 30, 0.05, 1)
## Yields 1-0.798 = 0.202
