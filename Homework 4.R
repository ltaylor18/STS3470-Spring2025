rm(list=ls())
## Homework #4 - Dr. Taylor
## Solutions

library(tidyverse)
library(readxl)

bridges <- read_excel("~/STS 347/0_Spring 2025/Old Data/Lesson 3 - StatewideBridges.xls")

##1

truemean <- 2011 - mean(bridges$YEARBUILT)

##2
out <- 2011 - replicate(5000, sample(bridges$YEARBUILT,30, replace=F))

##2a
dim(out)
#The dimensions are 30 rows and 5000 columns. Each column represents
#the ages of 30 randomly selected bridges.

##2b
?apply
mymeans <- apply(out,2,mean)
mysds <- apply(out,2,sd)

length(mymeans) ##length of 5000, one for each sample of 30 obs.
length(mysds) ##length of 5000

##2c
myt <- (mymeans-truemean)/(mysds/sqrt(30))

##2d
mytdata <- data.frame(myt=myt)
ggplot(mytdata, aes(x=myt)) + 
  geom_histogram(aes(y=after_stat(density)),color="white")
##The graph is centered around 0 and has a bell-shape!

##2e
ggplot(mytdata, aes(x=myt)) + 
  geom_histogram(aes(y=after_stat(density)),color="white") +
  labs(title="n=30")+
  stat_function(fun=dt, args=list(df=29))
##This curve really closely matches the pattern in the histogram
##of all of our randomly generated t-scores.

##2f
ggplot(mytdata, aes(x=myt)) + 
  geom_histogram(color="white") +
  labs(title="n=30")+
  stat_function(fun=dt, args=list(df=29), color="red", linewidth=5)
##Answer B. The density curve appears to be one with the x-axis!

##2g
##YES!

##2h
out50 <- 2011 - replicate(5000, sample(bridges$YEARBUILT,50, replace=F))
##Change 30 to 50 in the sample() function!
dim(out50)
#The dimensions are 50 rows and 5000 columns. Each column represents
#the ages of 50 randomly selected bridges.

mymeans50 <- apply(out50,2,mean)
mysds50 <- apply(out50,2,sd)

length(mymeans50) ##length of 5000, one for each sample of 50 obs.
length(mysds50) ##length of 5000

myt50 <- (mymeans50-truemean)/(mysds50/sqrt(50)) ##Change 30 to 50!

mytdata50 <- data.frame(myt50=myt50)
ggplot(mytdata50, aes(x=myt50)) + 
  geom_histogram(aes(y=after_stat(density)),color="white") + 
  labs(title="n=50")
##The graph is centered around 0 and has a bell-shape!

ggplot(mytdata50, aes(x=myt50)) + 
  geom_histogram(aes(y=after_stat(density)),color="white")  + 
  labs(title="n=50") +
  stat_function(fun=dt, args=list(df=49))  ##Change 29 to 49
##This curve really closely matches the pattern in the histogram
##of all of our randomly generated t-scores.

##The t-distribution with 49 degrees of freedom does a good job
##of describing the t-scores now with n=50.


##2i. 
##TRUE

##2j.
out<-replicate(5000,t.test(2011-sample(bridges$YEARBUILT,30,replace=F),
                           mu=truemean)$conf.int)
dim(out)
##The dimensions are 2 rows and 5000 columns. The first row 
##contains the lower bound for the 5000 confidence intervals, and
##the second row contains the upper bound for the 5000 confidence
##intervals.

##2k.
mycounter <- if_else(out[1,] <= truemean & out[2,] >= truemean, 1, 0)
sum(mycounter)/5000
##My proportion is very close to 95%!

##2l.
out90<-replicate(5000,t.test(2011-sample(bridges$YEARBUILT,30,replace=F),
                           mu=truemean,conf.level=0.90)$conf.int)
dim(out90)
mycounter90 <- if_else(out90[1,] <= truemean & out90[2,] >= truemean, 1, 0)
sum(mycounter90)/5000
##My proportion is very close to 90%!

##2m.
##In the initial simulation we generated 5000 samples and calculated
##the 95% confidence interval to estimate mu for each of those samples.
##When we counted how many of those intervals contained mu, the proportion
##was close to 95%, matching the level of confidence.
##When we repeated this for 90% confidence, we found that about 90%
##of our intervals contained mu, matching the level of confidence.
##Level of confidence is about what happens in the long-run if we 
##were to randomly sample from the population over and over again
##which we illustrated in this simulation.



##3
##We obtain a ##large## p-value leading us to conclude that there is 
##insufficient evidence of a difference when in reality 
##the ##alternative## hypothesis is true.

##false negative. the test incorrectly gives us a negative result!

##4
set.seed(2320)
outtypeII <- replicate(10000, t.test(2011-sample(bridges$YEARBUILT,30,replace=FALSE),mu=36,alternative="two.sided")$p.value)
sum(outtypeII > 0.05)/10000
##With the seed, you should have 0.9212 
##92.12% of our replications led us to commit a Type II Error!

##5
outtypeIIb <- replicate(10000, t.test(2011-sample(bridges$YEARBUILT,30,replace=FALSE),mu=30,alternative="two.sided")$p.value)
sum(outtypeIIb > 0.05)/10000
##With the seed, you should have 0.4594
##45.94% of our replications led us to commit a Type II Error!
##The proportion of samples that lead us to commit a Type II Error
##decreases because 30 is much further from the true population mean
##of 38.095. We would anticipate that as we test values further
##from the true mean, the probability that we conclude insufficient
##evidence of a difference when in reality there is a difference
##should decrease.

##6
outtypeIIc <- replicate(10000, t.test(2011-sample(bridges$YEARBUILT,10,replace=FALSE),mu=30,alternative="two.sided")$p.value)
sum(outtypeIIc > 0.05)/10000
##With the seed, you should have 0.8007 
##80.07% of our replications led us to commit a Type II Error!
##There is a sizeable increase in the number of Type II Errors
##when our sample size shrinks and the value of mu being test (30)
##stays the same. It seems plausible that we would anticipate that 
##with smaller sample sizes we would have more Type II Errors. 