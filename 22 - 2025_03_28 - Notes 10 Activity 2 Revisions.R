# Revisions on Lesson 10 - Activity 1 Part 2

#------------------------------------------------------
# Lesson 10 - Activity 1 Part 2

#1
TypeISimb <- function(n=10){
  out <- replicate(5000,t.test(rnorm(n,100,10),mu=100,alternative="two.sided")$p.value)
  prop <- sum(out <= 0.05)/5000
  prop
}

#2
TypeISimb(n=15)

#3
set.seed(2320)
TypeISimb(n=15)

#4
set.seed(6493)

#5
myn <- 3:30
myresults <- rep(NA,length(myn))

#6
for(i in myn){
  myresults[i - 2] <- TypeISimb(n = i)
}

#7
# Answer in notes

#8
TypeISimResults <- data.frame(myn=myn, myresults=myresults)
TypeISimResults
mean(TypeISimResults$myresults)

#9
library(tidyverse)
ggplot(TypeISimResults,aes(x=myn,y=myresults)) + 
  geom_point() +
  geom_line() +
  labs(x="Sample Size", y="Type I Error Rate")

mean(TypeISimResults$myresults)

