# Day 23 - 3/31/2025
# Notes 10 - Activities 3 (Review solutions)

# Notes 11
# Note: See Code to Copy file!

#----------------------------------------------------

#------------------------------------------------------
# Lesson 10 - Activity 1 Part 3

#Type II Error Code:
out <- replicate(5000,t.test(sample(boxhouses$rooms,10,replace=FALSE),mu=4,alternative="two.sided")$p.value)
sum(out >= 0.05)/5000 #Type II Error
sum(out <= 0.05)/5000 #Power of Test

#1
# Answer in notes

#2
TypeIISim <- function(n=10, mualt=4){
  out <- replicate(5000,t.test(sample(boxhouses$rooms,n,replace=FALSE),mu=mualt,alternative="two.sided")$p.value)
  typeII <- sum(out >= 0.05)/5000 #Type II Error
  power <- sum(out <= 0.05)/5000 #Power of Test
  myreturn <- list(power=power, typeII=typeII)
  return(myreturn)
}

#3
#A
TypeIISim(n=15, mualt=5)

#B
TypeIISim(n=5, mualt=5)

#C
# Answer in notes

#D
TypeIISim(n=15, mualt=0)

#E
# Answer in notes

#4
myn <- 5:30
myresultsII <- matrix(NA, nrow=length(myn), ncol=2)

#5
#Note: Depending on how you coded your function, you may need to modify
#the following code.
out2 <- NULL
for(i in myn){
  print(i)
  out2 <- TypeIISim(n=i)
  myresultsII[i - 4, 1] <- out2$power 
  myresultsII[i - 4, 2] <- out2$typeII 
}

#6
TypeIISimResults <- data.frame(myn = myn, 
                               power = myresultsII[,1], 
                               typeII = myresultsII[,2])

#7
library(tidyverse)
ggplot(TypeIISimResults, aes(x=myn, y=power)) +
  geom_point() +
  geom_line() +
  labs(x="Sample Size", y="Power of Test")

#8
#a
myseq <- 0:15
myresultsIIB <- matrix(NA, nrow=length(myseq), ncol=2)

#b
out3 <- NULL
for(j in myseq){
  out3 <- TypeIISim(n = 10, mualt = j + 0.42)
  myresultsIIB[j + 1, 1] <- out3$power
  myresultsIIB[j + 1, 2] <- out3$typeII
}

#9
TypeIISimResultsB <- data.frame(myseq = myseq,
                                power = myresultsIIB[,1],
                                typeII = myresultsIIB[,2])


#10
library(tidyverse)
ggplot(TypeIISimResultsB, aes(x=myseq, y=power)) + 
  geom_point() +
  geom_line() +
  labs(x="Value of mu in Alternative Hypothesis", 
       y="Power of Test", caption="True value of mu is 7.42")




#-----------------------------------------------------------
# Notes 11

# Notes 11 Code to Copy

#--------------------------------------------

library(ggplot2)

coin <- c("Heads", "Tails")
set.seed(6493)
fewtosses <- sample(coin,10000,replace=TRUE)

fewtosses_sum <- cumsum(fewtosses=="Heads")
fewtosses_prop <- fewtosses_sum/1:10000

few <- data.frame(trial=1:10000,propHeads=fewtosses_prop,tosses=fewtosses)
few_graph <- ggplot(few,aes(x=trial,y=propHeads))+
  geom_point()+
  geom_hline(yintercept=0.5,color="red")+
  geom_line(color="purple")

few_graph

