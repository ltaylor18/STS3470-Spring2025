# Day 16 - 3/7/2025
# Notes 9
# Note: See Code to Copy file!

#----------------------------------------------------

# Bead box warm-up!

mybox <- c(rep("green",10000), rep("red", 5000), rep("blue", 15000))


trueprop <- sum(mybox == "blue")/length(mybox)
trueprop

mysample <- sample(mybox, 30, replace=F)
mysample

table(mysample)

sum(mysample=="blue")/30

out30 <- replicate(5000, sum(sample(mybox, 30, replace=F)=="blue")/30)
library(tidyverse)
results30 <- data.frame(out30)
ggplot(results30,aes(x=out30,y=after_stat(density)))+
  geom_histogram(color="white",bins=20) + 
  labs(title="n=30")

mean(out30)
min(out30)
max(out30)

out50 <- replicate(5000, sum(sample(mybox, 50, replace=F)=="blue")/50)
results50 <- data.frame(out50)
ggplot(results50,aes(x=out50,y=after_stat(density)))+
  geom_histogram(color="white",bins=25) +
  labs(title="n=50")

mean(out50)
min(out50)
max(out50)


out100 <- replicate(5000, sum(sample(mybox, 100, replace=F)=="blue")/100)
results100 <- data.frame(out100)
ggplot(results100,aes(x=out100,y=after_stat(density)))+
  geom_histogram(color="white",bins=33) +
  labs(title="n=100")

mean(out100)
min(out100)
max(out100)



#--------------------------------------------------------
# Notes 9


my_mat <- matrix(1:15,nrow=3)

apply(my_mat, 2, mean)
apply(my_mat, 1, max)
apply(my_mat, 2, sum)
apply(my_mat, 1, sd)

apply(mtcars, 2, mean)

apply(randu, 2, t.test, mu=0.5)

#---------------------------------------------------
# for() loops

#Example 1
for(i in 1:5) {print(i)}

#Example 2
for(index in seq(0,10,2)) {print(index)}

#Example 3
for(j in c(1,12,50)){
  print(j)
  print(2*j)
}
