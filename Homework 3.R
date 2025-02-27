rm(list=ls())
# Homework #3 - Solutions
library(tidyverse)
library(readxl)

#--------------------------------------------------------
##1 
# You can check your answers with the following:
myresults <- data.frame(x=1:5,y=c("A","B","C","D","E"))

#i.
filter(myresults, x <= 3)
#Pulls off all rows where x is less than or equal to 3,
#which would be the first three rows.
# Output D

#ii.
filter(myresults, x < 3)
#Pulls off all rows where x is less than but not equal to 3,
#which would be the first two rows.
# Output E

#iii.
filter(myresults, x < 3, y = "A")
#This code is missing an "exactly equals"
# == not =

#iv.
filter(myresults, x < 3, y == "A")
#The only row that has an x value less than 3 AND
#where y is exactly equal to A is the first row.
# Output A

#v.
filter(myresults, y == "A")
#Pulls off only the first row where y is exactly
#equal to A.
# Output A

#vi.
filter(myresults, y = A)
#This code is missing the exactly equals == AND
#the quotes around A.

#vii.
filter(myresults, x != 3)
#Pulls off all rows where x is not equal to 3,
#this would be all rows except the third row.
# Output B

#viii.
filter(myresults, x %in% c(1,3,5), y %in% c("A","B","C"))
#Pulls off all the rows were x is either 1, 3, or 5 AND
#y is in either A, B, or C. The only combinations of 
#x and y that meet these criteria are rows 1 and 3.
#These rows would have x=1, y=A and then x=3, y=C.
# Output C!

#-------------------------------------------------------
# yoyo

##2
yoyo <- read_excel("~/STS 347/0_Spring 2025/New Data/NationalYoYoContestFinals_HW3.xlsx")
head(yoyo)
summary(yoyo)
names(yoyo)
dim(yoyo)

##3
group_by(yoyo, Round) %>% summarize(mean(choreography),sd(choreography),mean(control),sd(control))

##4
ggplot(yoyo, aes(x=factor(Round),y=execution)) +
  geom_boxplot()

##5
ggplot(yoyo,aes(x=execution,y=total_te,color=Round)) + 
  geom_point()

##6
preliminary <- filter(yoyo,Round=="Preliminary")
final <- filter(yoyo, Round=="Final")

##7
final <- mutate(final,competition_score = total_te+execution+control+trick_diversity+choreography+construction+body_control+showmanship+stop_deduction+discard_deduction+detach_deduction)
preliminary <- mutate(preliminary,competition_score = total_te+execution+control+choreography+body_control+stop_deduction+discard_deduction+detach_deduction)

##8
advance <- filter(preliminary,competition_score >= 60)
myfinalists1 <- data.frame(Name=advance$Name,competition_score=advance$competition_score)
myfinalists1

#Alternative solution:
myfinalists1b <- preliminary %>% filter(competition_score >= 60) %>% select(Name, competition_score)

##9
advance2 <- filter(preliminary,total_te > 40, stop_deduction == 0)
myfinalists2 <- data.frame(Name=advance2$Name, total_te=advance2$total_te, stop_deduction=advance2$stop_deduction)
myfinalists2

#Alternative solution:
myfinalists2b <- filter(preliminary, total_te > 40, stop_deduction == 0) %>%
  select(Name, total_te, stop_deduction)

##10
winners <- arrange(final,desc(competition_score)) %>% head(3)
select(winners,Name, competition_score)
#Evan Nagao is my yoyo hero! :)

#Alternative solution:
arrange(final,desc(competition_score)) %>% head(3) %>% 
  select(Name, competition_score)

#----------------------------------------------
# Yahtzee!

##11
set.seed(2320)
dice <- 1:6

#12
?sample
myroll <- sample(dice,5,replace=T)
myroll
#Since everyone used the same seed, we should all get
#2 5 2 3 3 as our five dice rolls.
#The first argument of sample is the object you want to take random samples from
#The second argument is how many random samples you want to take (the size of your sample)
#The third argument is whether you want to replace a value once it has been used in the sample or not
#When the third argument is set to F, once a value is sampled it can't be used for other samples.

#13
#sample(5,dice,replace=TRUE) has the object dice and the sample size in the wrong order.

#sample(dice,5,replace=FALSE) does not allow for values to be sampled multiple times. 
#Therefore, if the first dice roll is a 3, the value of 3 would not be possible for any other randomly
#generated dice rolls.

#SAMPLE(dice,5,TRUE) uses all-caps SAMPLE and R is case sensitive!

#14
?unique
myroll2 <- unique(myroll)
myroll2
#This reports all of the unique values in the data. 

#15
length(unique(myroll))
#If there was just a single value, then we would have rolled a Yahtzee.
#Therefore, if the length is 1 we rolled a Yahtzee!

#16
myout <- replicate(5000,length(unique(sample(dice,5,replace=TRUE)))==1)

#17
sum(myout) #counts
sum(myout)/5000 #proportion

#------------------------------------------------

#18
c(1,2,3,4) + c(0,10)
#When a vectors do not have the same length, R will recycle the shorter vector
#in order to have enough values to complete the calculation. 
#Here the 0, 10 was used twice to complete the calculation as
# 1+0    2+10     3+0     4+10