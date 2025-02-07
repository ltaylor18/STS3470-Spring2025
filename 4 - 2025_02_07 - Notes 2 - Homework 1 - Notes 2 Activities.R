# Day 4
# Notes 2 continued

#--------------------------------------------------------------------------#
# Homework 1 Comments

#One of the questions asked you to create a vector containing: 0, 0, 1, 1, 2, 2
#My solution:
newseq <- c(rep(0,2),rep(1,2),rep(2,2))

#Here are the variety of solutions you all submitted:

newseq <- rep(0:2, c(2,2,2))
newseq <- rep(c(0:2), times = 1, each = 2)
newseq <- rep(0:2, each = 2)
newseq <- rep(c(0,1,2),each=2)
newseq <- rep(c(0:2),each = 2)
newseq <- rep(0:2, c(2,2,2))
newseq <- rep(c(0,1,2),2)
newseq <- c(rep(0,2), rep(1,2), rep(2,2)) #Matched my solution! :)

c_function <- c(0,1,2)
newseq <- rep(c_function,1, each = 2)

oldseq <- c(0:2)
newseq <- rep(oldseq,1,NA,2)

# Your homework score is on Moodle, but please look at my feedback on the
# actual R code you submitted. Also, look at my solutions available on GitHub.

#--------------------------------------------------------------------------#

# Where we left off:
#Lesson 1 - Activity 1 follow-up
outcomes <- c(0,1)
results10 <- replicate(10,sum(sample(outcomes,7,replace=TRUE)))
results10

results10b <- replicate(10,sample(outcomes,7,replace=TRUE))
results10b

#What will the following tell us:
sum(results10b[,1])

sum(results10b[1,])



#---------------------------------Lesson 2 - Activity 1

#1
dice <- 1:6

#2
is.vector(dice)
is.numeric(dice)

#3
sample(dice,1)

#4
roll1 <- sample(dice,1)
length(roll1) #scalar!

#5
sample(dice,10,replace=TRUE)

#6
roll10 <- sample(dice,10,replace=TRUE)
is.vector(roll10) #vector!

#7
roll10[1]

#8
sum(roll10)

#9
roll10[5] <- NA

#10
sum(roll10)
?sum #is there an argument that will allow you to omit missing values?
sum(roll10,na.rm=T)


#11
dice <- seq(1,6)

#12
rolls60 <- sample(dice,60,replace=TRUE)

#13
catanrolls <- matrix(rolls60,nrow=2)

#14
dim(catanrolls)

#15
sum(catanrolls[,10])

#16
apply(catanrolls,2,sum)


# Then Complete Lesson 2
# Then any remaining time can be spent on Lesson 2 - Activity 2


#-------------------------------------------------------
mynewdata <- data.frame(exam=1:3, grade=c(85,90,89,97))

myvec1 <- 1:30
myvec2 <- 31:60

mydata <- data.frame(myvec1=myvec1, secondset=myvec2)
names(mydata)


#--------------------------------------------------------
# Data frames exercises

View(Loblolly)
?Loblolly
head(Loblolly)
dim(Loblolly)
str(Loblolly)
is.data.frame(Loblolly)


Loblolly$height

max(Loblolly$height)

mean(Loblolly$height)
sd(Loblolly$height)

#Exploring:
Loblolly[[1]]
Loblolly[1]
is.vector(Loblolly[[1]])
is.vector(Loblolly[1])
is.data.frame(Loblolly[1])
Loblolly[1:2,]
Loblolly[,2]



