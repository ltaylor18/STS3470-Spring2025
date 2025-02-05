# Lesson 1 - Activity 2
# Lesson 2


#------------Lesson 1 - Activity 2----------------------#

all_employees <- c(25, 33, 35, 38, 48, 55, 55, 55, 56, 64)
mean(all_employees)

laidoff <- c(55, 55, 64)
mean(laidoff)

#5
mean(sample(all_employees,3,replace=FALSE))

#6
mean(sample(all_employees,3,replace=FALSE))
mean(sample(all_employees,3,replace=FALSE))
mean(sample(all_employees,3,replace=FALSE))
mean(sample(all_employees,3,replace=FALSE))
mean(sample(all_employees,3,replace=FALSE))
mean(sample(all_employees,3,replace=FALSE))
mean(sample(all_employees,3,replace=FALSE))
mean(sample(all_employees,3,replace=FALSE))
mean(sample(all_employees,3,replace=FALSE))

#Repeating above using the replicate() function
mysimulation <- replicate(10,mean(sample(all_employees,3,replace=FALSE)))
mysimulation

set.seed(1000000)
mysimulation <- replicate(10,mean(sample(all_employees,3,replace=FALSE)))
mysimulation

set.seed(2320)
mybigsimulation <- replicate(5000,mean(sample(all_employees,3,replace=FALSE)))
hist(mybigsimulation)
table(mybigsimulation)

(128+134)/5000 #proportion of simulations yielded 58 or larger
#0.0524

sum(mybigsimulation >= 58)/5000

#Example of logic
(mybigsimulation >= 58)




#-----------------------------Notes 2------------------------------#


statement <- "Hello World!"
height <- 72
mylogic <- TRUE
mylogic2 <- "TRUE"

length(statement)
is.character(statement)
is.character(height)
is.character(mylogic2)
is.numeric(height)

name <- c("Laura","Taylor")
length(name)

is.vector(statement)


#Example #1
a <- c(11, 13, 15, 1, 9, 7, 7, 10)
b <- seq(5,10,1)

a[3]

a[1] <- 8
a



a[3:6]

subset <- a[3:6]
subset

a[c(1,5,7)]

c(3:10)

3:10

seq(3:10)

seq(3,10,2)

a[-3]

x <- rep(4,10)
x

is.numeric(x)
is.character(x)
is.logical(x)
is.vector(x)

#############
#Exercises #2

?matrix

y <- matrix(1:10, ncol=2)
y

dim(y)

w <- matrix(1:10, nrow=2)
w

w[2,4]

y[,2]

y[2,]

w[1,c(4,5)]

z <- matrix(rep(10,12),3,4)
z
zb <- matrix(10,3,4)
zb

z2 <- matrix(10,3,4)

q <- matrix(10:18,3,3)


#Lesson 1 - Activity 1 follow-up
outcomes <- c(0,1)
results10 <- replicate(10,sum(sample(outcomes,7,replace=TRUE)))
results10

results10b <- replicate(10,sample(outcomes,7,replace=TRUE))
results10b













