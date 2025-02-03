# Lesson 1 and Activity 2

x <- c(1,3,4,
       8)
y = c(55, 84, 88, 89)
x
y
lm(y~x)

?lm
?seq

#Examples from Help code:
seq(0, 1, length.out = 11)
seq(stats::rnorm(20)) # effectively 'along'
seq(1, 9, by = 2)     # matches 'end'
seq(1, 9, by = pi)    # stays below 'end'
seq(1, 6, by = 3)
seq(1.575, 5.125, by = 0.05)
seq(17) # same as 1:17, or even better seq_len(17)


z <- 5:10
z
z <- seq(10,20)
z



#----------------------Activity 2----------------------#

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

set.seed(2320)
mysimulation <- replicate(10,mean(sample(all_employees,3,replace=FALSE)))
mysimulation

mysimulation <- replicate(5000,mean(sample(all_employees,3,replace=FALSE)))
hist(mysimulation)
table(mysimulation)
(128+134)/5000

sum(mysimulation >= 58)/5000
