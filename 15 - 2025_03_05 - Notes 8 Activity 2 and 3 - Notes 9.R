# Day 15 - 3/5/2025
# Notes 8 - Activity 2
# Notes 8 - Activity 3
# Notes 9

#----------------------------------------------------
# Lesson 8 - Activity 2

out_ci <- replicate(5000,
                    t.test(sample(boxhouses$rooms,10),
                           mu=7.42,conf.level=0.99)$conf.int[1:2])

dim(out_ci)
is.matrix(out_ci)
is.numeric(out_ci)

myCIresults <- if_else(out_ci[1,] <= 7.42 & out_ci[2,] >= 7.42, 
                       "Captured",
                       "Not Captured")

table(myCIresults)

sum(out_ci[1,] <= 7.42 & out_ci[2,] >= 7.42)/5000

#----------------------------------------------------
# Lesson 8 - Activity 3


out <- t.test(sample(boxhouses$rooms,10),mu=7.42)
out
names(out)
out$statistic
out$statistic[[1]]

tstats10 <- replicate(5000,t.test(sample(boxhouses$rooms,10,replace=FALSE),mu=7.42,alternative="two.sided",conf.level=0.95)$statistic[[1]])

mydata <- data.frame(tstats10)
ggplot(mydata, aes(x=tstats10)) + 
  geom_histogram(aes(y=after_stat(density)),color="white")

tstats30 <- replicate(5000,t.test(sample(boxhouses$rooms,30,replace=FALSE),mu=7.42,alternative="two.sided",conf.level=0.95)$statistic[[1]])
tstats50 <- replicate(5000,t.test(sample(boxhouses$rooms,50,replace=FALSE),mu=7.42,alternative="two.sided",conf.level=0.95)$statistic[[1]])

tstats <- c(tstats10, tstats30, tstats50)
ind <- c(rep(10, 5000), rep(30, 5000), rep(50, 5000))

mydata <- data.frame(tstats, ind)
ggplot(mydata, aes(x=tstats, y=after_stat(density))) +
  geom_histogram(color="white") + facet_grid(~ind)


ggplot(mydata, aes(x=tstats)) +
  geom_histogram(aes(y=after_stat(density)),color="white") + facet_grid(~ind)
