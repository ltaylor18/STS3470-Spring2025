Laura <- 6
Laura/7

Laura2 <- 5
Laura2/7

myproc <- Laura/7

outcomes <- c(0,1)
outcomes

#Alternative way to create the vector outcomes
outcomes2 <- c("wrong","right")
outcomes2

#create a vector that contains the even numbers from 2 to 10
c(2,4,6,8,10)

#getting in help in R
?c

c(1,7:9)


?sample



myresults1 <- sum(sample(outcomes,7,replace=TRUE))

?replicate
myresults10000 <- replicate(10000,sum(sample(outcomes,7,replace=TRUE)))

hist(myresults10000)
table(myresults10000)



sum(myresults10000==7)/10000
