# Notes 11 Code to Copy

#--------------------------------------------

library(ggplot2)

coin <- c("Heads", "Tails")
set.seed(2320)
fewtosses <- sample(coin,100,replace=TRUE)

fewtosses_sum <- cumsum(fewtosses=="Heads")
fewtosses_prop <- fewtosses_sum/1:100

few <- data.frame(trial=1:100,propHeads=fewtosses_prop,tosses=fewtosses)
few_graph <- ggplot(few,aes(x=trial,y=propHeads))+
  geom_point()+
  geom_hline(yintercept=0.5,color="red")+
  geom_line(color="purple")

few_graph



#------------------------------------------------------
# Notes 11 p. 7

#1
x1 <- seq(__,__,__)
y1 <- dnorm(x1,__,__)
mydata2 <- data.frame(x1=x1,y1=y1)
ggplot(mydata2,aes(x=__,y=__)) + geom_line()

#2
mydata2 <- mutate(mydata2,y2=dnorm(x1,__,__))
ggplot(mydata2,aes(x=x1,y=y1)) +
  geom_line(color="purple") +
  geom_line(aes(y=__),color="green")


#-------------------------------------------------------
# Notes 11 Activity 1

#1
myx <- seq(,,)
myy <- _____________________
mydata3 <- data.frame(myx, myy)
ggplot(mydata3,aes(x=myx, y=myy)) + geom_line()

#3
SATheight <- dnorm(1300, 1060, 195)
SATheight

#Add this layer to your graph!
#geom_linerange(x=1300, ymin=__, ymax=__, col="red")

#6
simul_SAT <- rnorm(30, 1060, 195)


#9
set.seed(XXXX)

out <- replicate(10000, rnorm(30,1060,195))


mean30 <- apply(__,__,__)


#16
mymeandata <- data.frame(mean30)
ggplot(mymeandata,aes(x=mean30)) + 
  geom_histogram(aes(y=after_stat(density)), color="white")

#17
#Add layer
#stat_function(fun=dnorm, args=list(mean=1060,35.602))