# Day 30 - 4/16/2025
# Notes 14
# Note: See Code to Copy file!

library(tidyverse)
#----------------------------------------------------

#Verification of variance
mydf <- 39
myt <- rt(50000,mydf)
var(myt)
truevar <- mydf/(mydf-2)
truevar

#----------------------------------------------------

# We will need to create our zscores and tscores again!

library(tidyverse)
library(patchwork)
set.seed(2024)
mydata <- replicate(10000,rnorm(9,100,5))
#Check: What does mydata look like?
dim(mydata)

samp.means <- apply(mydata,2,mean)
samp.sd <- apply(mydata,2,sd)

#2
zscores <- (samp.means - 100)/(5/sqrt(9))
tscores <- (samp.means - 100)/(samp.sd/sqrt(9))

#3
mydata.df <- data.frame(samp.means=samp.means,
                        samp.sd=samp.sd,
                        zscores=zscores,
                        tscores=tscores)

#--------------------------------------------------
# Investigation #2

invest1 <- data.frame(mystery = tscores*tscores)
ggplot(invest1,aes(x=mystery))+
  geom_histogram(aes(y=after_stat(density)),
                 color="white") +
  xlim(c(0,10)) +
  ylim(c(0,.7)) +
  stat_function(fun=df,args=list(df1=1,df2=9))


#Verification
ndf = 4
ddf = 99

myf <- rf(10000,df1=ndf,df2=ddf)

mean(myf)
ddf/(ddf-2)

var(myf)
(2*ddf*ddf*(ndf+ddf-2))/(ndf*(ddf-2)*(ddf-2)*(ddf-4))


#Shapes of F distribution

x <- seq(0.1,5,.01)
y1 <- df(x,3,126)           #F 3,126
y2 <- df(x,6,45)            #F 6,45
y3 <- df(x,1,9)             #F 1,9
t.df <- data.frame(x=x,y1=y1,y2=y2,y3=y3)
ggplot(t.df,aes(x=x,y=y1)) +
  geom_line(color="pink",linetype=1,size=1.25)+
  geom_line(aes(y=y2),color="purple",linetype=2,size=1.25)+
  geom_line(aes(y=y3),color="orange",linetype=3,size=1.25)



#------------------------------------------------------
# Investigation #3

invest2 <- data.frame(mystery = zscores*zscores)
ggplot(invest2,aes(x=mystery))+
  geom_histogram(aes(y=after_stat(density)),
                 color="white") +
  xlim(c(0,10)) +
  ylim(c(0,.7)) +
  stat_function(fun=dchisq,args=list(df=1),color="red")


#Verification
df = 4

mychi <- rchisq(10000,df=df)

mean(mychi)
df

var(mychi)
2*df

#Shapes of Chi-Square
x <- seq(0.001,70,.01)
y1 <- dchisq(x,3)
y2 <- dchisq(x,45)
y3 <- dchisq(x,19)
chisq.df <- data.frame(x=x,y1=y1,y2=y2,y3=y3)
ggplot(chisq.df,aes(x=x,y=y1)) +
  geom_line(color="pink",linetype=1,size=1.25)+
  geom_line(aes(y=y2),color="purple",linetype=2,size=1.25)+
  geom_line(aes(y=y3),color="orange",linetype=3,size=1.25)




#------------------------------------------------------
# Investigation #4

#1
set.seed(2320)
myvar <- replicate(5000,var(rnorm(15,sd=5)))

ggplot(NULL,aes(x=myvar))+geom_histogram(color="white",bins=30)


#2
newvar <- (15-1)*myvar/25
myvardata <- data.frame(newvar=newvar)
ggplot(myvardata,aes(x=newvar))+
  geom_histogram(aes(y=after_stat(density)),color="white",bins=30) +
  stat_function(fun=dchisq,args=list(df=14))


#3
myn <- ____
truevar <- _____ #You will need to calculate the theoretical value!
#Recall, Var(t_df) = df/(df-2)
#Recall, var(F_ndf,ddf) = (2*ddf*ddf*(ndf+ddf-2))/(ndf*(ddf-2)*(ddf-2)*(ddf-4))
#Recall, var(Chi^2 _ df) = 2*df
#Recall, var(Exp_rate) = 1/(rate^2)


myvar <- replicate(5000, var(r__(myn, <<parameters>>)))
newvar <- (myn-1)*myvar/truevar
myvardata <- data.frame(newvar=newvar)
ggplot(myvardata,aes(x=newvar))+
  geom_histogram(aes(y=after_stat(density)),
                 color="white",bins=30) +
  stat_function(fun=dchisq,args=list(df=myn-1))


