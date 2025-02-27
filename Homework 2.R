rm(list=ls())
#Laura Taylor - Homework #2 - Solutions

#---------------------------------------------------------------------------#
## 1 ##
is.data.frame(randu)
dim(randu)
str(randu)
names(randu)
#The object randu at first looks like a matrix, but it is actually a data frame.
#It contains three variables: x, y, and z that are all numeric.
#There are 400 rows and 3 columns.

#---------------------------------------------------------------------------#
## 2 ##
myranduz <- randu$z

#---------------------------------------------------------------------------#
## 3 ##
mymean <- mean(myranduz)
mysd <- sd(myranduz)
mymean
mysd

#---------------------------------------------------------------------------#
## 4a ##
myt <- (mymean - 0.5)/(mysd/sqrt(length(myranduz)))
myt

#---------------------------------------------------------------------------#
## 4b ##
t.test(myranduz,alternative="two.sided",mu=0.5)

#---------------------------------------------------------------------------#
## 4c ##
#Yes! The value of myt (-1.365) that I calculated in #4a is shown after t = 
#in the output for the t.test() function.

#---------------------------------------------------------------------------#
## 4d ##
#The p-value is 0.1729957. We conclude (d): there is insufficient evidence
#to conclude that the population mean for the population from which these
#random numbers is generated is different from 0.5.

#The conclusion seems to indicate that this is a "good" random number generator
#as we would expect the mean to be 0.5 and we did not find evidence that the
#mean was different from 0.5.

#---------------------------------------------------------------------------#
## 4e ##
#(0.4535, 0.5084)

#---------------------------------------------------------------------------#
## 4f ##
out95 <- t.test(myranduz,alternative="two.sided",mu=0.5)
is.list(out95)
#Since the object out95 contains a variety of vectors and scalars, it is
#a list object.

#---------------------------------------------------------------------------#
## 4g ##
lower95 <- out95$conf.int[1]
upper95 <- out95$conf.int[2]
upper95 - lower95

#---------------------------------------------------------------------------#
## 4h ##
out90 <- t.test(myranduz,alternative="two.sided",mu=0.5, conf.level=0.90)
lower90 <- out90$conf.int[1]
upper90 <- out90$conf.int[2]
upper90 - lower90

#---------------------------------------------------------------------------#
## 4i ##
#The 95% confidence interval has a width of 0.05485621776 (upper95-lower95)
#whereas the 90% CI has a width of 0.0460039562 (upper90-lower90).
#The 95% confidence interval is wider.

#---------------------------------------------------------------------------#
## 4j ##
mean(c(upper95,lower95))
(upper90+lower90)/2
mean(myranduz)
#The intervals are centered at the mean of the sample!

#---------------------------------------------------------------------------#
## 4k ##
#If we calculated a 99% confidence interval, (b) it would be centered at the
#same place as the 95% confidence intrval, but it would be wider.

#Verification:
out99 <- t.test(myranduz,alternative="two.sided",mu=0.5, conf.level=0.99)
mean(out99$conf.int)
out99$conf.int[2]-out99$conf.int[1] 
#It is centered at the mean of the sample (0.48095) and has a great width
#of 0.0722.

#---------------------------------------------------------------------------#
## 5a ##
myrandu <- as.matrix(randu)
is.matrix(myrandu)

#---------------------------------------------------------------------------#
## 5b ##
myrandu[100,]

#---------------------------------------------------------------------------#
## 5c ##
myrandu[c(5,10),]

#---------------------------------------------------------------------------#
## 6 ##
myteeth <- as.matrix(ToothGrowth)
is.matrix(myteeth)
myteeth
#Since the object ToothGrowth contains character and numeric values and
#matrices can only contain one type, all of the numbers are converted to
#characters and shown in "" when displayed. This is unfortunate!

#---------------------------------------------------------------------------#
## 7 ##
c(rep(1:5,2),rep(7,4),seq(20,15,-1))

#---------------------------------------------------------------------------#
## 8 ##
myseq <- c(rep(1:5,2),rep(7,4),seq(20,15,-1))
myseq[11] <- 6
myseq