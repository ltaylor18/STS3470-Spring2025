# 2/14/2025
# Lesson 4 

#----------------------------------------------------------------
dat<- data.frame(t=seq(0, 2*pi, by=0.01) )

xhrt<- function(t) 16*sin(t)^3

yhrt<- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)

dat$y=yhrt(dat$t)

dat$x=xhrt(dat$t)
with(dat, plot(x,y, type="l", axes=FALSE, frame.plot=FALSE, xlab = '', ylab = ''))

with(dat, polygon(x,y, col="#FF7575"))

#-------------------------------------------Lesson 4, Activity 1
library(readxl)
bison <- read_excel("~/STS 347/0_Spring 2025/Old Data/Lesson 3 - bison.xlsx")
View(bison)

names(bison)

names(bison) <- c("year","nhscr", "chscr", "pdsi", "sweaccNH", "sweaccCH", "bisonNH", "bisonCH", "elk")

summary(bison)
summary(bison$elk)
mean(bison$elk)


?mean


mean(bison$elk,na.rm=T)
length(bison$elk)


length(bison$elk, na.rm=T) #doesn't work!

sum(!is.na(bison$elk)) #sums up the number of data values that are not missing

cor(bison$elk,bison$bisonNH)
cor(bison$elk,bison$bisonNH, use="complete.obs")
cor(bison$elk,bison$bisonNH, use="pairwise.complete.obs")
cor(bison$bisonNH,bison$elk, use="pairwise.complete.obs")

plot(bison$bisonNH,bison$elk)


bison$ratio2 <- bison$elk/bison$bisonNH
View(bison) #check out our new variable!
names(bison)




View(InsectSprays)
summarize(InsectSprays,mean(count),sd(count))

mean(InsectSprays$count)
sd(InsectSprays$count)

summarize(bison,mean(elk))
summarize(bison,mean(elk,na.rm=T))

summarize(bison,meanelk = mean(elk,na.rm=T))

summarize(group_by(InsectSprays,spray),mean(count))
