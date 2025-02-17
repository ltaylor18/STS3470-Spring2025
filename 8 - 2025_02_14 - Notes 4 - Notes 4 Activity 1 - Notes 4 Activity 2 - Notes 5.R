# 2/14/2025
# Lesson 4, Lesson 4 Activity 2, Lesson 4 Activity 3
# Lesson 5

#----------------------------------------------------------------
# Lesson 4 Activity 2
library(tidyverse)
library(readxl)
bison <- read_excel("~/STS 347/0_Spring 2025/Old Data/Lesson 3 - bison.xlsx")
View(bison)

names(bison)
bison <- rename(bison,Year = Yr, BisonNH = `Bison northern herd`,
                Elk = `Elk northern herd`)
names(bison)

Elk <- filter(bison, !is.na(Elk))

summarize(Elk,mean=mean(Elk), 
          sd(Elk), 
          cor(Elk,BisonNH),
          min(Year),
          maxYr = max(Year))


summarize(bison,mean=mean(Elk), sd(Elk), cor(Elk,BisonNH),min(Year),maxYr = max(Year))




#------------------------
print("Dr. Taylor's secret code is below.........")






#------------------------
summarize(bison,
          mean=mean(Elk,na.rm=T), 
          sd(Elk, na.rm=T), 
          cor(Elk,BisonNH,use="pairwise.complete.obs"),
          min(Year),
          maxYr = max(Year))


#5

Elk <- arrange(Elk,desc(Elk))
?head
head(Elk, 1)

#6
Elk <- mutate(Elk, ratioEB = Elk/BisonNH)

#7
summarize(Elk, median(ratioEB))


#----------------------------------------------------------------

#1 - long way
ChickWeight2 <- group_by(ChickWeight,Diet)
summarize(ChickWeight2,mean(weight))

#2 - long way
Loblolly2 <- arrange(Loblolly,desc(height))
head(Loblolly2,1)





#------------------------
print("Dr. Taylor's secret code is below.........")

#1
names(ChickWeight)
ChickWeight %>% group_by(Diet) %>% summarize(mean(weight))
group_by(ChickWeight, Diet) %>% summarize(mean(weight))

#2
Loblolly %>% arrange(desc(height)) %>% head(1)
arrange(Loblolly,desc(height)) %>% head(1)


#---------------------------------------------------------
#Lesson 4 - Activity 2

#1
head(diamonds)
View(diamonds)
summary(diamonds)
names(diamonds)
str(diamonds)
dim(diamonds)

#2 
count(diamonds, cut)
table(diamonds$cut)

#3
mean(diamonds$price)
summarize(diamonds,mean(price))

#4
diamonds %>% group_by(cut) %>% summarize(mean(price))

#5
