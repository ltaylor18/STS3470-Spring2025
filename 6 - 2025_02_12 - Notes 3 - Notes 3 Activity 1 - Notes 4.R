# 2/12/2025
# Finish Lesson 3
# Lesson 3 Activity 1
# Lesson 4 

#--------------------------------------bison exercises

library(readxl)
bison <- read_excel("~/STS 347/0_Spring 2025/Old Data/Lesson 3 - bison.xlsx")
View(bison)

names(bison)
bison$`Northern herd spring calf ratio`
summary(bison)

#--------------------------------------Lesson 3, Activity 1

library(readxl)
bridges <- read_excel("~/STS 347/0_Spring 2025/Old Data/Lesson 3 - StatewideBridges.xls")
View(bridges)

summary(bridges)
hist(bridges$YEARBUILT)

bridges[1,] #first row
bridges[,1] #first column

Alamance <- bridges[bridges$COUNTY=="ALAMANCE",]
View(Alamance) #check it!

AlamanceB <- bridges[bridges$COUNTY=="Alamance",]
Laura <- bridges[bridges$COUNTY=="Laura",]
View(Laura)


length(Alamance$COUNTY)

min(Alamance$YEARBUILT)
max(Alamance$YEARBUILT)
table(Alamance$STRUCTURALLYDEFICIENT)


library(tidyverse)
Alamance2 <- filter(bridges, COUNTY=="ALAMANCE")
View(Alamance2)

#Which do you prefer?
Alamance <- bridges[bridges$COUNTY=="ALAMANCE",]
Alamance2 <- filter(bridges, COUNTY=="ALAMANCE")

#6b
Alamance_deficient <- filter(bridges, COUNTY=="ALAMANCE",STRUCTURALLYDEFICIENT=="SD")

table(Alamance_deficient$COUNTY)
table(Alamance_deficient$STRUCTURALLYDEFICIENT)

#6c
recentbridges <- filter(bridges,YEARBUILT >= 1990)
table(recentbridges$STRUCTURALLYDEFICIENT)



#--------------------------------------------Lesson 4
x <- seq(1,10)
median(x)
quantile(x,0.5)
mean(x,trim=0.5)

x <- 1:10
out <- summary(x)
out[2]
quantile(x,.25)


#-------------------------------------------Lesson 4, Activity 1
library(readxl)
bison <- read_excel("~/STS 347/0_Spring 2025/Old Data/Lesson 3 - bison.xlsx")

names(bison)

names(bison) <- c("year","nhscr", "chscr", "pdsi", "sweaccNH", "sweaccCH", "bisonNH", "bisonCH", "elk")

summary(bison)
summary(bison$elk)
mean(bison$elk)


?mean