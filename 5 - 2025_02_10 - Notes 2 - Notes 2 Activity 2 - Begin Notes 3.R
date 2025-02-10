# 2/10/2025
# Lesson 2 Continued
# Lesson 2 Activity 2
# Lesson 3 (?)

myvec <- c(5, "Laura", 7.9)
myvec

#-------------------------------------------------------
#Lesson 2 - Activity 2#

#1
View(mtcars)
summary(mtcars)
head(mtcars)
str(mtcars)
?mtcars
names(mtcars)

#2
mean(mtcars$mpg)

#3
mympg <- mtcars$mpg

#4
max(mympg)

#5
#install tidyverse library
library(tidyverse)
View(diamonds) #it works now!

#8
dim(diamonds)
nrow(diamonds)

#9
table(diamonds$cut)

#10
somediamonds <- diamonds[1:20,c(1,7)]

#11
is.data.frame(somediamonds)
typeof(somediamonds)

#are data frames special cases of lists?




#----------------------------------Notes 3---------------------


library(readr)
twilight <- read_table("~/STS 347/0_Spring 2025/Old Data/Lesson 3 - Age Data.txt", 
                       comment = "!")
View(twilight)

names(twilight)




library(readxl)
bison <- read_excel("~/STS 347/0_Spring 2025/Old Data/Lesson 3 - bison.xlsx")
View(bison)

names(bison)

bison$`Northern herd spring calf ratio`  #notice the ` marks!

summary(bison)`
