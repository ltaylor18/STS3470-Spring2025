# Day 14 - 3/3/2025
# Notes 8
# Notes 8 - Activity 1

#----------------------------------------------------
# Lesson 8 - Activity 1

library(readxl)
boxhouses <- read_excel("~/STS 347/0_Spring 2025/New Data/Box houses.xlsx")
View(boxhouses)

#Find mu!
mean(boxhouses$rooms)

#4
t.test(sample(boxhouses$rooms,10,replace=F),mu=7.42)$p.value

set.seed(2320)
#5
out <- replicate(10000,
                 t.test(sample(boxhouses$rooms,10,replace=F),mu=7.42)$p.value)

#6
pcount <- if_else(out <= 0.05, "significant", "not significant")

#7
table(pcount)

#8
sum(pcount == "significant")/10000

#9
pcount2 <- case_when(out <= 0.10 ~ "significant",
                     out > 0.10 ~ "not significant")

#10
table(pcount2)

#11
sum(pcount2  == "significant")/10000

#-------------------------------------------------
# Repeat for n=20
#5
print("n=20 results")
out <- replicate(10000,
                 t.test(sample(boxhouses$rooms,20,replace=F),mu=7.42)$p.value)

#6
pcount <- if_else(out <= 0.05, "significant", "not significant")

#7
print("n=20 results")
table(pcount)

#8
sum(pcount == "significant")/10000

#9
pcount2 <- case_when(out <= 0.10 ~ "significant",
                     out > 0.10 ~ "not significant")

#10
print("n=20 results")
table(pcount2)

#11
sum(pcount2  == "significant")/10000

