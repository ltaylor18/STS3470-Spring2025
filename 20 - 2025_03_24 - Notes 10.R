# Day 20 - 3/24/2025
# Notes 10 - Functions!
# Note: See Code to Copy file!

#----------------------------------------------------

myfx <- function(x){
  out <- 6*x*x + 2*x - 4
  return(out)
}

#1
myfx(0)
myfx(x=0)

#2
myfx(1)

#3
f10 <- myfx(10)
f10

#4
myfx2 <- function(x){
  out <- 6*x*x + 2*x - 4
  #return(out)
}

myfx2(0)

#5
myfx3 <- function(x=0){
  out <- 6*x*x + 2*x - 4
  out
}

myfx3()
myfx()

#6
args(myfx3)
args(sample)
args(t.test)

#-------------------------------------------------------------

MyMean <-  function(X) { 
  S <- sum(X) 
  L <- length(X) 
  M <- S/L 
  m <- c("Mean is: ", M) 	#creates a character string   
  newS <- paste("Sum is: ", S)  #what does paste do?
  newL <- paste("Length is: ", L)
  print(newS) #What is the default output?
  print(newL, quote=FALSE) #What does quote=FALSE do?
  print(m, quote=FALSE) #Did we get the same output?  
  X  #what happens to whatever you list last?
}#end function

#5
View(trees)
MyMean(trees) #Weird...
MyMean(trees$Volume)

#7
out <- MyMean(trees$Volume)
out

#8
MyMean(mtcars$mpg)


#------------------------------------------------------
# Lesson 10 - Activity 1 Part 1

# Read in boxhouses data!
library(readxl)
boxhouses <- read_excel("~/STS 347/0_Spring 2025/New Data/Box houses.xlsx")

#Confidence Interval Code:
out_ci <- replicate(5000,t.test(sample(boxhouses$rooms,10,replace=FALSE),mu=7.42,alternative="two.sided",conf.level=0.95)$conf.int[1:2])
sum(out_ci[1,] <= 7.42 & out_ci[2,] >= 7.42)/5000 #Level of Confidence

#1
CIsim <-function(n=10, level=0.95){
  out_ci <- replicate(5000,t.test(sample(boxhouses$rooms,n,replace=FALSE),mu=7.42,alternative="two.sided",conf.level=level)$conf.int[1:2])
  prop <- sum(out_ci[1,] <= 7.42 & out_ci[2,] >= 7.42)/5000 #Level of Confidence
  return(prop)
}

CIsim()


#2
CIsim(5, 0.95)
CIsim(10, 0.95)
CIsim(15, 0.95)
CIsim(15, 0.90)
CIsim(15, 0.95)
CIsim(15, 0.99)


#3
# Answer in notes

#4
CIsimwidth <- function(n=10, level=0.95){
  out_ci <- replicate(5000,t.test(sample(boxhouses$rooms,n,replace=FALSE),mu=7.42,alternative="two.sided",conf.level=level)$conf.int[1:2])
  width <- mean(out_ci[2,] - out_ci[1,]) #Width of CI
  width
}

#5
CIsimwidth(5, 0.90)
CIsimwidth(10, 0.90)
CIsimwidth(15, 0.90)
CIsimwidth(5, 0.95)
CIsimwidth(10, 0.95)
CIsimwidth(15, 0.95)
CIsimwidth(5, 0.99)
CIsimwidth(10, 0.99)
CIsimwidth(15, 0.99)

#6
# Answer in notes

#7 (if time permits)


#------------------------------------------------------
# Illustration that #5 could be completed with a for() loop!
nsim <- rep(c(5,10,15),3)
loc <- c(rep(0.9,3),rep(0.95,3),rep(0.99,3))

simresults <- NULL

for(i in 1:9){
  simresults[i] <- CIsimwidth(n=nsim[i],level=loc[i])
}

show_simresults <- data.frame(sample_size=nsim, level_of_confidence=loc, width=simresults)