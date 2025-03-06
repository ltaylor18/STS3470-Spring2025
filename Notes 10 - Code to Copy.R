# Notes 10 Code to Copy

myfx <- function(x){
  out <- 6*x*x + 2*x - 4
  return(out)
}

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



MyTrim <- function(datavar)		
{
  xmax <- max(datavar)
  print(xmax)
  xmin <- min(datavar)
  print(xmin)
  S <- sum(datavar) - xmax - xmin
  print(S)
  L <- length(datavar) - 2
  print(L)
  M <- S/L
  print(M)
  print("Laura Taylor")
  return(M)
}#end function



#------------------------------------------------------
# Lesson 10 - Activity 1 Part 1

# Read in boxhouses data!

#Confidence Interval Code:
out_ci <- replicate(5000,t.test(sample(boxhouses$rooms,10,replace=FALSE),mu=7.42,alternative="two.sided",conf.level=0.95)$conf.int[1:2])
sum(out_ci[1,] <= 7.42 & out_ci[2,] >= 7.42)/5000 #Level of Confidence

#1
CIsim <-function(n, level){
  
}

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
CIsimwidth <- function(n, level){
  out_ci <- _______________________________
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





#------------------------------------------------------
# Lesson 10 - Activity 1 Part 2

#Type I Error Code:
out <- replicate(5000,t.test(sample(boxhouses$rooms,10,replace=FALSE),mu=7.42,alternative="two.sided")$p.value)
sum(out <= 0.05)/5000

#1
TypeISim <- function(){
  
}

#2


#3
set.seed(2320)
#Insert code from line 2

#4
set.seed(_______)

#5
myn <- ___________
myresults <- ________

#6
for(__ in ___){
  myresults[__ - 2] <- TypeISim(n = ___)
}

#7
# Answer in notes

#8
TypeISimResults <- ______________________

#9
library(tidyverse)
ggplot(___,aes(x=__,y=__)) + 
  geom_point() +
  geom_line() +
  labs(x="__", y="__")





#------------------------------------------------------
# Lesson 10 - Activity 1 Part 3

#Type II Error Code:
out <- replicate(5000,t.test(sample(boxhouses$rooms,10,replace=FALSE),mu=4,alternative="two.sided")$p.value)
sum(out >= 0.05)/5000 #Type II Error
sum(out <= 0.05)/5000 #Power of Test

#1
# Answer in notes

#2
TypeIISim <- function(){
  
}

#3
#A #I updated the notes - test Ha: mu != 5.
TypeIISim(n=15, mualt=5)

#B #I updated the notes - test Ha: mu != 5.

#C
# Answer in notes

#D

#E
# Answer in notes

#4
myn <- 5:30
myresultsII <- matrix(NA, nrow=___, ncol=___)

#5
#Note: Depending on how you coded your function, you may need to modify
#the following code.
out2 <- NULL
for(i in ___){
  print(i) #So you can see the progress!
  out2 <- TypeIISim(___________)
  myresultsII[__ - 4, 1] <- out2$______ 
  myreulstsII[__ - 4, 2] <- out2$______ 
}

#6
TypeIISimResults <- data.frame(myn = myn, 
                               power = myresultsII[,__], 
                               typeII = myresultsII[,__])

#7
library(tidyverse)
ggplot(______, aes(x=___, y=____)) +
  geom_point() +
  geom_line() +
  labs(x="__", y="__")

#8
#a
myseq <- __________
myresultsIIB <- matrix(NA, nrow=___, ncol=___)

#b
out3 <- NULL
for(__ in ___){
  out <- TypeIISim(n = 10, mualt = __ + 0.42)
  myresultsIIB[__ + 1, 1] <- out3$______
  myresultsIIB[__ + 1, 2] <- out3$______
}

#9
TypeIISimResultsB <- data.frame(myseq = myseq,
                                power = myresultsIIB[,__],
                                typeII = myresultsIIB[,__])


#10
library(tidyverse)
ggplot(___, aes(x=__, y=__)) + 
  geom_point() +
  geom_line() +
  labs(x="__", y="__", caption="__")