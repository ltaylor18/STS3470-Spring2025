rm(list=ls())
#Laura Taylor - Homework #1 - Solutions

#---------------------------------------------------------------------------#
##1##
#5 + 7 performs the calculation, but does not store the resulting value
#into an object.

# x <- 5 + 7 stores the resulting addition value into the object x
#so that x contains the value of 12.

# x = 5 + 7 does the same thing as the previous line of code but uses
#an equal sign instead of the assignment arrow

#---------------------------------------------------------------------------#
##2##
?seq

#---------------------------------------------------------------------------#
##3##
myseq <- seq(1,10)

#---------------------------------------------------------------------------#
##4##
2*myseq
#multiples each element of the sequence by 2

#---------------------------------------------------------------------------#
##5##
myseq - 100
#subtracts 100 from each element of the sequence

#---------------------------------------------------------------------------#
##6##
myseq2 <- seq(2,10,2)

#---------------------------------------------------------------------------#
##7##
myseq3 <- c(1,2,3,4,5,6,7,8,9,10)

#---------------------------------------------------------------------------#
##8##
sum(myseq)
sum(myseq2)
sum(myseq3)

#---------------------------------------------------------------------------#
##9##
sum(seq(1,100))

#---------------------------------------------------------------------------#
##10##
?rep
newseq <- c(rep(0,2),rep(1,2),rep(2,2))
newseq

#---------------------------------------------------------------------------#
##11##
exam1 <- 87
exam2 <- 78
exam3 <- 95
homework <- 92
project <- 89
.7*mean(c(exam1, exam2, exam3)) + .15*homework + .15*project

#---------------------------------------------------------------------------#
##12##
exam1replaced <- mean(c(exam3, exam2, exam3))
exam2replaced <- mean(c(exam1, exam3, exam3))
noreplacement <- mean(c(exam1, exam2, exam3))

examscore <- max(exam1replaced, exam2replaced, noreplacement)

.7*examscore + .15*homework + .15*project

#---------------------------------------------------------------------------#
##13##
?ceiling
ceiling(.7*examscore + .15*homework + .15*project)

#---------------------------------------------------------------------------#
##14##
?round
round(.7*examscore + .15*homework + .15*project)