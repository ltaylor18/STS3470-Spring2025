# Notes 9 Code to Copy

#---------------------------------------------------
# for() loops

#Example 1
for(i in 1:5) {print(i)}

#Example 2
for(index in seq(0,10,2)) {print(index)}

#Example 3
for(j in c(1,12,50)){
  print(j)
  print(2*j)
}

#Example 4
my_mat <- matrix(1:15,nrow=3)
myrowmeans <- NULL
for(i in 1:nrow(my_mat)){
  myrowmeans[i] <- mean(my_mat[i,])
}
myrowmeans

#------------------------------------------------
mypvals <- NULL
mycis <- matrix(NA,nrow=3,ncol=2)
for(i in __________){
  out <- t.test(randu[,__],mu=0.5)
  mypvals[___] <- out$p.value
  mycis[___,] <- out$conf.int
}
mypvals
mycis


#Equivalent to:
mypvals <- NULL
mycis <- matrix(NA,nrow=3,ncol=2)

#Iteration 1
out <- t.test(randu[,1],mu=0.5)
mypvals[1] <- out$p.value
mycis[1,] <- out$conf.int

#Iteration 2
out <- t.test(randu[,2],mu=0.5)
mypvals[2] <- out$p.value
mycis[2,] <- out$conf.int

#Iteration 3
out <- t.test(randu[,3],mu=0.5)
mypvals[3] <- out$p.value
mycis[3,] <- out$conf.int

mypvals
mycis



#Example 5
mysequence <- seq(100,2,-2)
for(i in mysequence){print(i/2)}









print("Do not look ahead!!!!!!!!!!!!!!!!!!!")

















print("I really mean it. No peeking!!!!!!!!!!!!!!!!!!!!")














# -----------------------------------------------------------
# Lesson 9 - Activity 3 Reference Code
# Do not use this until you have attempted first!

# n <- 10 #update for #7, 10, 11
# CIcapture <- NULL
# mypower <- NULL
# mytypeII <- NULL
# index <- c(1, 3, 5, 6, 8, 9, 11, 13)
# for(k in index){
#   out <- NULL
#   pvalsim <- NULL
#   CIsim <- matrix(NA,nrow=1000,2)
#   for(i in 1:1000){
#     out <- sample(boxhouses$rooms, n, replace=FALSE)
#     out2 <- t.test(out,mu=k)
#     pvalsim[i] <- out2$p.value
#     CIsim[i,] <- out2$conf.int
#   }
#   print(k)
#   print("Power")
#   mypower <- sum(pvalsim <= 0.05)/1000
#   print(mypower)
#   print("Type II Error")
#   mytypeII <- 1-(sum(pvalsim <= 0.05)/1000)
#   print(mytypeII)
#   
#   print("Capture Rate")
#   CIcapture <- sum(CIsim[,1] <= 7.42 & CIsim[,2] >= 7.42)/1000
#   print(CIcapture)
#   
# }
