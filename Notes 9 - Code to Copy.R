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

