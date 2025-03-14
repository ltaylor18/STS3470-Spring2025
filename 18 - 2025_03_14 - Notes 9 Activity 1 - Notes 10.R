# Day 19 - 3/14/2025
# Notes 9 - Activity 1
# Notes 10 - Functions!
# Note: See Code to Copy file!

#----------------------------------------------------
# Happy Pi Day!

# Notes 9 - Activity 1

boxhouses <- read_excel("~/STS 347/0_Spring 2025/New Data/Box houses.xlsx")


out <- NULL
pvalsim <- NULL
CIsim <- matrix(NA,1000,2)
for(i in 1:1000){
  out <- sample(boxhouses$rooms, 10, replace=FALSE)
  out.test <- t.test(out,mu=8)
  pvalsim[i] <- out.test$p.value
  CIsim[i,] <- out.test$conf.int
}

sum(pvalsim <= 0.05)/1000 #power

sum(pvalsim > 0.05)/1000

sum(CIsim[,1] <= 7.42 & CIsim[,2] >= 7.42)/1000


# Alternative Solution with a for() loop in a for() loop!
n <- 20 #update for #7, 10, 11
CIcapture <- NULL
mypower <- NULL
mytypeII <- NULL
index <- c(1, 3, 5, 6, 8, 9, 11, 13)
for(k in index){
  out <- NULL
  pvalsim <- NULL
  CIsim <- matrix(NA,nrow=1000,2)
  for(i in 1:1000){
    out <- sample(boxhouses$rooms, n, replace=FALSE)
    out2 <- t.test(out,mu=k)
    pvalsim[i] <- out2$p.value
    CIsim[i,] <- out2$conf.int
  }
  print(k)
  print("Power")
  mypower <- sum(pvalsim <= 0.05)/1000
  print(mypower)
  print("Type II Error")
  mytypeII <- 1-(sum(pvalsim <= 0.05)/1000)
  print(mytypeII)

  print("Capture Rate")
  CIcapture <- sum(CIsim[,1] <= 7.42 & CIsim[,2] >= 7.42)/1000
  print(CIcapture)

}
