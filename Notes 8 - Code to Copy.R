# Notes 8 Code to Copy

case_when(myscores >= 90 ~ "A",
          myscores >= 80 ~ "B",
          myscores >= 70 ~ "C",
          myscores >= 60 ~ "D",
          myscores < 60 ~ "F")


case_when(myscores < 60 ~ "F",
          myscores >= 60 ~ "D",
          myscores >= 70 ~ "C",
          myscores >= 80 ~ "B",
          myscores >= 90 ~ "A")

#-----------------------------------------------
# Age Discrimination Activity

all_employees <- c(25, 33, 35, 38, 48, 55, 55, 55, 56, 64)
mean(all_employees)

laidoff <- c(55, 55, 64)
mean(laidoff)

out <- replicate(5000,mean(sample(all_employees,3,replace=FALSE)))

myout <- case_when(___________________________________ ~ "More Extreme",
                   ___________________________________ ~ "As Extreme",
                   ___________________________________ ~ "Less Extreme")



#-----------------------------------------------

score <- 81

if(score >= 90) { 
  status <- "Pass"
  grade <- "A"
} else if(score >= 80){
  status <- "Pass"
  grade <- "B"
} else if(score >= 70){
  status <- "Pass"
  grade <- "C"
} else if(score >= 60){
  status <- "Fail"
  grade <- "D"
} else {
  status <- "Fail"
  grade <- "F"
}

status
grade






score <- -50
if(score <= 100 & score >= 70){
  status <- "Pass"
  valid <- TRUE
} else if(score >= 0 & score < 70){
  status <- "Fail"
  valid <- TRUE
} else {
  status <- NA
  valid <- FALSE
}

status
valid
