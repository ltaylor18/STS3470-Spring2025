#Day 10
#Snow Day #2
# Lesson 6 - Activity 1
# Lesson 6 - Content

library(tidyverse)
library(patchwork)

ggplot(diamonds,aes(x=price)) +
  geom_histogram(color="white") +
  labs(x="Price", 
       title="Frequency histogram of price of diamonds")

ggplot(diamonds,aes(x=price,y=after_stat(density))) +
  geom_histogram() +
  labs(x="Price", 
       title="Density histogram of price of diamonds")

A <- ggplot(diamonds,aes(x=price,y=after_stat(density))) +
  geom_histogram() +
  labs(x="Price", 
       title="Density histogram of price of diamonds")
B <- ggplot(diamonds,aes(x=factor(cut),y=price)) +
  geom_boxplot()
C <- ggplot(diamonds,aes(x=carat,y=price)) +
  geom_point()

A

A + B
A / B
(A + B)/C


# Lesson 6 - Partial Activity 2/Partial Guided Instruction 
# Code to Copy

library(tidyverse)

#1
myx <- c(1,2,2,3,3,3,3,4,4,5)
out.x <- data.frame(myx=myx)


#2
ggplot(out.x,aes(x=myx)) + 
  geom_histogram(color="white",bins=5)


#3
table(myx) #frequencies (and heights of bars!)
table(myx)/length(myx) #proportion
table(myx)/length(myx)/1 #proportion/width


#4
ggplot(out.x,aes(x=myx,y=after_stat(density))) + 
  geom_histogram(color="white",bins=5)

#or
ggplot(out.x,aes(x=myx)) + 
  geom_histogram(aes(y=after_stat(density)),color="white",bins=5)

#5
ggplot(out.x,aes(x=myx)) + 
  geom_histogram(color="white",bins=10)


#6
table(myx)/length(myx)/0.5 #densities


#7
ggplot(out.x,aes(x=myx,y=after_stat(density))) + 
  geom_histogram(color="white",bins=10)


#8
#True?/False? A histogram of densities of a data set will have 
#different bar heights based on the number of bins we use.


#9
#True?/False? A histogram of densities of a data set will be 
#proportional to a frequency histogram of the same data set 
#(assuming bin widths are the same between the two histograms).


#10
#True?/False? The sum of all of the proportions will equal 1 
#regardless of bin size.


#11
#True?/False? The sum of all of the densities will equal 1 
#regardless of bin size.


#12
myx2 <- rep(myx, 5)

out.x2 <- data.frame(myx2=myx2)
ggplot(out.x2,aes(x=myx2)) + 
  geom_histogram(color="white",bins=5)

table(myx2) #frequencies (and heights of bars!)
table(myx2)/length(myx2) #proportion
table(myx2)/length(myx2)/1 #proportion/width


#13
#YES!

#14
ind <- c(rep("A",length(myx)), rep("B",length(myx2)))


#15
out.all <- data.frame(x.all = c(myx, myx2), ind=ind)


#16
ggplot(out.all,aes(x=x.all)) + 
  geom_histogram(color="white",bins=5) + 
  facet_grid(~ind)


#17


#18
ggplot(out.all,aes(x=x.all)) + 
  geom_histogram(aes(y=after_stat(density)),color="white",bins=5) + 
  facet_grid(~ind)


#19

