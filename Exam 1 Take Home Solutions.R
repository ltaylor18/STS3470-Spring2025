rm(list=ls())
#Laura Taylor - Take Home #1
library(readxl)
library(tidverse)
#--------------------------------------------------------------

##2
frogs <- read_excel("~/STS 347/0_Spring 2025/Exams/jumping frogs.xlsx")

##3
frogs <- mutate(frogs,frog_type2 = case_when(frog_type == 1 ~ "Rental", 
                                             frog_type == 3 ~ "Professional",
                                             .default = "Unknown"))

##4
count(frogs, frog_type2)

##5

rentAndpro <- filter(frogs,frog_type2 %in% c("Rental", "Professional"))

filter(rentAndpro, distance != 0) %>% ggplot(aes(x=distance,y=after_stat(density))) + geom_histogram(bins=30,color="white") +
  facet_grid(~frog_type2) + labs(x="Distance (cm)",
                                y="Density",
                                title="Jump Distance for Rental and Professional Frogs")

##6
group_by(frogs,frog_type2) %>% filter(frog_type2 %in% c("Rental", "Professional"), distance != 0) %>% 
  summarize(mean(angle_01, na.rm=T),sd(angle_01, na.rm=T))

##7
sampmean <- rep(100.5, 4)
sampsd <- c(10.08, 6.08, 2.08, 1.08)
sampsize <- rep(30,4)
tscores <- (sampmean - 100)/(sampsd/sqrt(sampsize))
tscores
pvalues <- 2*(1-pt(tscores, 29))
pvalues

data.frame(sampmean, sampsd,sampsize, tscores, pvalues)

#As the standard deviation decreases, the test statistic increases.
#As the standard deviation decreases, the p-value increases.

#A smaller standard deviation indicates that there is less variability,
#therefore a difference of 0.5 is more unusual (hence a large test statistic
#and smaller p-value) when the standard deviation is smaller. This helps
#us understand the relationship between the standard deviation and
#test statistics and p-values noted in my statements.