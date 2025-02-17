# 2/19/2025
# Lesson 4 Activity 2
# Lesson 5
# Lesson 5 - Activity 1

library(tidyverse)

#---------------------------------------------------------
# SNOW DAY!
#---------------------------------------------------------

#---------------------------------------------------------
#Lesson 4 - Activity 2

#1
head(diamonds)
View(diamonds)
summary(diamonds)
names(diamonds)
str(diamonds)
dim(diamonds)

#2 
count(diamonds, cut)
table(diamonds$cut)

#3
mean(diamonds$price)
mean(diamonds$carat)
summarize(diamonds,mean(price), mean(carat))

#4
diamonds %>% group_by(cut) %>% summarize(mean(price), mean(carat))

#5
Premium <- filter(diamonds,cut=="Premium")
PremiumE <- filter(diamonds, cut=="Premium",color=="E")
PremiumorE <- filter(diamonds, cut == "Premium" | color=="E")
IdealPremiumE <- filter(diamonds, color=="E", cut %in% c("Ideal","Premium"))
BigVG <- filter(diamonds, carat >= 3, cut == "Very Good")

#6
summarize(Premium, n())
summarize(PremiumE, n())
summarize(PremiumorE, n())


#7
group_by(diamonds, cut) %>% summarize(max(carat))
summarize(group_by(diamonds,cut), max(carat))

#8
diamonds2 <- mutate(diamonds, x_cm = x/10, y_cm = y/10, z_cm = z/10)

#9
#YES! This is the base syntax. For a single variable, I would prefer base,
#but for multiple variables, I would prefer tidyverse!










#---------------------------------------------------------
#Lesson 5
#Recall that we have already loaded the tidyverse library!

ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point(size=2)

ggplot(mtcars,aes(x=factor(am),y=mpg)) + geom_boxplot()

ggplot(mtcars, aes(x=wt)) + 
  geom_histogram(bins=15, color="white")

#Top Secret:
hist(diamonds$price)
ggplot(diamonds,aes(x=price)) + geom_histogram()

mygraph <- ggplot(diamonds,aes(x=price))
mygraph + geom_histogram(bins=10,color="purple")
mygraph + geom_histogram(bins=50, color="pink")



#---------------------------------------------------------
#Lesson 5 Activity 1

head(starwars)

#1
ggplot(starwars,aes(x=height,y=mass)) + geom_point()

#2
filter(starwars, species %in% c("Human","Droid")) %>%
  ggplot(aes(x=height,y=mass,color=species)) + 
  geom_point()

swgraph <- filter(starwars, species %in% c("Human","Droid")) %>%
  ggplot(aes(x=height,y=mass,color=species)) + geom_point()

#3
swgraph + guides(color="none")

#4
swgraph + labs(color="none")

#5 
swgraph + labs(color="Species")

#6
swgraph + labs(color="Species",
               x="Height (cm)",
               y="Mass (kg)",
               title="Mass vs. Height",
               subtitle="for Star Wars characters")

#7
ggplot(starwars,aes(x=height,y=mass)) + geom_point(size=3,
                                                   shape=8,
                                                   color="purple")

#8
ggplot(starwars,aes(x=height,y=mass)) + geom_point(size=5,
                                                   shape=0,
                                                   color="pink")

#9
filter(starwars,species %in% c("Human","Droid")) %>% 
  ggplot(aes(x=factor(species), y=mass)) + 
  geom_boxplot() +
  geom_point()

#10
filter(starwars,species %in% c("Human","Droid")) %>% 
  ggplot(aes(x=factor(species), y=mass)) + 
  geom_point() +
  geom_boxplot()

#11
#Code A
ggplot(starwars, aes(x=mass, y=height)) + 
  geom_point(color="purple")
#Code B
ggplot(starwars, aes(x=mass, y=height, color="happy")) + 
  geom_point()
