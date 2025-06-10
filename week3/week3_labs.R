library(tidyverse) #gender age interaction thing???
library(scales)
theme_set(theme_bw())
library(MASS)
install.packages("ISLR")
fix(Boston)
names(Boston)
library(readr)
Body <- read.table("body.dat.txt")
names(body)
#v25 is gender while v24 is height and v23 is weight

Body <- Body %>% rename(height = V24, weight = V23)
lm.fit=lm(weight~height, data=Body)
summary(lm.fit)
#ISRS page 291 exercises 6.1,6.2,6.3

Babyweights <- read.table("babyweights.txt")
names(Babyweights)
view(Babyweights |> head(n=100))

#a) weight = -8.94(smoke) + 123.05
#b) interpret the slope in this context and calculate the predicrted birth weight 
#of babies born to smoker and non-smoker mothers