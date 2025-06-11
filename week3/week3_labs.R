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
lm.fit=lm(bwt~smoke, data=Babyweights)
summary(lm.fit)
#a) weight = -8.94(smoke) + 123.05
#b) interpret the slope in this context and calculate the predicrted birth weight 
#of babies born to smoker and non-smoker mother if the mother smokes baby will weight 8.94 units less
#predicted weight for smokers 114.11 for nonsmokers 123.05
#c) yes because the p value is low so you reject the null hypothesis that they have no relationship

#with parity variable
#a) predictd baby weight = -1.93(x) + 120.07
#b) so if the child is not first born the baby will weight 1.93 units less 
#not a stat sig relationhsip p value is too high for first born children they are expected to weigh 120.07 units 
#if the baby is not a first born the expected baby weight 118.77 units

#labs 5.3.1-5.3.3
library(ISLR)
library(MASS)
set.seed(1)
train=sample(392,196)

#selecting a random subset of 196 observations of out the orginial 392
lm.fit=lm(mpg~horsepower, data=Auto, subset=train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
#18.716
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
#18.79
set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
#25.72651
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
#20.430
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
#20.38533

#leave one out cross validation is k fold cross validation
glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)
lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)
library(boot)
glm.fit=glm(mpg~horsepower, data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta

cv.error = rep(0,5)
for (i in 1:5){
    glm.fit=glm(mpg~poly(horsepower,i), data=Auto)
    cv.error[i] = cv.glm(Auto,glm.fit)$delta[1] #this outputs the average MSE for n iterations
}
cv.error

#that was LOOCV which does n folds which is insanely expensive only good when u have small dataset
#and accuracy is critical, each observation takes turn being the "validation set" and its trained n times
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
    glm.fit=glm(mpg~poly(horsepower,i), data=Auto)
    cv.error.10[i]=cv.glm(Auto,glm.fit, K=10)$delta[1] #so only 10 iterations divides it up into 10ths
}
cv.error.10
#much quicker omg