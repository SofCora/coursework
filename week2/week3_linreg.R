library(tidyverse) #gender age interaction thing???
library(scales)
theme_set(theme_bw())
#exercises 5.2 and 5.29
#page 259
#5.2 a 
#y=mx + b
#m = .6079
#b = y bar - mxbar
#b = 105
# y = (.607)x + 105.35711
#starting from 105 cm, for every cm of shoulder girth, the predicted increase
#in height will be .6079 cm when shoulder girth is 0 height is 105

#c R^2 of the regression lien and interpret it in the context of this application
# 1 - mse(y hat, y) / var(y)
# but we just do R ^2 .4489
#d) predicted height is 166.05 cm roughly
#e) student is actually 160 cm calculate ther residual and explain what residual means
"residual is 6.05 cm which is kind of slay s owe "
#(f) it wouldn't because the minimum is like 70 or something
#also a 1 year old is not 105 cm tall (3'4" tall)

#5.29 page 262 in textbook
# T is simply the calculated difference represented in units of standard error. The greater the magnitude of T,
# the greater the evidence against the null hypothesis. 
#strongly positvely correlated using the slope of 1.017
#y=1.0176(x)-105.0113 when height is 0 weight is -105.01 kg for every cm increase in height, weight increases by 1.0176 kg 

#null hypothesis -- height and weight have nothing to do w each other, m =0  alternative hypothesis -- height is a predictuor for weight m not equal to 0
#the extremely low p value means we should reject the null hypothesis we have enough evidence to reject that height and weight don't have a relationship

#(d) correlation .5184 the changes in height explain 51% of the changes in weight
library(MASS)
install.packages("ISLR")
fix(Boston)
names(Boston)

lm.fit=lm(medv ~ lstat)
lm.fit=lm(medv ~ lstat, data=Boston)
attach(Boston)
lm.fit=lm(medv  ~ lstat)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="confidence")
# Thepredict() function can be used to prodcue confidence itnervals and prediction intervals for the prediction of medv for a given value of lstat
predict(lm.fit,data.frame(lstat=c(5,10,15)), interval="prediction")
#confidence interval is the interval for the fitted line while the prediction interval is the interval for each indivdiual datapoint which has 
#way more variance. the line confidence interval is for the line which is the average of the point so its a much tigher range
#confidence interval uses the mean and standard deviation  from the dataset
plot(lstat,medv)
abline(lm.fit)
plot(lstat,medv,pch="+")
par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
par(mfrow = c(1,1))
plot(predict(lm.fit), residuals(lm.fit))

plot(hatvalues(lm.fit)) #i think this plots the leverage how far that observation's preductor values are from the mean predictor values how much each value influences the final prediction
which.max(hatvalues(lm.fit))
#the least squares method is finding the best fit for a set of data points by minimizing the sum of the squares of the residuals
#y=mx+b taking partial derivative for slope and intercept = 0 
lm.fit = lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

lm.fit=lm(medv~., data = Boston)
summary(lm.fit)

summary(lm.fit)$r.sq
#r^2 is .74 which means using all 13 features accounts for 74% of variation in the data. 74% less variation than if we just used the mean
library(car)
vif(lm.fit) #variance inflation factors -- how much the variation of a regression coefficient is inflated due to correltion with other predictors
#if its really high then it means theres multicolinearity in some of the features which is bad because it makes them unstable they 
#are basically the same and it makes the coeffiicents unreliable. 
lm.fit1=lm(medv~-age, data=Boston) #all variables except for age because it has a very high p value according to summary(lm.fit)
#p value for a coefficient tells you whether that particular predictor like age is statisticlally significnaly associated with response vairable after 
#accounting for other variables in the model. we do not have strong evidence that age affects the response.
#the observed coefficient could easily be due to chance if u fit the model just using age though the p value is super low
#this indicates that theres colinearlty and that age isnt helpful when other variables are around it
lm.fit1=update(lm.fit, ~.-age)

#in the coeffifcent table, the t value tells you how mand se the estimated coeffiicent is away from 0
#basically if the predictors effect is drastically different from 0. estimate/se large t value >2 means it has sig effect

lm.fit2 = lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
#we use anova() function to quantify the extent to which the quadratic fit is superior to linear, we know its better cuz p values are low
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)

#performs a hypothesis test with both models, the null hypothesis is that the 2 models fit the data equally well
par(mfrow=c(2,2))
plot(lm.fit2)
lm.fit5=lm(medv ~ poly(lstat,5)) #5th degree polynomial linear function
summary(lm.fit5)
#you can also do a log transfomration for some reason idk
 summary(lm(medv~log(rm),data=Boston))
install.packages("ISLR")
library(ISLR)
fix(Carseats)
names(Carseats)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
contrasts()
#returns the encoding R uses for shelveloc levels because its a factor variable
#because when you look at the coefficients in summary(lm.fit) theres ShleveLocGood ShelveLocMedium shelvelocbad is just the default
