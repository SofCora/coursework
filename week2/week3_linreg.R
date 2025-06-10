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