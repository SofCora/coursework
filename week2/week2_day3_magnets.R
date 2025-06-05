#Sofia Cora Week 2 Day 3, 6/4/25
#page 162 
library(tidyverse)
library(scales)
theme_set(theme_bw())
magnets <- read_csv("magnets.csv")
summary(magnets)
magnets <- magnets |> mutate(avg = mean(change))
#question 1: the average difference between score 1 and 2 is 3.5
str(magnets)
#question 2: the active column type is a factor
view(magnets |> mutate(average_active = mean(change[1:29])) |> mutate(average_placebo = mean(change[30:50])))
#question 3: average change for paitents who received the active ingredient is 5.2 while 
#the average change for paitents who received placebo is 1.09
view(magnets |> group_by(active) |> mutate(sd = sd(change)))
#question 4: sd for active group is 3.23 and sd for placebo group is 1.57
ggplot(magnets, aes(x = active, y = change, fill = active)) +
  geom_boxplot() +
  coord_flip()
#question 5 :theres more outliers for the placebo group

#chapter 10 questions
#page 183
# Simulate the sampling distribution of average and the median of a sample
#of size n = 100 from the Normal(32) distribution. Compute the expectation
#and the variance of the sample average and of the sample median.
#Which of the two estimators has a smaller mean square error?
mu <- 3
sig <- sqrt(2)
x_bar <- rep(0,10^5) #initalizing an empty vector of 0s that are filled in with the for loop
mid_range <- rep(0,10^5)
for(i in 1:10^5)
{
    x <- rnorm(100,mu,sig) #n=100 this is doing 1 replication of sample size n
    x_bar[i] <- mean(x)
    mid_range[i] <- (max(x) + min(x)) / 2
}
var(x_bar) #x_bar is a vector of means from each replication
var(mid_range) #mid_range is a vector of medians from each replication
print(x_bar)
#var is the same as mse it takes the true mean value 

mean(x_bar) #mean of means 
mean(mid_range) # mean of medians
#question 1: if the distribution is normal it would be symmetric thus the 
#estimators woukld be = to expectations  it was pretty close 3.0005 for mean and 3.001 for median??? 
#variance of x_bar is .019 variance of mid range is .1859
#which of the two estimators has a smaller mean square error? x_bar 
#MSE is same as variance
#variance is a statistical measurement of the spread between numbers in a dataset


#10.1.2 uniform 
x_bar <- rep(0,10^5) #initalizing an empty vector of 0s that are filled in with the for loop
mid_range <- rep(0,10^5)
for (i in 1:10^5)
{
    x <- runif(100,.5,5.5)
    x_bar[i] <- mean(x)
    mid_range[i] <- (max(x) + min(x)) / 2  
}
mean(x_bar)
mean(mid_range)

var(x_bar)
var(mid_range)
#median has a smaller MSE (variance)

runif(8, 1, 4)
