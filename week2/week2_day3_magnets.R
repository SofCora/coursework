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

#questions 10.2
#questions 1: Compute the proportion in the sample of those with a high level of blood pressure
population <- read_csv("pop2.csv")
sample <- read_csv("ex2.csv")
sample_2 <- sample |> group_by(group) |> summarize(count = n()) |> mutate(prop = count/nrow(sample)) |> filter(group == "HIGH")
view(sample_2)
#.2467 proportion

 #2. Compute the proportion in the population of those with a high level of
 #blood pressure.
population_2 <- population |> group_by(group) |> summarize(count = n()) |> mutate(prop = count/nrow(population))
view(population_2)
#.28 proportion
#3. Simulate the sampling distribution of the sample proportion and compute its expectation.
x_proportions <- rep(0,10^2)
for(i in 1:10^2)
{
  x <- sample_n(population, size = 150, replace = TRUE)
  x <- x |> group_by(group) |> summarize(count = n()) |> mutate(prop = count/nrow(x)) |> filter(group == "HIGH") 
  view(x)
  x_proportions[i] = x$prop
}
p0_mean = mean(x_proportions)
#p0 expectation is .28132


#4. Compute the variance of the sample proportion.
p0_var = var(x_proportions)
print(p0_var)
#variance is .001

#5. It is proposed in Section 10.5 that the variance of the sample proportion is Var( P) = p(1 -p)/ n, where p is the probability of the event (having a
#high blood pressure in our case) and n is the sample size (n = 150 in our case).
# Examine this proposal in the current setting.
prob_of_high <- mean(pop2$group == "HIGH") #this works as counting the number of highs and / total number of observations in the population to see the probability of getting a HIGH
var_p0 <- (prob_of_high*(1-prob_of_high)) / 150 
print(var_p0)


#isrs textbook questions 2.2 and 2.6
#question 2.2 What proportion of patients in the treatment groupand what proportion of patients in the control group died?
#control group 30/34 = .8823 died in treatment group 45/69 died = .65217
#page 115 b 1
#h0  = The difference in proportion of deaths can be due to chance 
#ha = the proportion of deaths in the treatment group is statstically larger than the proportion in the control
# b ii 
# we write alive on the 28 cars representing paitents who were alive at the end
#of the study and dead on the 75 cards who were not. then we shuffle these cards 
#and split them into 2 groups, 1 of size 69 representing treamtent and 1 group of size 34 representing control
#we calculate the idff between proprtion od dead cards in treatment vs control groups
# we repeat this many times to build a distribution centered at [  0  ] lastly we calculate the fraction of 
#simulations where the simulated differences in proprtions are  [  as extreme as what we observed (.88-.65 = .23)  ] if this fraction is low, 
#we conclude it is unlikely to have observed such an outcome by chance and the null hypothesis must be rejected...

#b iii that is highly unlikely that we would get a difference in means where the treatment group is .23 or more higher than control due to chance


#2.6 yawning question
#2.6.a h0 is having seomeon yawn near another person has no influence on whether the other will yawn
# ha = is having someone yawn will influence the people around them to also yawn subconsciously and this is not by chance

# 2.6.b 10/34 =.29 yawning rate for treatment group, 4/16 = .25 yawning rate for control group difference is .04

#2.6.c im not sure its pretty high though idk what the scale is for the 
#fail to reject the null hypothesis 
mean(replicate(1e4, prop.test(x = sum(rbinom(100, 1, 0.51)), n = 100, alternative = "greater")$p.value <= 0.05))
#for the record i think number 5 on the quiz is true

#9.2 questions
#we're looking for the confidence interval if both expected values are 3.5 using the test statistic given
#in the question
mu <- 3.5
mu2 <- 3.5
sig <- 3
sig2 <- 3.5
test_stat <- rep(0,10^5)
se <- rep(0,10^5)
for(i in 1:10^5)
{
  x1 <- rnorm(29,mu,sig)
  x1_mean <- mean(x1)
  x1_sig <- sd(x1)

  x2 <- rnorm(21,mu2,sig2)
  x2_mean <- mean(x2)
  x2_sig <- sd(x2)

  test_stat[i] = (x1_mean - x2_mean)/sqrt((x1_sig**2)/29 + (x2_sig**2)/21) 
  se[i] = sqrt((x1_sig**2)/29 + (x2_sig**2)/21)
}
print(mean(test_stat))
print(mean(se)*2)
#i feel like its supposed to be +/- standard error but my answer is super different

