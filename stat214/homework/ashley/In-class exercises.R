# the bootstrap (developed by Bradley Efron in the 70s)

# general method:
# (1) create a bootstrap sample: sample the available data with replacement
#     to create a dataset of the same size
# (2) compute whatever sample statistic you care about for your bootstrap sample 
#     (e.g., sample mean, linear regression coefficient, etc.)
# (3) repeat many many many many times
# (4) visualize/analyze the resulting approximation of the sampling distribution

library(tidyverse)
library(readr)

# Exercise 1 - chronic.csv
# (1) create 2 random subsets (one of size 100 & one of size 1,000)
# (2) for each subset, apply the bootstrap to get 95% confidence intervals for logistic regression coefficients 
# (3) for each subset build 2,000 bootstrap samples
# (4) compare confidence interval results to ground truth using all data
# cross_validation.r works with chronic.csv

chronic <- read_csv("Desktop/Easter/Stats/Datasets/chronic.csv")

# generate small subset for CV experiments
chronic_subset100 <- slice_sample( chronic, n=100 )

# set number of bootstrap samples
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(chronic_subset100), nrow(chronic_subset100), replace = TRUE )
  boot <- chronic_subset100[ idx, ]
  fit <- glm( Condition ~ Age, data=boot, family="binomial") 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

# calculate 95% confidence interval (95% CI = 0.03849223, 0.08746758)
quantile( coefficients$Slope, probs = c(0.025,.975))

# plot 
ggplot(coefficients, aes(Slope)) + geom_histogram()

# compare CI results to ground truth 

chronic.fit <- glm( Condition ~ Age, data=chronic, family="binomial")
summary( chronic.fit) 

# coefficient = 0.0518152 (contained in the 95% CI)

# repeat for subset n = 1000

chronic_subset1000 <- slice_sample( chronic, n=1000 )

n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(chronic_subset1000), nrow(chronic_subset1000), replace = TRUE )
  boot <- chronic_subset1000[ idx, ]
  fit <- glm( Condition ~ Age, data=boot, family="binomial") 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

quantile( coefficients$Slope, probs = c(0.025,.975))

ggplot(coefficients, aes(Slope)) + geom_histogram()

chronic.fit <- glm( Condition ~ Age, data=chronic, family="binomial")
summary( chronic.fit) 

# 95% CI = (0.04155940, 0.05579742) 

# coefficient = 0.0518152 (contained in the 95% CI)

library(tidyverse)
library(haven)

# Exercise 2 - nhanes
# (1) create random subsets (one of size 100 & one of size 1,000)
# (2) for each, apply the bootstrap to get 95% confidence intervals for linear regression coefficients 
# (3) for each subset build 2,000 bootstrap samples
# (4) compare confidence interval results to ground truth using all data
# concrete is an example of a linear regression model

blood <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_CBC.XPT")

# rename variables 
blood <- blood %>%
  rename(WBC = LBXWBCSI) %>%
  rename(LCN = LBDLYMNO)

# generate small subset for CV experiments
blood_subset100 <- slice_sample( blood, n=100 )

# set number of bootstrap samples
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(blood_subset100), nrow(blood_subset100), replace = TRUE )
  boot <- blood_subset100[ idx, ]
  fit <- lm( LCN ~ WBC, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

# calculate 95% confidence interval (95% CI = 0.1846320, 0.3809433)
quantile( coefficients$Slope, probs = c(0.025,.975))

# plot 
ggplot(coefficients, aes(Slope)) + geom_histogram()

blood.fit <- lm( LCN ~ WBC, data=blood)
summary( blood.fit) 

# coefficient = 0.734654 (not contained in the 95% CI)

# repeat for subset n = 1000
blood_subset1000 <- slice_sample( blood, n=1000 )

# set number of bootstrap samples
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(blood_subset1000), nrow(blood_subset1000), replace = TRUE )
  boot <- blood_subset1000[ idx, ]
  fit <- lm( LCN ~ WBC, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

# calculate 95% confidence interval (95% CI = 0.1996892, 0.7852561)
quantile( coefficients$Slope, probs = c(0.025,.975))

ggplot(coefficients, aes(Slope)) + geom_histogram()

blood.fit <- lm( LCN ~ WBC, data=blood)
summary( blood.fit) 

# coefficient = 0.734654 (contained in the 95% CI)
