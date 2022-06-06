#########################################################################
# Name: Sherry Khan
# STAT214 - Final Project
# Chronic and Blood Playthrough Homework Part 1 - Focus on Chronic
##########################################################################
library( tidyverse )
library(haven)
library(dplyr)
library(DT)

# Import dataset
chronic <- read_csv("chronic.csv")

# Bootstrapping by making 100 and 500 samples via n
subC_100 <- slice_sample( chronic, n=100 )

# Making 5000 folds
n_boot <- 5000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 

for( i in 1:n_boot ){
  idx <- sample( 1:nrow(subC_100), nrow(subC_100), replace = TRUE )
  boot <- subC_100[ idx, ]
  fit <- glm( Condition ~ Age, data=boot, family="binomial") 
  
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))
}

quantile( coefficients$Slope, probs = c(.025,.975))
# 2.5%      97.5% 
# 0.0261 0.0729

summary(coefficients)

# Results:
# Intercept - mean: -2.1597 median: -2.1120
# Slope - mean: 0.05116 median: 0.04986

# Creating a simple ggplot with said variables from boostrapping
ggplot( coefficients, aes(Slope)) + geom_histogram()

# n = 500
subC_1000 <- slice_sample( chronic, n=500 )

n_boot <- 5000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 

for( i in 1:n_boot ){
  idx <- sample( 1:nrow(subC_1000), nrow(subC_1000), replace = TRUE )
  boot <- subC_1000[ idx, ]
  fit <- glm( Condition ~ Age, data=boot, family="binomial") 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))
}

quantile( coefficients$Slope, probs = c(.025,.975))

# 2.5%      97.5% 
# 0.0456 0.0672

summary(coefficients)

# Results:
# Intercept - mean: -2.1597 median: -2.1120
# Slope - mean: 0.05116 median: 0.04986

# Creating a simple ggplot with said variables from boostrapping
ggplot( coefficients, aes(Slope)) + geom_histogram()

