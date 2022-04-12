# in class 4/12
# exercise 1 ----

# a) using chronic dataset, create two random subsets, one of size 100 and one of size 1000
# b) for each subset, apply the bootstrap to get 95% confidence intervals for logistic regression coefficients
# c) compare confidence intervals to ground truth using all data

library( tidyverse )
library(haven)
chronic <- read_csv("chronic.csv")

chronic_subset_100 <- slice_sample( chronic, n=100 )
chronic_subset_1000 <- slice_sample( chronic, n=1000 )

# for 100 sample ----
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(chronic_subset_100), nrow(chronic_subset_100), replace = TRUE )
  boot <- chronic_subset_100[ idx, ]
  fit <- glm( Condition ~ Age, data=boot, family="binomial") 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))
}

quantile( coefficients$Slope, probs = c(.025,.975))
ggplot( coefficients, aes(Slope)) + geom_histogram()
# interval is from .0307 to .0843

chronic.fit <- glm( Condition ~ Age, data=chronic, family = "binomial")
summary(chronic.fit) 
# .052 contained within confidence interval

# for 1000 sample ----
n_boot <- 1000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(chronic_subset_1000), nrow(chronic_subset_1000), replace = TRUE )
  boot <- chronic_subset_1000[ idx, ]
  fit <- glm( Condition ~ Age, data=boot, family="binomial") 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))
}

quantile( coefficients$Slope, probs = c(.025,.975))
ggplot( coefficients, aes(Slope)) + geom_histogram()
# interval is from .0436 to .0576

chronic.fit <- glm( Condition ~ Age, data=chronic, family="binomial")
summary(chronic.fit) 
# .052 contained within confidence interval


# exercise 2 ----

# a) using nhanes, create random subsets of size 100 and 1000
# b) for each, apply the bootstrap to get 95% confidence intervals for linear regression coefficients
# c) compare confidence intervals to ground truth using all data

blood <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_CBC.XPT")

blood <- blood %>%
  rename(WBC = LBXWBCSI) %>%
  rename(LCN = LBDLYMNO)

blood_subset_100 <- slice_sample( blood, n=100 )
blood_subset_1000 <- slice_sample( blood, n=1000 )

# for 100 sample (blood) ----
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(blood_subset_100), nrow(blood_subset_100), replace = TRUE )
  boot <- blood_subset_100[ idx, ]
  fit <- lm( LCN ~ WBC, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

quantile( coefficients$Slope, probs = c(0.025,.975))
ggplot( coefficients, aes(Slope)) + geom_histogram()
# interval is from .1324 to .510

blood.fit <- lm( LCN ~ WBC, data=blood)
summary( blood.fit) 
# 0.7347 not contained within confidence interval

# for 1000 sample (blood) ----
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(blood_subset_1000), nrow(blood_subset_1000), replace = TRUE )
  boot <- blood_subset_1000[ idx, ]
  fit <- lm( LCN ~ WBC, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

quantile( coefficients$Slope, probs = c(0.025,.975))
ggplot( coefficients, aes(Slope)) + geom_histogram()
# interval is from .2518 to .3302

blood.fit <- lm( LCN ~ WBC, data=blood)
summary( blood.fit) 
# 0.7347 not contained within confidence interval

