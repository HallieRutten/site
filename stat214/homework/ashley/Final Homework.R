# Final homework --> Bootstrap 

# Data was downloaded from the Georgia Department of Education 


#### Maybe group by rural and urban if possible (not sure if data is available from GOSA)
# In presentation, maybe mention there were data limitations
# FRL
# Bootstrapping but maybe add other variables (i.e., salary, FRL, both enrollment)

library(tidyverse)

library(haven)

school <- read_dta("Desktop/Research/Thesis/Stata Files/final.dta")

View(school)

school <- school %>%
  select( avgannualsalary, avgyearsexp, enrollblack, enrollwhite) %>%
  rename(salary = avgannualsalary) %>%
  rename(exper = avgyearsexp) %>%
  rename(black = enrollblack) %>%
  rename(white = enrollwhite)

# Relationship between salary and experience 
# Teachers in GA are paid on a tiered pay scale using experience and degree level
ggplot( school, aes(x=exper, y=salary)) +
  geom_point() +
  geom_smooth( method="lm", level=0)

# Relationship between experience and percentage black 
# Research shows that "Difficult to staff" schools (i.e., schools w/ a large proportion of minority & low-income students) have a lower distribution of "high-quality" teachers
ggplot( school, aes(x=black, y=exper)) +
  geom_point() +
  geom_smooth( method="lm", level=0)

# Relationship between experience and percentage white 
ggplot( school, aes(x=white, y=exper)) +
  geom_point() +
  geom_smooth( method="lm", level=0)

# All three appear to have a linear relationship! 
# Safe to proceed with regression!

# Generate 2 subsets (n = 100 & n = 1,000)
school_subset100 <- slice_sample( school, n=100 )
school_subset1000 <- slice_sample( school, n=1000 )

# Salary & experience ----

# Regression 1 
cor( school$exper, school$salary)
school.fit <- lm( salary ~ exper, data=school)
summary( school.fit) 

# coefficient = 924.91

# subset n = 100
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset100), nrow(school_subset100), replace = TRUE )
  boot <- school_subset100[ idx, ]
  fit <- lm( salary ~ exper, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = 434.0727, 1349.7450 // coefficient is contained in the 95% CI

# subset n = 1000
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset1000), nrow(school_subset1000), replace = TRUE )
  boot <- school_subset1000[ idx, ]
  fit <- lm( salary ~ exper, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = 801.0063, 1212.4066 // coefficient is contained in the 95% CI


# Experience & percent black ----

# Regression 2
cor( school$black, school$exper)
school.fit <- lm( exper ~ black, data=school)
summary( school.fit) 

# coefficient = -0.0216436

# subset n = 100
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset100), nrow(school_subset100), replace = TRUE )
  boot <- school_subset100[ idx, ]
  fit <- lm( exper ~ black, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = -0.038094162, -0.007633547 // coefficient is contained in the 95% CI

# subset n = 1000
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset1000), nrow(school_subset1000), replace = TRUE )
  boot <- school_subset1000[ idx, ]
  fit <- lm( exper ~ black, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = -0.02641537, -0.01714326  // coefficient is contained in the 95% CI


# Experience & percent white ----

# Regression 3
cor( school$white, school$exper)
school.fit <- lm( exper ~ white, data=school)
summary( school.fit) 

# coefficient = 0.03746

# subset n = 100
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset100), nrow(school_subset100), replace = TRUE )
  boot <- school_subset100[ idx, ]
  fit <- lm( exper ~ white, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = 0.006877431, 0.042076811  // coefficient is contained in the 95% CI

# subset n = 1000
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset1000), nrow(school_subset1000), replace = TRUE )
  boot <- school_subset1000[ idx, ]
  fit <- lm( exper ~ white, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = 0.03524164, 0.04494746  // coefficient is contained in the 95% CI



