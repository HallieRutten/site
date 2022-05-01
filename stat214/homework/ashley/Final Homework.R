# Final homework --> Bootstrapping 

# Motivation: Research shows that "Difficult to staff" schools (i.e., schools w/ a large proportion of minority & low-income students) have a lower distribution of "high-quality" teachers

# Data was downloaded from the Georgia Department of Education 



library(tidyverse)

library(haven)

school <- read_dta("georgiaschools.dta")

View(school)

summary(school)


# Generate 2 subsets (n = 100 & n = 1,000)

school_subset100 <- slice_sample( school, n=100 )

school_subset1000 <- slice_sample( school, n=1000 )



# Proxies for Teacher Quality
  # 1.) Experience 
  # 2.) Education level



# Proxy 1
  
# Relationship between experience and percentage black 
ggplot( school, aes(x=enroll_percent_black, y=avgyearsexp)) +
  geom_point() +
  geom_smooth( method="lm", level=0)

# Relationship between experience and percentage white 
ggplot( school, aes(x=enroll_percent_white, y=avgyearsexp)) +
  geom_point() +
  geom_smooth( method="lm", level=0)

# Relationship between experience and percentage Hispanic
ggplot( school, aes(x=enroll_percent_hispanic, y=avgyearsexp)) +
  geom_point() +
  geom_smooth( method="lm", level=0)

# Relationship between experience and percentage FRL
ggplot( school, aes(x=frl, y=avgyearsexp)) +
  geom_point() +
  geom_smooth( method="lm", level=0)

# All relationships look linear! 
# Okay to proceed with regressions!



# Regression 1 ---

cor( school$enroll_percent_black, school$avgyearsexp)
school.fit <- lm( avgyearsexp ~ enroll_percent_black, data=school)
summary( school.fit) 

# coefficient = -0.0310888***

# subset n = 100
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset100), nrow(school_subset100), replace = TRUE )
  boot <- school_subset100[ idx, ]
  fit <- lm( avgyearsexp ~ enroll_percent_black, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = (-0.07051735, -0.02274637)  // coefficient is contained in the 95% CI

# subset n = 1000
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset1000), nrow(school_subset1000), replace = TRUE )
  boot <- school_subset1000[ idx, ]
  fit <- lm( avgyearsexp ~ enroll_percent_black, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = (-0.03247507, -0.02207657) // coefficient is contained in the 95% CI


# Regression 2 ---

cor( school$enroll_percent_white, school$avgyearsexp)
school.fit <- lm( avgyearsexp ~ enroll_percent_white, data=school)
summary( school.fit) 

# coefficient = 0.04135***

# subset n = 100
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset100), nrow(school_subset100), replace = TRUE )
  boot <- school_subset100[ idx, ]
  fit <- lm( avgyearsexp ~ enroll_percent_white, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = (0.03949956, 0.08654891 )  // coefficient is contained in the 95% CI

# subset n = 1000
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset1000), nrow(school_subset1000), replace = TRUE )
  boot <- school_subset1000[ idx, ]
  fit <- lm( avgyearsexp ~ enroll_percent_white, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = (0.03441338, 0.04445485 ) // coefficient is contained in the 95% CI


# Regression 3 ---

cor( school$enroll_percent_hispanic, school$avgyearsexp)
school.fit <- lm( avgyearsexp ~ enroll_percent_hispanic, data=school)
summary( school.fit) 

# coefficient = -0.02407*** 

# subset n = 100
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset100), nrow(school_subset100), replace = TRUE )
  boot <- school_subset100[ idx, ]
  fit <- lm( avgyearsexp ~ enroll_percent_hispanic, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = (-0.08951639, -0.00641480 )  // coefficient is contained in the 95% CI

# subset n = 1000
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset1000), nrow(school_subset1000), replace = TRUE )
  boot <- school_subset1000[ idx, ]
  fit <- lm( avgyearsexp ~ enroll_percent_hispanic, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = (-0.03247507, -0.02207657) // coefficient is contained in the 95% CI


# Regression 4 ---

cor( school$frl, school$avgyearsexp)
school.fit <- lm( avgyearsexp ~ frl, data=school)
summary( school.fit) 

# coefficient = -0.0179778***

# subset n = 100
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset100), nrow(school_subset100), replace = TRUE )
  boot <- school_subset100[ idx, ]
  fit <- lm( avgyearsexp ~ frl, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = (-0.059902711, -0.002606241 )  // coefficient is contained in the 95% CI

# subset n = 1000
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset1000), nrow(school_subset1000), replace = TRUE )
  boot <- school_subset1000[ idx, ]
  fit <- lm( avgyearsexp ~ frl, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = (-0.020717553, -0.007713389 ) // coefficient is contained in the 95% CI




# Proxy 2

# Relationship between education level and percentage black 
ggplot( school, aes(x=enroll_percent_black, y=grad_degree)) +
  geom_point() +
  geom_smooth( method="lm", level=0)

# Relationship between education level and percentage white 
ggplot( school, aes(x=enroll_percent_white, y=grad_degree)) +
  geom_point() +
  geom_smooth( method="lm", level=0)

# Relationship between education level and percentage Hispanic
ggplot( school, aes(x=enroll_percent_hispanic, y=grad_degree)) +
  geom_point() +
  geom_smooth( method="lm", level=0)

# Relationship between education level and percentage FRL
ggplot( school, aes(x=frl, y=grad_degree)) +
  geom_point() +
  geom_smooth( method="lm", level=0)]



# Regression 1 ---

cor( school$enroll_percent_black, school$grad_degree)
school.fit <- lm( grad_degree ~ enroll_percent_black, data=school)
summary( school.fit) 

# coefficient = -0.0005649***

# subset n = 100
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset100), nrow(school_subset100), replace = TRUE )
  boot <- school_subset100[ idx, ]
  fit <- lm( grad_degree ~ enroll_percent_black, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = (-0.0019979164, -0.0001923512 )  // coefficient is contained in the 95% CI

# subset n = 1000
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset1000), nrow(school_subset1000), replace = TRUE )
  boot <- school_subset1000[ idx, ]
  fit <- lm( grad_degree ~ enroll_percent_black, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = (-0.0008305160, -0.0003510818) // coefficient is contained in the 95% CI


# Regression 2 ---

cor( school$enroll_percent_white, school$grad_degree)
school.fit <- lm( grad_degree ~ enroll_percent_white, data=school)
summary( school.fit) 

# coefficient = 0.0006872****

# subset n = 100
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset100), nrow(school_subset100), replace = TRUE )
  boot <- school_subset100[ idx, ]
  fit <- lm( grad_degree ~ enroll_percent_white, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = (0.0007601807, 0.0024951328)  // coefficient is contained in the 95% CI

# subset n = 1000
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset1000), nrow(school_subset1000), replace = TRUE )
  boot <- school_subset1000[ idx, ]
  fit <- lm( grad_degree ~ enroll_percent_white, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = (0.0004622636, 0.0009599511) // coefficient is contained in the 95% CI


# Regression 3 ---

cor( school$enroll_percent_hispanic, school$grad_degree)
school.fit <- lm( grad_degree ~ enroll_percent_hispanic, data=school)
summary( school.fit) 

# coefficient = -0.0001588**

# subset n = 100
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset100), nrow(school_subset100), replace = TRUE )
  boot <- school_subset100[ idx, ]
  fit <- lm( grad_degree ~ enroll_percent_hispanic, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = (-0.003284177, 0.00007670688)  // coefficient is contained in the 95% CI

# subset n = 1000
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset1000), nrow(school_subset1000), replace = TRUE )
  boot <- school_subset1000[ idx, ]
  fit <- lm( grad_degree ~ enroll_percent_hispanic, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = (0.0006755952, 0.0001045092) // coefficient is contained in the 95% CI


# Regression 4 ---

cor( school$frl, school$grad_degree)
school.fit <- lm( grad_degree ~ frl, data=school)
summary( school.fit) 

# coefficient = -0.0005861***

# subset n = 100
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset100), nrow(school_subset100), replace = TRUE )
  boot <- school_subset100[ idx, ]
  fit <- lm( grad_degree ~ frl, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = (-0.0017347463, 0.0003223224 )  // coefficient is contained in the 95% CI

# subset n = 1000
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(school_subset1000), nrow(school_subset1000), replace = TRUE )
  boot <- school_subset1000[ idx, ]
  fit <- lm( grad_degree ~ frl, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

# 95% CI = (-0.0008951959, -0.0003585596 ) // coefficient is contained in the 95% CI



# Multiple Linear Regression Model (experience)

# Proxy 1 

experience <- lm(avgyearsexp ~ frl + enroll_percent_black + enroll_percent_white + enroll_percent_hispanic, data = school)
summary(experience)

# Coefficients ---
# frl                      0.011193***
# enroll_percent_black    -0.005741 
# enroll_percent_white     0.040763***
# enroll_percent_hispanic -0.011392** 


# Proxy 2 

education <- lm(grad_degree ~ frl + enroll_percent_black + enroll_percent_white + enroll_percent_hispanic, data = school)
summary(education)

# Coefficients ---
# frl                      0.01119***
# enroll_percent_black     0.001278*** 
# enroll_percent_white     0.001764***
# enroll_percent_hispanic  0.001466*** 



