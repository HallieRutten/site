#########################################################################
# Name: Sherry Khan
# Date: May 1st, 2022
# STAT214 - Final Project
# Used a Singapore based real estate data set to look at differences in
# per unit area pricing of houses
# Main features of the data set - distance from train, and age of house
##########################################################################

# Importing libraries
library(tidyverse)
library(haven)
library(dplyr)
library(modelr)
library(DT)

# Importing data set
houses <- read_csv("RealEstateFinal.csv")

# Selecting columns of interest
houses <- houses %>%
  dplyr::select( purchaseDate, ageHouse, distanceMRT, unitAreaPrice)

# Plotting a plot to identify relationship between the unit Area price and age of house
ggplot( houses, aes(x=ageHouse, y=unitAreaPrice)) +
  geom_point(color='darkblue') +
  geom_smooth( method="lm", level=0) +
  ggtitle("Graph for relation b/w Price of each unit Area vs Age of House") + xlab("Age of the House") + ylab("Price of each Unit Area")

# Plotting a plot to identify relationship between the unit Area price and distance between MTR train
ggplot( houses, aes(x=distanceMRT, y=unitAreaPrice)) +
  geom_point(color = "red") +
  geom_smooth( method="lm", level=0) +
  ggtitle("Graph for relation b/w Price of each unit Area vs distance from MRT") + xlab("Age of the House") + ylab("Price of each Unit Area")

# Building 2 different linear regression models for MRT AND AGE:

# Linear regression model - age of house and unit Area Price
cor( houses$unitAreaPrice, houses$ageHouse)
houses.reg <- lm( unitAreaPrice ~ ageHouse, data=houses)

summary( houses.reg) 

# Correlation: -0.210567 (as age of house increases, unit area price of the house decreases and vice versa)
# y = mx+c - this means that on average the per unit area costs $42.4, and as age of house increase by 1 unit,
# price of each unit area decreases by 0.3.
# Since the R-squared value is very low, 4% this model is not a very good fit of the data

# Linear regression model - distance from MTR and unit Area Price
cor( houses$unitAreaPrice, houses$distanceMRT)
houses.reg <- lm( unitAreaPrice ~ distanceMRT, data=houses)

summary( houses.reg) 

# Correlation: -0.6736129 (as distance from MRT (train) increases, the unit area price of house decreases and vice versa)
# This negative correlation is stronger than that of age of house to its price.
# y = mx+c - this means that on average the per unit area costs $45.85, and as age of house increase by 1 unit,
# price of each unit area decreases by 0.0073 units.
# Since the R-squared value is 45%, this model is a better fit of the data than the age dependancy

##########################################################################################################################

# Now that regression is done, we can proceed with Bootstrapping and Cross Validation of the regression!

##########################################################################################################################

# Creating 2 different bootstrapping samples - 100
houses.div100 <- slice_sample( houses, n=100 )

# subset n = 100
# Make 5000 different sets of the data
n_boot <- 5000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 

for( i in 1:n_boot ){
  idx <- sample( 1:nrow(houses.div100), nrow(houses.div100), replace = TRUE )
  boot <- houses.div100[ idx, ]
  fit <- lm( unitAreaPrice ~ distanceMRT, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))
summary(coefficients)

# -0.00932647 -0.00600448 - 95% CI

# Intercept: mean - 44.72, median: -0.007296
# Slope: mean: 44.70, median: -0.007163 

# Sampling with 400 different samples
houses.div400 <- slice_sample( houses, n=400 )

# subset n = 300
n_boot <- 5000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 

for( i in 1:n_boot ){
  idx <- sample( 1:nrow(houses.div400), nrow(houses.div400), replace = TRUE )
  boot <- houses.div400[ idx, ]
  fit <- lm( unitAreaPrice ~ distanceMRT, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

summary(coefficients)
# Intercept: Median :45.88   Median : 45.89
# Slope: Mean: -0.007234     :45.89   Mean   :-0.007244  

# Setting the quantile to between 2.5 and 97.5%
quantile( coefficients$Slope, probs = c(0.025,.975))

# We are 95% confident that the slope value lies between these 2 points: -0.008258898 -0.006704252

# Boostrap the second regression now:
# subset n = 100
n_boot <- 5000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 

for( i in 1:n_boot ){
  idx <- sample( 1:nrow(houses.div100), nrow(houses.div100), replace = TRUE )
  boot <- houses.div100[ idx, ]
  fit <- lm( unitAreaPrice ~ ageHouse, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

summary(coefficients)

# Intercept: Median :37.02   Mean : 36.95  
# Slope: Median: 0.002129 Mean: 0.003481


# subset n = 300
n_boot <- 5000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 

for( i in 1:n_boot ){
  idx <- sample( 1:nrow(houses.div400), nrow(houses.div400), replace = TRUE )
  boot <- houses.div400[ idx, ]
  fit <- lm( unitAreaPrice ~ ageHouse, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

summary(coefficients)

# Intercept: Median: 42.67 Mean: 42.67
# Slope: Median: -0.2505 Mean: -0.2504

# Conducting a Cross-Validation on the dataset with 10 different sub-sets
cv  <- crossv_kfold(houses, k = 10)
cv

models1  <- map(cv$train, ~lm(unitAreaPrice ~ ageHouse, data = .))
models2  <- map(cv$train, ~lm(unitAreaPrice ~ distanceMRT, data = .))

prediction  <- function(model, test_data){
  data  <- as.data.frame(test_data)
  pred  <- add_predictions(data, model)
  return(pred)
}
pred01  <- map2_df(models1, cv$test, prediction, .id = "Run")
pred02  <- map2_df(models2, cv$test, prediction, .id = "Run")

model1  <- pred01 %>% group_by(Run) %>%
  summarise(MSE = mean( (unitAreaPrice - pred)^2)) 

model1

model2  <- pred02 %>% group_by(Run) %>%
  summarise(MSE = mean( (unitAreaPrice - pred)^2)) 

model2

summary(model1)
summary(model2)


# Conclusion: both of these factors impact the prices of the per unit area land 
# However, distance from the train affects it much more!