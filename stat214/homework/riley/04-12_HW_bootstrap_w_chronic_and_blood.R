# Riley Barrett
# 12 Apr 2022

# Setup -----
library(tidyverse)
setwd("/home/rileybarrettfg/214")

# Make a bunch of functions to automate common parts -----
# Pull a random subset of a specified size
randomSubset <- function( data, size ){
  result <- sample( 1:nrow(data), size, replace = FALSE )
  result <- data[ result, ]
  result
}

# Pull a bootstrap sample by drawing with replacement
bootstrapSample <- function( data ){
  result <- sample( 1:nrow(data), nrow(data), replace = TRUE )
  result <- data[ result, ]
  result
}

bootstrapCoefficients_List <- function( subset, nBoot, fitType ){
  coefficients <- data.frame( Intercept = c(), Slope = c() )
  
  for( i in 1:nBoot ){
    bSample <- bootstrapSample( subset )
    
    if( fitType == "glm" ){
      fit <- glm( bSample )$coefficients
    } else if( fitType == "lm" ){
      fit <- lm( bSample )$coefficients
    }
    
    coefficients <- rbind( coefficients,
                           data.frame( Intercept = fit[1],
                                       Slope = fit[2]))
  }
  
  coefficients
}

# Function for the entire process
bootstrapConfidenceInterval <- function( data, size, nBoot, fitType ){
  subset <- randomSubset( data, size )
  
  coefficients <- bootstrapCoefficients_List( subset, nBoot, fitType )
  
  intercepts_interval <- quantile( coefficients$Intercept, probs = c(0.05, 0.95) )
  conditions_interval <- quantile( coefficients$Slope, probs = c(0.05, 0.95) )
  
  confIntdf <- data.frame("Intercept"=intercepts_interval, "Slope"=conditions_interval)
  confIntdf <- t(confIntdf) # In case we need to transpose
  
  confIntdf
}

# Exercise 1 -----
chronic <- read_csv("datasets/chronic.csv")

# Run above functions and return results
chronic_bootstrap_with_100 <- bootstrapConfidenceInterval( chronic, 100, 2000, "glm" )
chronic_bootstrap_with_100

chronic_bootstrap_with_1000 <- bootstrapConfidenceInterval( chronic, 1000, 2000, "glm" )
chronic_bootstrap_with_1000


# Exercise 2 -----
blood <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_CBC.XPT")

# Filter six outliers, select used columns, and drop NA values
blood <- blood %>%
  filter(LBXWBCSI < 24) %>%
  select(LBXWBCSI, LBDLYMNO) %>%
  drop_na()

# Finally rename the columns
blood <- blood %>%
  rename(WBC = LBXWBCSI) %>%
  rename(LCN = LBDLYMNO)

# Run above functions and return results
blood_bootstrap_with_100 <- bootstrapConfidenceInterval( blood, 100, 2000, "lm" )
blood_bootstrap_with_100

blood_bootstrap_with_1000 <- bootstrapConfidenceInterval( blood, 1000, 2000, "lm" )
blood_bootstrap_with_1000









