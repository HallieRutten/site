#########################################################################
# Name: Sherry Khan
# STAT214 - Final Project
# Chronic and varBlood Playthrough Homework Part 2 - Focus on varBlood
##########################################################################
library( tidyverse )
library(haven)
library(dplyr)
library(DT)

# Import data set as XPT | tried csv, but hard to convert b/w the two
varBlood <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_CBC.XPT")

# Conducting linear regression:
ggplot( varBlood, aes(x=LBDLYMNO, y=LBXWBCSI)) +
  geom_point() +
  geom_smooth( method="lm", level=0)

# Since dataset variables have a linear relation + dropping NAs to calculate values of regg
varBlood <- varBlood %>%
  drop_na()

cor( varBlood$LBDLYMNO, varBlood$LBXWBCSI)
# strong correlation 0.9028913

varBlood.reg <- lm( LBDLYMNO ~ LBXWBCSI, data=varBlood)

summary( varBlood.reg) 

# Strong R-Squared - 82%, Intercept -2.90 and slope 0.73

# Bootstrapping by making 100 and 500 samples via n
subB_100 <- slice_sample( varBlood, n=100 )

# n - 5000
n_boot <- 5000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 

for( i in 1:n_boot ){
  idx <- sample( 1:nrow(subB_100), nrow(subB_100), replace = TRUE )
  boot <- subB_100[ idx, ]
  fit <- lm( LBDLYMNO ~ LBXWBCSI, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

quantile( coefficients$Slope, probs = c(0.025,.975))
# Results: 2.5%     97.5% 
# 0.1281304 0.2662945 

summary(coefficients)
# Results:  Median : 0.9380   Median :0.19806  
#           Mean   : 0.9392   Mean   :0.19813

# Plotting graph
ggplot( coefficients, aes(Slope)) + geom_histogram()


# n - 500
subB_1000 <- slice_sample( varBlood, n=500 )
n_boot <- 5000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 

for( i in 1:n_boot ){
  idx <- sample( 1:nrow(subB_1000), nrow(subB_1000), replace = TRUE )
  boot <- subB_1000[ idx, ]
  fit <- lm( LBDLYMNO ~ LBXWBCSI, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))}

quantile( coefficients$Slope, probs = c(0.025,.975))
# 2.5%     97.5% 
# 0.1963437 0.3114050
summary(coefficients)

# Results:  Median : 0.5685   Median :0.2527  
#           Mean   : 0.5649   Mean   :0.2530

# Plotting graph
ggplot( coefficients, aes(Slope)) + geom_histogram()

