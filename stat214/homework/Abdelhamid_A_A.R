library(tidyverse)
library(nhanesA)


meas <- read.xport("P_BMX.XPT") #Reading medical measurements from NHANES 

# BMXWT is the weight in lbs on the x-axis
# BMXWAIST is the  waist circumference in inches on the y-axis

meas.plot <- ggplot( meas, aes(BMXWT, BMXWAIST)) + geom_point()

( y.bar <- mean( meas$BMXWAIST ) )
ggplot( meas, aes(BMXWAIST)) + geom_histogram( binwidth=3 )

gather( meas) %>% 
  group_by( key ) %>% 
  summarize( Average = mean(value),
             SD = sd(value))

meas.plot + 
  geom_hline( yintercept = y.bar, color ="red", linetype="dashed" ) +
  geom_vline( xintercept = mean( meas$BMXWAIST), color="blue", linetype="dashed")
( meas.r <- cor( meas$BMXWT, meas$BMXWAIST ))


             

meas.fit <- lm( BMXWAIST ~ BMXWT, data=meas)
meas.fit
summary( meas.fit )
sigma( meas.fit ) 
meas.fit$coefficients
meas.fit$fitted.values

summary( meas.fit )