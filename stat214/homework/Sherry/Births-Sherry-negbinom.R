#########################################################################
# Name: Sherry Khan
# STAT214 - Final Project
# Regression-NegBinom Homework
##########################################################################

library(tidyverse)
library(kableExtra)
library(tidyverse)
library(gridExtra)
library(DT)
library(MASS)

# reading in the data
births <- read.csv("births.csv")

# formatting the data
births$Pop2010 <- births$Pop2010/1000

births_by_county <- births %>%
  group_by(county) %>% 
  summarize( Mean = mean(births),
             Variance = var(births),
             Ratio = Variance/Mean ) %>% 
  arrange( desc(Ratio))

# January 2016 - Sequencing
JANUARY2016 <- births %>% 
  filter( month=="January" & year==2016)

# Graph Plotting
ggplot( JANUARY2016, aes( x=Pop2010, y=births)) +
  geom_point() +
  geom_smooth( method="glm.nb" )

JANUARY2016_Comp <- glm.nb( births ~ Pop2010, data=JANUARY2016)

# Log
JANUARY2016$Pop2010 <- log( JANUARY2016$Pop2010 )

# Graph Plotting
ggplot( JANUARY2016, aes( x=Pop2010, y=births)) +
  geom_point() +
  geom_smooth( method="glm.nb" )

JANUARY2016_Comp <- glm.nb( births ~ Pop2010, data=JANUARY2016)

# Mobile County 
mobile <- births %>% 
  filter( county == "Mobile County, AL")

# MU Predictor
new_data <- data.frame( Pop2010 = log(412.992))
mu <- predict( JANUARY2016_Comp, newdata = new_data, type="response")

mobile <- mobile %>% 
  mutate( simulation = rnbinom( nrow(mobile), mu=mu[1], size=JANUARY2016_Comp$theta))

ggplot( mobile %>% dplyr::select( births, simulation) %>% pivot_longer(births:simulation), 
        aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol=1) +
  labs( title="Mobile County, AL")

# Henry County
henry <- births %>% 
  filter( county == "McHenry County, IL")

# MU Predictor
new_data <- data.frame( Pop2010 = log(100.157))
mu <- predict( JANUARY2016_Comp, newdata = new_data, type="response")

# Data for Henry County
henry <- henry %>% 
  mutate( simulation = rnbinom( nrow(henry), mu=mu[1], size=JANUARY2016_Comp$theta))

ggplot( henry %>% dplyr::select( births, simulation) %>% pivot_longer(births:simulation), 
        aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol=1) +
  labs( title="McHenry County, IL")

