# Name: Sherry Khan
# Class: STAT214
# HW: Violets
# Date: February 23rd, 2022

library(readr)
library(dplyr)
library(tidyverse)
library(foreign)

violets <- read_csv("violets.csv")

chase <- violets %>%
  select (`Bud counts`, Photoperiod, `Bud type`, ) %>%
  filter (`Bud type` == 'Chasmogamous')

chase <- chase %>%
  mutate (budc = as.integer(`Bud counts` > 0))

chase.plot <- ggplot(
  chase,aes(Photoperiod, budc)) +
  geom_point()
chase.plot

# Assorting logistic curve to fit chase's data
ggplot(chase, aes(Photoperiod, budc)) + 
  geom_smooth(method = glm,
              method.args = list(family = binomial),
              size = 2) +
  geom_point()

chase.fit <- glm(budc ~ Photoperiod,
                 data = chase,
                 family = "binomial")
summary(chase.fit)

# Calculating deviance to check likelihood
G1 <- chase.fit$null.deviance - chase.fit$deviance
G1
# Calculating p value
valuep <- pchisq(G1,1,lower.tail = FALSE)
valuep
# Calculating Threshold
chaseco <- -chase.fit$coefficients[1] / chase.fit$coefficients[2]
chaseco
