# Name: Sherry Khan
# Class: STAT214
# HW: NHANES Data
# Date: February 14th, 2022

# importing the libraries
library(tidyverse)
library(haven)
library(foreign)
library(dplyr)

# importing weight data from years 2017-2018
whdata <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/WHQ_I.XPT")

# cleaning up data
whclean <- whdata %>%
  select(WHD010, WHD020) %>%
  filter(WHD010 < 75, WHD010 > 50, WHD020 < 450)

whclean <- whclean %>%
  rename(Height = WHD010) %>%
  rename(Weight = WHD020)

# Calculating means using the mean function
heightm <- mean(whclean$Height)
weightm <- mean(whclean$Weight)

# Calculating Standard Deviations using the sdfunction
heightsd <- sd(whclean$Height)
weightsd <- sd( whclean$Weight)

# Gradient/Slope of the standard deviations
gradstd <- (weightsd/heightsd)

# Computing group averages to plot
whgroup <- whclean %>% 
  group_by(Height) %>%
  summarize(groupm = mean(Weight),
            SD = sd(Weight))

# plotting the clean data set
whplot <- ggplot(whclean,aes(Height, Weight)) +
  geom_point() +
  geom_smooth(method = "lm", level = 0)
whplot

# Plotting the calculated group averages
nor.plot<-ggplot(whclean, aes(x=Height, y=Weight)) +
  geom_point() +
  geom_hline( yintercept = weightm, color = "green", linetype = "dashed") +
  geom_point( data = whgroup, aes(x = Height, y = groupm), color = "red")
nor.plot

# Calculating correlation between Height and Weight
cor(whclean$Height, whclean$Weight)

# defining a linear regression (lm) model
whlinm<-lm(Height ~ Weight, whclean) 
summary(whlinm) 

