
# Chapter 1 mini-project 
########################

# Note the liberal use of summary( nhanes2013.clean ) to check results as you go!

library(tidyverse)

# load the NHANES data -- CHANGE YOUR WORKING DIRECTORY AS NEEDED!!
nhanes2013 <- read.csv("nhanes_2013_ch1.csv")

# examine the data types:
# DUQ200 and RIAGNDR should be factors
# RIDAGEYR should be numeric
class(nhanes2013$DUQ200)
class(nhanes2013$RIDAGEYR)
class(nhanes2013$RIAGENDR)

# convert to factors and numeric
nhanes2013.clean <- nhanes2013 %>%
  mutate(DUQ200 = as.factor(DUQ200)) %>%
  mutate(RIAGENDR = as.factor(RIAGENDR)) %>%
  mutate(RIDAGEYR = as.numeric(RIDAGEYR))

# check the results
summary( nhanes2013.clean )

# missing values are fine for age, gender
# recode 7 and 9 as NA for DUQ200
nhanes2013.clean <- nhanes2013.clean %>%
  mutate(DUQ200 = na_if(x = DUQ200, y = 7)) %>%
  mutate(DUQ200 = na_if(x = DUQ200, y = 9)) %>%
  droplevels()

# check the results
summary( nhanes2013.clean )

# recode DUQ200 to have human-readable labels 
nhanes2013.clean <- nhanes2013.clean %>%
  mutate(DUQ200 = recode_factor(DUQ200, `1` = "Yes", `2` = "No")) %>%
  droplevels()

# check the results
summary(nhanes2013.clean)

# make a simple bar graph of marijuana use
nhanes2013.clean %>%
  drop_na(DUQ200) %>% 
  ggplot(aes(x = DUQ200)) + 
  geom_bar() +
  labs(x = "Ever used marijuana")

# add age categories variable
nhanes2013.clean <- nhanes2013.clean %>%
  mutate(age.cat = cut(x = RIDAGEYR,
                       breaks = c(-Inf, 29, 39, 49, 59),
                       labels = c('18-29', '30-39', '40-49', '50-59'))) %>%
  droplevels()

# check the results
summary(nhanes2013.clean)

# make a side-by-side bar graph of marijuana use by age category
nhanes2013.clean %>%
  drop_na(DUQ200) %>% 
  ggplot(aes(x = age.cat, fill = DUQ200)) + 
  geom_bar(position = "dodge") +
  labs(x = "Age in years")
