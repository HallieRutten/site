
# Chapter 2 mini-project 
########################

library(tidyverse)
library(tableone)

# load the BRFSS data -- CHANGE YOUR WORKING DIRECTORY AS NEEDED!!
trans2014 <- read.csv("transgender_hc_ch2.csv")

# just analyze the transgender respondents (but include all of them!)
# then select variables of interest
trans2014.small <- trans2014 %>%
  filter(TRNSGNDR < 4) %>%
  select(TRNSGNDR, PHYSHLTH, X_INCOMG, X_EDUCAG, X_AGEG5YR, X_AGE80, X_RACE)

# check results
summary(trans2014.small)

# check data types
class(trans2014.small$TRNSGNDR)
class(trans2014.small$PHYSHLTH)
class(trans2014.small$X_INCOMG)
class(trans2014.small$X_EDUCAG)
class(trans2014.small$X_AGEG5YR)
class(trans2014.small$X_AGE80)
class(trans2014.small$X_RACE)

# as appropriate, change variables to factors and recode to make them human-readable:

trans2014.small <- trans2014.small %>%
  mutate(TRNSGNDR = recode_factor(TRNSGNDR, `1` = "Transgender male-to-female",
                                  `2` = "Transgender female-to-male",
                                  `3` = "Transgender gender nonconforming")) %>%
  mutate(PHYSHLTH = as.numeric(PHYSHLTH)) %>% 
  mutate(PHYSHLTH = na_if(PHYSHLTH, 77)) %>%
  mutate(PHYSHLTH = na_if(PHYSHLTH, 99)) %>%
  mutate(PHYSHLTH = recode(PHYSHLTH, `88` = 0)) %>%
  mutate(X_INCOMG = recode_factor(X_INCOMG, `1` = "Less than $15,000",
                                  `2` = "$15,000 to less than $25,000",
                                  `3` = "$25,000 to less than $35,000",
                                  `4` = "$35,000 to less than $50,000",
                                  `5` = "$50,000 or more",
                                  `9` = NA_character_,)) %>% 
  mutate(X_EDUCAG = recode_factor(X_EDUCAG, `1` = "Did not graduate High School",
                                  `2` = "Graduated High School",
                                  `3` = "Attended College or Technical School",
                                  `4` = "Graduated from College or Technical School",
                                  `9` = NA_character_,)) %>% 
  mutate(X_AGEG5YR = recode_factor(X_AGEG5YR, `1` = "Age 18 to 24",
                                   `2` = "Age 25 to 29",
                                   `3` = "Age 30 to 34",
                                   `4` = "Age 35 to 39",
                                   `5` = "Age 40 to 44",
                                   `6` = "Age 45 to 49",
                                   `7` = "Age 50 to 54",
                                   `8` = "Age 55 to 59",
                                   `9` = "Age 60 to 64",
                                   `10` = "Age 65 to 69",
                                   `11` = "Age 70 to 74",
                                   `12` = "Age 75 to 79",
                                   `13` = "Age 80 or older",
                                   `14` = NA_character_)) %>% 
  mutate(X_RACE = recode_factor(X_RACE, `1` = "White only, non-Hispanic",
                                `2` = "Black only, non-Hispanic",
                                `3` = "American Indian or Alaskan Native only, Non-Hispanic",
                                `4` = "Asian only, non-Hispanic",
                                `5` = "Native Hawaiian or other Pacific Islander only, Non-Hispanic",
                                `6` = "Other race only, non-Hispanic",
                                `7` = "Multiracial, non-Hispanic",
                                `8` = "Hispanic",
                                `9` = NA_character_))

# check results
summary(trans2014.small)

# check the distributions of the continuous variables
# age 
trans2014.small %>% 
  ggplot(aes(x = X_AGE80)) +
  geom_histogram()

# poor health
trans2014.small %>% 
  ggplot(aes(x = PHYSHLTH)) +
  geom_histogram()

# NOTE: not surprisingly, neither one is normally distributed!

# descriptive statistics of all the variables
desc.table <- CreateTableOne(data = trans2014.small)
print(desc.table, nonnormal = c('PHYSHLTH', 'X_AGE80'))

# descriptive statistics of all the variables
desc.table <- CreateTableOne(data = trans2014.small)
print(desc.table, nonnormal = c('PHYSHLTH', 'X_AGE80'))

# summary stats for PHYSHLTH
trans2014.small %>%
  drop_na( PHYSHLTH ) %>%
  summarize( Average = mean(PHYSHLTH),
             SD = sd(PHYSHLTH),
             Median = median(PHYSHLTH),
             IQR = IQR(PHYSHLTH))

