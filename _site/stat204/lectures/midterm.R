# Stat 204 Midterm Exam, Advent 2021

# `midterm.csv` contains responses from 9,364 people to questions from a previous cycle of the National Health 
# and Nutrition Examination Survey (NHANES). The dataset provides each respondent's sex, 
# educational level, and experience using firearms, as well as the age of the respondent's head of household.

# Question 1 -----------------------------------------------------------------------------
# Load the `tidyverse` library, then load the data and give it a convenient name.

# Note: make sure you set your working directory properly!

library(tidyverse)
library(tableone)
nhanes <- read_csv("midterm.csv")

# Question 2 -----------------------------------------------------------------------------
# Using the provided codebook, recode the factor variable(s) so that 
#    a. the levels are human-readable, and
#    b. any missing answers are all coded as `NA`. 
# (Refusing to answer a question or not knowing the answer is the same as not answering at all!) 
    
nhanes <- nhanes %>%
  mutate( RIAGENDR = recode_factor(.x = RIAGENDR,
                                   "1" = "Male",
                                   "2" = "Female" )) %>%
  mutate( DMDEDUC2 = recode_factor(.x = DMDEDUC2,
                                   "1" = "Less than 9th",
                                   "2" = "9th to 11th",
                                   "3" = "High school/GED",
                                   "4" = "Some college or AA",
                                   "5" = "College or above",
                                   "7" = NA_character_,
                                   "9" = NA_character_)) %>%
  mutate( AUQ300 = recode_factor(.x = AUQ300,
                                 "1" = "Yes",
                                 "2" = "No",
                                 "7" = NA_character_,
                                 "9" = NA_character_)) %>%
  droplevels()
  
# Question 3 -----------------------------------------------------------------------------
# Using one of the methods we've discussed, create a frequency table for each factor. 

CreateTableOne(data = nhanes)

descr::freq(nhanes$RIAGENDR, plot=FALSE)
descr::freq( nhanes$DMDEDUC2, plot=FALSE)
descr::freq( nhanes$AUQ300, plot=FALSE )

# Question 4 -----------------------------------------------------------------------------
# Create a bar graph summarizing respondents' firearm use. Explain what you see.

# frequency counts:
nhanes %>% 
  drop_na(AUQ300) %>% 
ggplot( aes(AUQ300)) + geom_bar()

# percentages:
nhanes %>% 
  drop_na(AUQ300) %>% 
ggplot( aes(x=AUQ300, y = ..count../sum(..count..))) + geom_bar()

# About 2/3 of the respondents have never used a firearm.

# Question 5 -----------------------------------------------------------------------------
# Create a side-by-side bar graph that summarizes firearm use for each sex. Do these 2 
# variables seem to be related? If so, how?

nhanes %>%
  drop_na( AUQ300 ) %>%
  drop_na( RIAGENDR ) %>% 
  ggplot( aes(x=RIAGENDR, fill=AUQ300)) + geom_bar( position="dodge" )

# Most (roughly 80%) females have never used a firearm; about half of the males have used a firearm before.

# Question 6 -----------------------------------------------------------------------------
# Create a side-by-side bar graph that shows the breakdown by sex of each educational level. Do these 2 
# variables seem to be related? If so, how?

nhanes %>%
  drop_na( DMDEDUC2 ) %>%
  drop_na( RIAGENDR ) %>% 
  ggplot( aes(x=DMDEDUC2, fill=RIAGENDR)) + geom_bar( position="dodge" )

# Most of the respondents have a high school degree or higher. Among those who are more highly
# educated, there are more females than males.

# Question 7 -----------------------------------------------------------------------------
# For each numeric variable:
#   a. Create a histogram.
#   b. Compute the mean, standard deviation, median, and IQR.
#   c. Which pair of summary statistics should be used to summarize the variable? Why?
    
nhanes %>%
  drop_na(DMDHRAGE) %>%
  ggplot( aes( DMDHRAGE )) + geom_histogram()

nhanes %>% 
  drop_na(DMDHRAGE) %>% 
  summarize( Average = mean(DMDHRAGE),
             SD = sd(DMDHRAGE),
             Median = median(DMDHRAGE),
             IQR = IQR(DMDHRAGE))

# Question 8 -----------------------------------------------------------------------------
# Select _one_ random sample of 100 respondents. For that one sample of size 100:
#   a. Compute the average age of the heads of household for the people in your sample. How does it compare to the average age of the heads of household for everyone in the full dataset?
#   b. Compute the standard error for the mean.
#   c. Compute and interpret a 95% confidence interval (CI) for the average age of all heads of household in the United States.
#   d. Compute the percentage of people in your sample who have ever used firearms. How does it compare to the percentage of people in in the full dataset who have ever used firearms?
#   e. Compute the standard error for the percentage.
#   f. Compute and interpret a 95% confidence interval (CI) for the percentage of all American adults who have ever used firearms.

sam <- nhanes %>% 
  drop_na() %>% 
  sample_n( size=100 )

sam %>% 
  summarize( Average = mean(DMDHRAGE),
             SD = sd(DMDHRAGE),
             SE = SD / 10 ,
             lower.ci = Average - 2*SE,
             upper.ci = Average + 2*SE)

# After computing lower.ci and upper.ci, the interpretation is: 
# We are 95% confident that the average age of all heads of household in the United States 
# is between (your value of lower.ci) and (your value of upper.ci) years.
# (You should get something like: the average age is between 43 and 49 years.)

sam$AUQ300 <- as.numeric( sam$AUQ300 )- 1

sam %>% 
  summarize( Percentage = mean(AUQ300),
             SE = sqrt( Percentage*(1-Percentage) / 100 ) ,
             lower.ci = Percentage - 2*SE,
             upper.ci = Percentage + 2*SE)

# After computing lower.ci and upper.ci, the interpretation is: 
# We are 95% confident that the percentage of all American adults who have *NEVER* used firearms
# is between (your value of lower.ci) and (your value of upper.ci). 

# Note: These results describe those who have *NEVER* used firearms because "No" was coded as 2 and
# "Yes" was coded as 1 in the NHANES dataset (check the codebook!). If you want to estimate the 
# percentage of all American adults who *HAVE* used firearms, just subtract these results
# from 100%. (You should get something like: 20% to 40% of people have used firearms.)
