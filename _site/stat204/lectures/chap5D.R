# Chi-square tests: observed vs expected counts, p-values, etc.

library(tidyverse)
library(haven)
library(descr)
library(gridExtra)

# load the voting data (use read_sav() to handle SPSS format)
vote <- read_sav(file = "pew_apr_19-23_2017_weekly_ch5.sav")

# select variables of interest and clean them
vote.cleaned <- vote %>%
  select(pew1a, pew1b, race, sex, mstatus, ownhome, employ, polparty) %>%
  zap_labels() %>%
  mutate(pew1a = recode_factor(.x = pew1a,
                               `1` = 'Register to vote',
                               `2` = 'Make easy to vote',
                               `5` = NA_character_,
                               `9` = NA_character_)) %>%
  rename(ease.vote = pew1a) %>%
  mutate(pew1b = recode_factor(.x = pew1b,
                               `1` = 'Require to vote',
                               `2` = 'Choose to vote',
                               `5` = NA_character_,
                               `9` = NA_character_)) %>%
  rename(require.vote = pew1b) %>%
  mutate(race = recode_factor(.x = race,
                              `1` = 'White non-Hispanic',
                              `2` = 'Black non-Hispanic',
                              `3` = 'Hispanic',
                              `4` = 'Hispanic',
                              `5` = 'Hispanic',
                              `6` = 'Other',
                              `7` = 'Other',
                              `8` = 'Other',
                              `9` = 'Other',
                              `10` = 'Other',
                              `99` = NA_character_)) %>%
  mutate(sex = recode_factor(.x = sex,
                             `1` = 'Male',
                             `2` = 'Female')) %>%
  mutate(ownhome = recode_factor(.x = ownhome,
                                 `1` = 'Owned',
                                 `2` = 'Rented',
                                 `8` = NA_character_,
                                 `9` = NA_character_)) 

# check recoding
summary(object = vote.cleaned)

# voting ease by race-eth no spread
vote.cleaned %>%
  drop_na(ease.vote) %>%
  drop_na(race) %>%
  group_by(ease.vote, race) %>%
  summarize(freq.n = n())

# voting ease by race-eth with spread
vote.cleaned %>%
  drop_na(ease.vote) %>%
  drop_na(race) %>%
  group_by(ease.vote, race) %>%
  summarize(freq.n = n()) %>%
  spread(key = race, value = freq.n)

# voting ease by race-eth with table
table(vote.cleaned$ease.vote, vote.cleaned$race)

# table of percents voting ease by race-eth
prop.table(x = table(Voting.ease = vote.cleaned$ease.vote,
                     Race.eth = vote.cleaned$race))

# table of percents voting ease by race-eth
prop.table(x = table(Voting.ease = vote.cleaned$ease.vote,
                     Race.eth = vote.cleaned$race),
           margin = 2)

# table of percents voting required by race-eth
prop.table(x = table(Voting.requirement = vote.cleaned$require.vote,
                     Race.eth = vote.cleaned$race),
           margin = 2)

# graph the relationship between registration ease and race eth
ease.graph <- vote.cleaned %>%
  drop_na(ease.vote) %>%
  drop_na(race) %>%
  group_by(ease.vote, race) %>%
  count() %>%
  group_by(race) %>%
  mutate(perc = 100*n/sum(n)) %>%
  ggplot(aes(x = race, y = perc, fill = ease.vote)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("gray", "#7463AC"), name = "Opinion on\nvoter registration") +
  labs(x = "", y = "Percent within group") +
  theme(axis.text.x = element_blank())

# graph the relationship between required voting and race eth
req.graph <- vote.cleaned %>%
  drop_na(require.vote) %>%
  drop_na(race) %>%
  group_by(require.vote, race) %>%
  count() %>%
  group_by(race) %>%
  mutate(perc = 100*n/sum(n)) %>%
  ggplot(aes(x = race, y = perc, fill = require.vote)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("gray", "#7463AC"), name = "Opinion on voting") +
  labs(x = "Race-ethnicity group", y = "Percent within group")

grid.arrange(ease.graph, req.graph, nrow = 2)

# Cross-tabulation with expected counts and chi-square results:
CrossTable( x=vote.cleaned$ease.vote, y=vote.cleaned$race, expected = TRUE, chisq = TRUE)

# Chi-square distributions
###########################

# Hypothesis testing
#####################

# Goodness of fit test
#######################

# frequencies of race-ethnicity from pew data
race.eth <- vote.cleaned %>%
  drop_na(race) %>%
  count(race)
race.eth

# chi-squared comparing observed race-eth to population race-eth
race.gof <- chisq.test(x = race.eth$n, p = c(.691, .121, .125, .063))
race.gof

# add percents of race-ethnicity from Pew data to make a data frame
# for graphing
race.eth.graph.data <- vote.cleaned %>%
  drop_na(race) %>%
  group_by(race) %>%
  summarize(n = n()) %>%
  mutate(sample = n/sum(n),
         population = c(.691, .121, .125, .063)) %>%
  gather(key = samp.or.pop, value = perc, sample, population)
race.eth.graph.data

# Make the graph (Figure 5.11)
race.eth.graph.data %>%
  ggplot(aes(x = race, y = 100*perc, fill = samp.or.pop)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("gray", "#7463AC"),
                    name = "Population\nor sample") +
  labs(y = "Percent of people", x = "Race-ethnicity group")

# standardized residuals from race.gof object
race.gof$stdres
