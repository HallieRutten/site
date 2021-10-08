
# Chapter 4 mini-project 
########################

library(tidyverse)
library(tableone)

# load the PDMP data -- CHANGE YOUR WORKING DIRECTORY AS NEEDED!!

opioid.policy.kff <- read.csv(file = "pdmp_2017_kff_ch4.csv")

# frequency table for states with step therapy guidelines
descr::freq(opioid.policy.kff$Opioid.Step.Therapy.Requirements)

# 5 successes from a sample of 10 with 76.5% success rate
dbinom(x = 5, size = 10, prob = .765)

# 5 successes from a sample of 15 with 76.5% success rate
dbinom(x = 5, size = 15, prob = .765)

# recode Yes to 1 and No to 0, change long name to step
opioid.policy.kff.cleaned <- opioid.policy.kff %>%
  rename(step = Opioid.Step.Therapy.Requirements) %>%
  mutate( step = as.factor( step ) ) %>%
  mutate(step = as.numeric(x = step) - 1)

# check results -- compare to previous freq table!
descr::freq(opioid.policy.kff.cleaned$step)

# sample 30 states
opioid.policy.kff.cleaned %>% 
  sample_n(size = 30) %>% 
  summarize(step.mean = mean(x = step),
            se = sqrt((step.mean*(1 - step.mean))/length(x = step)),
            lower.ci = step.mean - 2*se,
            upper.ci = step.mean + 2*se)

# INTERPRETATION:
# Whatever you get for step.mean is your best estimate of the percentage of states with step therapy guidelines; 
# we're 95% confident that the percentage is between lower.ci and upper.ci (whatever you got for those).

# load the opioid policy data -- CHANGE YOUR WORKING DIRECTORY AS NEEDED!!
opioid.dist.exch <- read.csv(file = "opioid_dist_to_needle_exchange_2018.csv")

# mean and sd of VALUE
opioid.dist.exch %>%
  summarize(m.dist = mean(x = VALUE),
            sd.dist = sd(x = VALUE))

# z-score for St. Louis County, MO ; actual distance is 12.27 miles
(12.27 - mean(x = opioid.dist.exch$VALUE))/sd(x = opioid.dist.exch$VALUE)

# INTERPRETATION:
# The distance to an exchange in St. Louis County, MO is 1.07 standard deviations below the mean for counties in this data set.

# sample 500 counties and compute 95% CI

opioid.dist.exch %>% 
  sample_n(size = 500) %>% 
  summarize(mean.dist = mean(x = VALUE),
            sd.dist = sd(x = VALUE),
            se = sd(x = VALUE)/sqrt(x = length(x = VALUE)),
            lower.ci = mean.dist - 2*se,
            upper.ci = mean.dist + 2*se)

# INTERPRETATION:
# Whatever you get for mean.dist is your best estimate of the national average distance to an exchange with step therapy guidelines; 
# we're 95% confident that the national average is between lower.ci and upper.ci (whatever you got for those).


# 100 samples, 500 counties per sample
samples.500.counties <- bind_rows(replicate(n = 100,
                                            opioid.dist.exch %>%
                                              sample_n(size = 500, replace = FALSE),
                                         simplify = FALSE), .id = "sample_num")

# summary stats and CIs for each sample
samples.500.stats <- samples.500.counties %>% 
  group_by( sample_num ) %>%
  summarize(mean.dist = mean(x = VALUE),
          sd.dist = sd(x = VALUE),
          se = sd(x = VALUE)/sqrt(x = length(x = VALUE)),
          lower.ci = mean.dist - 1.96*se,
          upper.ci = mean.dist + 1.96*se)

samples.500.stats

# histogram of sample means 
samples.500.stats %>% 
  ggplot( aes( mean.dist ) ) +
  geom_histogram()

# If you want a better picture of the sampling distribution, modify the above to take 1000 samples instead of 100,
# then plot the results.
