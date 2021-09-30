# Exploring probability distributions: binomial, normal

library(tidyverse)
library(tableone)

# chance of 5 heads in 20 flips of a fair coin:
dbinom( x=5, size=20, prob=0.5 )
# chance of 10 heads in 20 flips of a fair coin:
dbinom( x=10, size=20, prob=0.5 )

# chance of 10 or fewer heads in 20 flips of a fair coin:
pbinom( q=10, size=20, prob=0.5)
# chance of 9 or fewer heads in 20 flips of a fair coin:
pbinom( q=9, size=20, prob=0.5)

# plot a binomial distribution with size=20, prob=0.5
coin_flips <- data.frame( successes = 0:20, probs = dbinom(0:20,size=20,prob=0.5))
ggplot( coin_flips, aes(x=successes, y=probs)) + geom_col(fill="purple")

# plot a binomial distribution with size=20, prob=0.5
die_rolls <- data.frame( successes = 0:20, probs = dbinom(0:20,size=20,prob=1/3))
ggplot( die_rolls, aes(x=successes, y=probs)) + geom_col()

coin_flips2 <- data.frame( successes =  rbinom( n=1000, size=20, prob=0.5 ) )
ggplot( coin_flips2, aes(x=successes)) + geom_bar()

# reproduce Figure 4.6:
pdmp_sample <- data.frame( n_states = 0:20, probs = dbinom(0:20,size=20,prob=0.51) )
ggplot( pdmp_sample, aes(x=n_states, y=probs))+geom_col( fill="purple")

opioid.policy.kff <- read_csv("pdmp_2017_kff_ch4.csv")
opioid.policy.kff <- opioid.policy.kff %>% rename( State = "...1")

CreateTableOne( data = opioid.policy.kff )
