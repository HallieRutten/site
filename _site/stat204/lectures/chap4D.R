# Exploring binomial and noral probability distributions

library(tidyverse)
library(tableone)

# chance of 10 heads in 20 flips of a fair coin:
dbinom( x=10, size=20, prob=0.5 )

# chance of 7 heads in 20 flips of a fair coin:
dbinom( x=7, size=20, prob=0.5 )

# chance of 10 heads in 20 flips of an unfair coin:
dbinom( x=10, size=20, prob=0.1 )

# chance of getting 9 or fewer heads in 20 flips of a fair coin:
pbinom( q=9, size=20, prob=0.5) 

coin_flips <- data.frame( successes = 0:20, probs = dbinom(0:20, size=20, prob=0.5))
ggplot( coin_flips, aes(x=successes, y=probs)) + geom_col()

coin_flips2 <- data.frame( successes = rbinom(n=1000, size=20, prob=0.5))
ggplot(coin_flips2, aes(x=successes)) + geom_bar()

unfair_flips <- data.frame( successes = 0:20, probs = dbinom(0:20, size=20, prob=0.1))
ggplot( unfair_flips, aes(x=successes, y=probs)) + geom_col()

unfair_flips2 <- data.frame( successes = rbinom(n=1000, size=20, prob=0.1))
ggplot(unfair_flips2, aes(x=successes)) + geom_bar() + xlim(0,20)

# reproduce Figure 4.6 :

pdmp_sample <- data.frame( n_states = 0:20, probs = dbinom( 0:20, size=20, prob=0.51)) 
ggplot( pdmp_sample , aes(x=n_states, y=probs)) + 
  geom_col(fill="purple") + 
  labs(x="States with monitoring programs")

opioid.policy.kff <- read_csv("pdmp_2017_kff_ch4.csv")
opioid.policy.kff <- opioid.policy.kff %>% rename( State = "...1" )
CreateTableOne(data = opioid.policy.kff %>% select(-State) )
opioid.policy.kff <- opioid.policy.kff %>%
  mutate_all(as.factor)
summary( opioid.policy.kff )

# chance of 15 or more states having PDMPs in a sample of size 25
pbinom( 14, size=25, prob=.63, lower.tail = FALSE)
# graphically:
pdmp_probs <- data.frame(successes = 0:25, probs=dbinom(0:25, size=25, prob=.63))
ggplot( pdmp_probs, aes( x = successes, y=probs )) + geom_col()
pdmp_probs <- pdmp_probs %>%
  mutate( More = if_else( successes > 14, "> 14", "<= 14"))
ggplot( pdmp_probs, aes( x=successes, y=probs, fill=More)) + geom_col()

# take one sample of size 25 and see what happens:
opioid.policy.kff %>% 
  select(`Required Use of Prescription Drug Monitoring Programs`) %>%
  sample_n( size=25 ) %>%
  summary()

dist.mat <- read.csv("opioid_dist_to_facility_2017_ch4.csv")

ggplot( dist.mat, aes(x=VALUE)) + geom_histogram()
dist.mat %>% 
  select(VALUE) %>%
  summarize( Average = mean(VALUE),
             SD = sd( VALUE ),
             Median = median(VALUE),
             IQR = IQR(VALUE) )

# percentage of counties with facility more than 50 miles away:
dist.mat <- dist.mat %>%
  mutate( Far = if_else( VALUE > 50, 1, 0)) 

dist.mat %>%
  summarize( PercentFar = 100*mean(Far))

ggplot( dist.mat, aes(x=VALUE^(1/3))) + geom_histogram()

# the standard normal distribution:
ggplot() + stat_function(fun=dnorm) + xlim(-4,4)

# chnnce of being less than or equal to 1:
pnorm( 1 ) 
# graphically:
ggplot() + 
  stat_function(fun=dnorm) + 
  stat_function(fun=dnorm, geom="area", fill="tomato", xlim=c(-4,1) ) + 
  xlim(-4,4)

pnorm(1) - pnorm(-1) 
pnorm(3) - pnorm(-3) 

# chnnce of being between -2 and 2:
pnorm( 2 ) - pnorm( -2 ) 
# graphically:
ggplot() + 
  stat_function(fun=dnorm) + 
  stat_function(fun=dnorm, geom="area", fill="green", xlim=c(-2,2) ) + 
  xlim(-4,4)

dist.mat <- dist.mat %>%
  mutate( miles.cube.root = VALUE^(1/3))

dist.mat %>% 
  select(miles.cube.root ) %>%
  summarize( Average = mean(miles.cube.root),
             SD = sd(miles.cube.root), 
             z1 = Average - SD,
             z2 = Average + SD )
