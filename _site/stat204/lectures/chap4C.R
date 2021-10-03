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

CreateTableOne( data = opioid.policy.kff %>% select(-State) )

# chance of getting 15 or more states with PDMPs in a sample of size 25:
pbinom( 14, size=25, prob=.63, lower.tail=FALSE)

pdmp_probs <- data.frame( successes = 0:25, probs=dbinom(0:25, size=25, prob=.63))
ggplot( pdmp_probs, aes(x=successes, y=probs)) + geom_col()
pdmp_probs <- pdmp_probs %>%
  mutate( More = if_else( successes >= 15, ">= 15", "< 15"))
ggplot( pdmp_probs, aes(x=successes, y=probs, fill=More)) + geom_col()

opioid.policy.kff <- opioid.policy.kff %>%
  mutate_all( as.factor )

opioid.policy.kff %>% 
  select(`Required Use of Prescription Drug Monitoring Programs`) %>%
  sample_n( size=25 ) %>%
  summary()

dist.mat <- read.csv("opioid_dist_to_facility_2017_ch4.csv")
ggplot( dist.mat, aes(x=VALUE)) + geom_histogram()
dist.mat %>% 
  select(VALUE) %>% 
  summarize( Average = mean( VALUE ),
             SD = sd(VALUE),
             Median = median(VALUE ),
             IQR = IQR(VALUE) )

dist.mat <- dist.mat %>% rename( distance = VALUE )
dist.mat <- dist.mat %>%
  mutate( Far = if_else( distance > 50, 1, 0))

dist.mat %>% summarize( PercentFar = mean(Far))

# the standard normal distribution:
ggplot() + stat_function( fun=dnorm ) + xlim(-4,4)

# percentage of values between -1 and 1 :
pnorm(1) - pnorm(-1)
# graphically:
ggplot() + 
  stat_function( fun=dnorm ) + 
  stat_function( fun=dnorm, geom="area", fill="tomato", xlim=c(-1,1)) + 
  xlim(-4,4)

# percentage within 2 SDs of average:
pnorm(2) - pnorm(-2)

# percentage within 3 SDs of average:
pnorm(3) - pnorm(-3)

ggplot( dist.mat, aes(x=distance^(1/3))) + geom_histogram()
dist.mat <- dist.mat %>%
  mutate( miles.cube.root = distance^(1/3))
dist.mat %>%
  select( miles.cube.root ) %>% 
  summarize( Average = mean( miles.cube.root),
             SD = sd( miles.cube.root),
             z1 = Average - SD,
             z2 = Average + SD )
