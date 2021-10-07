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


dist.mat %>% 
  summarize( Average = mean(distance),
             SD = sd(distance) )

counties.500 <- dist.mat %>%
  sample_n( size=500, replace=FALSE)

counties.500 %>% 
  summarize( Average = mean(distance),
             SD = sd(distance), 
             SE = SD/sqrt(length(distance)),
             lower.ci = Average - 1.96*SE,   ## NEW CODE!!
             upper.ci = Average + 1.96*SE)   ## NEW CODE!!

samples.20 <- bind_rows(replicate(n = 20, 
                                  dist.mat %>% sample_n(size = 500, replace = FALSE), 
                                  simplify = FALSE), 
                        .id = "sample_num")

# find the mean for each sample

sample.20.means <- samples.20 %>%
  group_by(sample_num) %>%
  summarize(mean.distance = mean(x = distance))

# find the mean of the sample means

sample.20.means %>%
  summarize(mean.20.means = mean(x = mean.distance),
            sd.20.means = sd( mean.distance))

# histogram of the 20 means (Figure 4.17)

sample.20.means %>%
  ggplot(aes(x = mean.distance)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  labs(x = "Mean distance to facility with MAT",
       y = "Number of samples") +
  theme_minimal()

# We need many more samples to get a good estimate of the 
# sampling distribution. Here we go...

samples.1000 <- bind_rows(replicate(n = 1000, 
                                    dist.mat %>% sample_n(size = 500, replace = FALSE), 
                                    simplify = FALSE), 
                          .id = "sample_num")

# find the mean for each sample

sample.1000.means <- samples.1000 %>%
  group_by(sample_num) %>%
  summarize(mean.distance = mean(x = distance))

# find the mean of the sample means

sample.1000.means %>%
  summarize(mean.1000.means = mean(x = mean.distance),
            sd.1000.means = sd( mean.distance))

# histogram of the 1000 means (Figure 4.17)

sample.1000.means %>%
  ggplot(aes(x = mean.distance)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  labs(x = "Mean distance to facility with MAT",
       y = "Number of samples") +
  theme_minimal()

## THE FOLLOWING CODE IS NEW SINCE TUESDAY!!

samp.20.stats <- samples.20 %>%
  group_by(sample_num) %>%
  summarize(means = mean(x = distance),
            sd = sd(x = distance),
            se = sd(x = distance)/sqrt(x = length(x = distance)),
            lower.ci = means - 1.96 * se,
            upper.ci = means + 1.96 * se)

samp.20.stats

samp.20.stats %>%
  ggplot(aes(y = means, x = sample_num)) +
  geom_errorbar(aes(ymin = lower.ci,
                    ymax = upper.ci,
                    linetype = "95% CI of\nsample mean"), color = "#7463AC") +
  geom_point(stat = "identity", aes(color = "Sample mean")) +
  geom_hline(aes(yintercept = 24.04, alpha = "Population mean"),
             color = "deeppink") +
  labs(y = "Mean distance to treatment facility (95% CI)",
       x = "Sample") +
  scale_color_manual(values = "#7463AC", name = "") +
  scale_linetype_manual(values = c(1, 1), name = "") +
  scale_alpha_manual(values = 1, name = "") +
  theme_minimal()

#### BACK TO PROPORTIONS...

# open state opioid program data

state.opioid.pgm.2017 <- read.csv(file = "pdmp_2017_kff_ch4.csv")

# recode Yes to 1 and No to 0, change long name to pdmp 
# N.B. need to convert pdmp to factor!

state.opioid.pgm.2017.cleaned <- state.opioid.pgm.2017 %>%
  rename(pdmp = Required.Use.of.Prescription.Drug.Monitoring.Programs) %>%
  mutate( pdmp = as.factor( pdmp ) ) %>%
  mutate(pdmp = as.numeric(x = pdmp) - 1)

# find the mean of pdmp

state.opioid.pgm.2017.cleaned %>%
  summarize(p = mean(x = pdmp))


samples.30.states <- bind_rows(replicate(n = 100,
                                         state.opioid.pgm.2017.cleaned %>% sample_n(size = 30, replace = TRUE),
                                         simplify = FALSE), 
                               .id = "sample_num")

# find the mean for each sample

sample.30.means.states <- samples.30.states %>%
  group_by(sample_num) %>%
  summarize(p.pdmp = mean(x = pdmp),
            p.se = sqrt( p.pdmp*(1-p.pdmp)/ 30),
            lower.ci = p.pdmp - 1.96*p.se,
            upper.ci = p.pdmp + 1.96*p.se)

sample.30.means.states

sample.30.means.states %>%
  ggplot(aes(x = 100*p.pdmp)) +
  geom_histogram(fill = "#7463AC", color = "white", binwidth=4) +
  labs(x = "Percent of states with PDMP",
       y = "Number of samples") +
  theme_minimal()

sample.30.means.states %>%
  ggplot(aes(y = p.pdmp, x = sample_num)) +
  geom_errorbar(aes(ymin = lower.ci,
                    ymax = upper.ci,
                    linetype = "95% CI of\nsample proportion"), color = "#7463AC") +
  geom_point(stat = "identity", aes(color = "Sample proportion")) +
  geom_hline(aes(yintercept = 0.627451, alpha = "Population proportion"),
             color = "deeppink") +
  labs(y = "Percent of states with PDMP (95% CI)",
       x = "Sample") +
  scale_color_manual(values = "#7463AC", name = "") +
  scale_linetype_manual(values = c(1, 1), name = "") +
  scale_alpha_manual(values = 1, name = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) # + coord_flip()
