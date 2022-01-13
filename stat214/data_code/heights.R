library(tidyverse)

# load the data -----
heights <- read_csv("heights.csv")

# get the column names:
names( heights )
# look at the Father variable:
heights$Father

# histogram of Father heights -----
ggplot( heights, aes(Father)) + geom_histogram()
# average, SD of Father heights
mean( heights$Father )
sd( heights$Father )

# histogram of Son heights -----
ggplot( heights, aes(Son)) + geom_histogram()
# average, SD of Son heights
mean( heights$Son )
sd( heights$Son )

# better histograms of height data -----
ggplot( data = gather(heights), aes( value) ) + 
  geom_histogram() +
  facet_wrap( ~ key )

# add a grouping variable for later use -----
heights <- heights %>% 
  mutate( Group = round(Father)) 

# get summary statistics by Group -----
heights %>% 
  group_by( Group ) %>% 
  summarize( N = n(),
             Mean = mean(Son ),
             SD = sd(Son))
