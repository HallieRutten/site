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

# means, SDs via gathering and grouping -----
gather( heights) %>% 
  group_by( key ) %>% 
  summarize( Average = mean(value),
             SD = sd(value))

# add a grouping variable for later use -----
heights <- heights %>% 
  mutate( Group = round(Father)) 

# get summary statistics by Group -----
( son.means <- heights %>% 
  group_by( Group ) %>% 
  summarize( N = n(),
             Mean = mean(Son),
             SD = sd(Son)))

ggplot() +
  geom_point(data = heights %>% filter(Group != 64),
             aes(Father, Son),
             alpha = 0.2) +
  geom_point(data = heights %>% filter(Group == 64), aes(Father, Son))

ggplot() +
  geom_point(data = heights %>% filter(Group != 70),
             aes(Father, Son),
             alpha = 0.2) +
  geom_point(data = heights %>% filter(Group == 70), aes(Father, Son))

heights.plot <- ggplot( heights, aes( Father,Son )) + 
  geom_point()

heights.plot +
  geom_point(data = son.means,
             aes(x = Group, y = Mean),
             color = "red",
             size = 2)

heights.plot +
  geom_smooth( method="lm", level=0) +
  geom_point(data = son.means,
             aes(x = Group, y = Mean),
             color = "red",
             size = 2) 

# compute successive differences to estimate slope:
diff( son.means$Mean )
# estimate slope via average of most of the differences 
mean( diff( son.means$Mean)[ 4:13 ] )
