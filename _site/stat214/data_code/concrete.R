library(tidyverse)
concrete <- read_csv("concrete.csv")

concrete <- concrete %>%
  filter( Age == 28 ) %>% 
  select( Cement, Water, Strength) 

concrete <- concrete %>%
  mutate( Ratio = Water / Cement )

ggplot( concrete, aes(x=Ratio, y=Strength)) +
  geom_point()

ggplot( concrete, aes(x=Ratio, y=Strength)) +
  geom_point() +
  geom_smooth()

# there is not a linear relationship!!
# you should not do regression!!
# (but I will anyway...)
concrete.fit <- lm( Strength ~ Ratio, data=concrete)
cor( concrete$Ratio, concrete$Strength)
summary( concrete.fit) 

# try transforming the predictor to see if there's
# a linear relationship:
concrete <- concrete %>%
  mutate( Ratio = Cement / Water )

ggplot( concrete, aes(x=Ratio, y=Strength)) +
  geom_point()

ggplot( concrete, aes(x=Ratio, y=Strength)) +
  geom_point() +
  geom_smooth( method="lm", level=0)

# now there is a linear relationship!!
# safe to proceed with regression!!
cor( concrete$Ratio, concrete$Strength)
concrete.fit <- lm( Strength ~ Ratio, data=concrete)
summary( concrete.fit) 

new.data <- data.frame( Ratio = c(1.5, 2.5)) 
predict( concrete.fit, new.data )
predict( concrete.fit, new.data, interval="confidence")
