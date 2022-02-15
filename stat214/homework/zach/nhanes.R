library(haven)
library(tidyverse)

food <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DRXIFF.XPT")

new_food <- food %>%
  select(DRXIPROT,DRXIKCAL) %>%
  na.omit()

ggplot(new_food,aes(DRXIPROT,DRXIKCAL)) +
  geom_point() +
  geom_smooth(method = "lm", level = 0)

food.fit <- lm(DRXIKCAL ~ DRXIPROT, data = new_food)
food.fit

x.bar <- mean(new_food$DRXIPROT)
y.bar <- mean(new_food$DRXIKCAL)
sd.x <- sd(new_food$DRXIPROT )
sd.y <- sd( new_food$DRXIKCAL )
b <- (sd.y / sd.x) #finding slope
a <- y.bar - b*x.bar #finding intercept
ggplot( new_food, aes(x=DRXIPROT, y=DRXIKCAL)) +
  geom_point() +
  geom_abline( intercept = a, slope = b, color = "blue")
