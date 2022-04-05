# Riley Barrett
# 22 Feb 2022

# Setup and get data -----
library(tidyverse)
setwd("/home/rileybarrettfg/214")

violets <- read.csv("datasets/violets.csv")

# Select and mutate dataset before analyzing -----
violets <- violets %>%
  filter(Bud.type == "Chasmogamous") %>%
  mutate(Bud.present = as.integer(Bud.counts > 0))

# Plot the fitted logistic curve with a method that works -----
violets.plot <-
  ggplot(violets, aes(x = Photoperiod, y = Bud.present)) +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    level = 0
  ) +
  geom_point(alpha = 0.08)

# violets.plot

# Plot the fitted logistic curve with a method that I would like to work -----
violets.fit <- glm(data = violets, family = "binomial",
                   Bud.present ~ Photoperiod)

#ggplot(violets, aes(x = Photoperiod, y = Bud.present)) +
#  stat_function(fun = violets.fit) +
#  geom_point(alpha = 0.08)

# Make some bins to check against fitted curve -----
group.size = 0.5

violets <- violets %>%
  mutate(Photoperiod.group = round(Photoperiod / group.size) * group.size)

violets.groups <- violets %>%
  group_by(Photoperiod.group) %>%
  summarise(Bud.mean = mean(Bud.present))

# Add the bin averages to the plot to check the curve -----
violets.plot +
  geom_point(
    data = violets.groups,
    aes(x = Photoperiod.group, y = Bud.mean),
    color = "tomato",
    size = 3
  )

# Likelihood ratio test -----
g <- violets.fit$null.deviance - violets.fit$deviance
p_value <- pchisq(g, 1, lower.tail = FALSE)
