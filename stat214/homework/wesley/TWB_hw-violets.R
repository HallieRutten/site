library(tidyverse)
violets <- read.csv("data/violets.csv")
violets
violets <- violets %>%
  filter(Bud.type == "Chasmogamous") %>%
  select(Photoperiod, Bud.counts)
violets$Bud.counts[violets$Bud.counts>0] <- 1
View(violets)

ggplot(violets, aes(x=Photoperiod, y=Bud.counts)) +
  geom_point()

violets.fit <- glm(Bud.counts ~ Photoperiod, data=violets,
                   family="binomial")
violets.fit

cohorts <- violets %>%
  group_by(Photoperiod) %>%
  summarize(N=n(), Percentage = sum(Bud.counts)/N)
View(cohorts)
ggplot(cohorts, aes(Photoperiod, Percentage)) +
  geom_point() +
  ylim(0,1) 
