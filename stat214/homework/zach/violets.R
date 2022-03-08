library(readr)
library(dplyr)
library(tidyverse)
library(flextable)

#data
violets <- read_csv("violets.csv")

#create new dataframe of just variables we want
chasma <- violets %>%
  filter (`Bud type` == 'Chasmogamous') %>%
  select (Photoperiod,`Bud type`, `Bud counts`) %>%
  mutate (Budded = as.integer(`Bud counts` != 0))

#simple plot of data
ggplot(chasma, aes(Photoperiod, Budded)) + geom_point()

regulartable(chasma)

#logistic curve
#z <- seq(-5, 5, len=1000)
#logistic <- data.frame( z=z, sigma = 1 / (1 + exp(-z)))
#ggplot( logistic, aes(z,sigma)) + 
#  geom_line( )


#fitted curve with data
ggplot(chasma, aes(Photoperiod, Budded)) + 
  geom_smooth(method=glm,method.args=list(family=binomial),size=1) +
  geom_point()

#fitted logistic regression model
chasma.fit <- glm(Budded ~ Photoperiod, data = chasma, family = "binomial")
summary(chasma.fit)

coeff <- -chasma.fit$coefficients[1] / chasma.fit$coefficients[2]
coeff

#g squared value --- -2log(null likelihood/model likelihood)
g <- chasma.fit$null.deviance - chasma.fit$deviance

#p-value
pchisq(g,1,lower.tail = FALSE)
