#Homework 1: Logistic Regression

# Step 1: Import data

library(tidyverse)
setwd("/home/ashleystewart/Easter/Stats/Data")
violets <- read.csv("datasets/violets.csv")

# Step 2: Create new dataframe 
  # Budcounts --> binary response varaible
  # Limit Bud type to chasmogamous 
  # What is the association between bud counts and photo period?

violets <- violets %>%
  select(`Bud type`, `Bud counts`, Photoperiod)%>%
  filter(Bud.type == "Chasmogamous") %>%
  mutate(Bud.new = as.integer(Bud.counts > 0))

# Step 3: Plot the data 
  # Basic plot

ggplot(violets, aes (Photoperiod, Bud.new)) +
  geom_point()

  # Fitted logistic curve & Fitted logistic model
  # Note: The fitted logistic model should fit nicely to the data!

violets.fit <- glm(Bud.new ~ Photoperiod, data = violets, family = "binomial")
violets.fit

ggplot(violets, aes(Photoperiod, Bud.new)) + 
  geom_smooth(method="glm",
              method.args=list(family=binomial),
              size=1) +
  geom_point()

# Step 4: Analyze 
  # Identify threshold 

threshold <- violets.fit$coefficients[1]/violets.fit$coefficients[2]
threshold

  # Likelihood ratio
g <- violets.fit$null.deviance - violets.fit$deviance

  # P-value
pvalue <- pchisq(g, 1, lower.tail = FALSE)
pvalue








