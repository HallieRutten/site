library(tidyverse)

# One-sample t-test ---------------------------

arsenic <- read_csv("ToenailArsenic.csv")
t.test( arsenic$Arsenic, mu=0.20)

# Two-sample t-test ---------------------------

calcium <- read_csv("ColaCalcium.csv")
t.test( calcium$Calcium ~ calcium$Drink )

# check SDs to see if var.equal should be TRUE or FALSE
calcium %>%
  group_by(Drink) %>%
  summarize( SD = sd(Calcium) )

# they're sufficiently different to stick with the default, var.equal=FALSE, in t.test

# One-way ANOVA -------------------------------

ants <- read_csv("SandwichAnts.csv")

# check SDs:
ants %>% 
  group_by( Filling ) %>%
  summarize( SD = sd(Ants) )

# just F and p:
oneway.test( Ants ~ Filling, data = ants, var.equal = TRUE)

# the full ANOVA table:
filling.aov <- aov( Ants ~ Filling, data = ants )
summary( filling.aov )

# check SDs:
ants %>% 
  group_by( Bread ) %>%
  summarize( SD = sd(Ants) )

# just F and p:
oneway.test( Ants ~ Bread, data = ants, var.equal = TRUE)

# the full ANOVA table:
bread.aov <- aov( Ants ~ Bread, data = ants )
summary( bread.aov )
