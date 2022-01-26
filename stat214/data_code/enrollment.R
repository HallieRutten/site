library(tidyverse) 

enrollment <- read_csv("enrollment.csv")

ggplot( enrollment, aes(Births, Enrollment)) +
  geom_point()
