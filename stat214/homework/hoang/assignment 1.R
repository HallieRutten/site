# Hoang Le - STAT 214
# Load haven
library(haven)
library(tidyverse)
body_mass <- read_xpt("BMX.XPT")

# select only two vars, drop na entries
body_mass <- body_mass %>%
  select(BMXWAIST, BMXBMI) %>%
  drop_na() %>%
  rename(Waist = BMXWAIST, BMI = BMXBMI)
View(body_mass)

# scatter plot
bmi.plot <- ggplot(body_mass, aes(Waist, BMI)) +
  geom_point(size = 1) +
  labs(x = "Waist circumference (cm)",
       y = "BMI")


#null model
ybar = mean(body_mass$BMI)

# linear model
bmi.plot + geom_smooth(method = "lm", level = 0) 
body.fit <- lm(Waist ~ BMI, data = body_mass)
summary(body.fit)

# computing avg for groups
body_mass <- body_mass %>% mutate(Group = round(Waist, -1))
bmi.means <- body_mass %>%
  group_by(Group) %>%
  summarise(N = n(), 
            Mean = mean(BMI), 
            SD = sd(BMI))

# Average by group and linear model fit 
bmi.plot + 
  geom_smooth(method = "lm", level = 0) + 
  geom_point(data = bmi.means, 
                      aes(x = Group, y = Mean),
                      color = "red", 
                      size = 2)

