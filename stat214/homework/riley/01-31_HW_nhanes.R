# Riley Barrett
# 31 Jan 2022

# Setup and get data -----
library(tidyverse)
library(haven)
setwd("/home/rileybarrettfg/214")

blood <- read_xpt("datasets/nhanes_2017-20_blood.XPT")

# Examine and filter the dataset -----
# Deciding what to analyze correlations of
cor.matrix <- cor(blood, use = "complete.obs")

# View a quick plot of two correlated variables
ggplot(blood, aes(LBXWBCSI, LBDLYMNO)) +
  geom_point() +
  geom_smooth()

# Filter six outliers, select used columns, and drop NA values
blood <- blood %>%
  filter(LBXWBCSI < 24) %>%
  select(LBXWBCSI, LBDLYMNO) %>%
  drop_na()

# Plot points
ggplot(blood, aes(LBXWBCSI, LBDLYMNO)) +
  geom_point() +
  geom_smooth()

# Establish the null model
ybar = mean(blood$LBDLYMNO)

# Finally rename the columns
blood <- blood %>%
  rename(WBC = LBXWBCSI) %>%
  rename(LCN = LBDLYMNO)

# Grouping and linear regression model -----
# Construct groups by rounding
blood <-
  blood %>%
  mutate(Group = round(WBC))

# Catalog the mean value of each group
LCN.means <- blood %>%
  group_by(Group) %>%
  summarize(N = n(),
            Mean = mean(LCN),
            SD = sd(LCN))

# Filter out small count groups
LCN.means <- LCN.means %>%
  filter(Group < 16)

# Assign a base plot for future use
blood.plot <- ggplot(blood, aes(WBC, LCN)) +
  xlim(0, 16) +
  ylim(0, 8) +
  geom_point(size = 0.6, alpha = 0.06)

# Plot the group averages, and fit a line to them
blood.plot.groups <- blood.plot +
  geom_smooth(
    method = "lm",
    level = 0,
    color = "darkorange1",
    size = 1.5
  ) +
  geom_point(
    data = LCN.means,
    aes(x = Group, y = Mean),
    color = "white",
    size = 4,
    pch = 23,
    fill = "orchid4"
  )

# Display the plot of averages
blood.plot.groups

# Give the slope and intercept of the linear regression model
blood.fit <- lm(LCN ~ WBC, data = blood)
blood.fit$coefficients

# Give the summary of the line fitted to blood
summary( blood.fit )

# Address heteroscedasticity by taking log
alt.plot.1 <- ggplot(blood, aes(x = WBC, y = log(LCN))) +
  geom_point() +
  geom_smooth(method = "lm")

alt.plot.2 <- ggplot(blood, aes(x = log(WBC), y = log(LCN))) +
  geom_point(size = 0.5, alpha = 0.1) +
  geom_smooth(method = "lm")

# Exploring a more exact way to find correlated variables -----
#?gather

#?list

#which.max(cor.matrix)

#cor.matrix <- cor.matrix %>%
#  gather()

#cor.list <- data.frame()
#for (i in nrow(cor.matrix)) {
#  for (j in ncol(cor.matrix)) {
#    a = c( i, j, cor.matrix[i,j] )
#    cor.list <- rbind( cor.list, a )
#  }
#}
