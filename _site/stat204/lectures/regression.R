library(tidyverse)

# Get the data --------------------------

heights <- read_csv("heights.csv")
UN11 <- read_csv("UN11.csv")

# Heights -------------------------------

# scatterplot of daughters' heights versus mothers' heights:
ggplot( heights, aes(x=mheight, y=dheight)) + 
  geom_point()

# add the regression line to the scatterplot (no confidence bands):
ggplot( heights, aes(x=mheight, y=dheight)) + 
  geom_point() +
  geom_smooth(method="lm", level=0)

# add the regression line to the scatterplot (with confidence bands):
ggplot( heights, aes(x=mheight, y=dheight)) + 
  geom_point() +
  geom_smooth(method="lm")

# get the regression equation (basic):
lm( dheight ~ mheight, data=heights)

# get the regression equation (more info. and diagnostics):
heights.fit <- lm( dheight ~ mheight, data=heights)
summary(heights.fit)

# compute the correlation coefficient
cor( heights$mheight, heights$dheight )

# United Nations data --------------------

# scatterplot of life expectancy versus fertility
ggplot( UN11, aes(x=fertility, y=lifeExpF)) +
  geom_point()

# scatterplot of life expectancy versus fertility, with point shapes/colors 
#   determined by group (OECD, Other, Africa)
ggplot( UN11, aes(x=fertility, y=lifeExpF)) +
  geom_point( aes(shape=group, color=group), size=2 ) +
  scale_color_brewer(palette = "Dark2")

# scatterplot of life expectancy versus fertility, with point shapes/colors 
#   determined by group (OECD, Other, Africa) and regression line
ggplot( UN11, aes(x=fertility, y=lifeExpF)) +
  geom_point( aes(shape=group, color=group), size=2 ) +
  scale_color_brewer(palette = "Dark2") +
  geom_smooth(method="lm", level=0)

# get the regression equation (basic):
lm( lifeExpF ~ fertility, data=UN11)

# get the regression equation (more info. and diagnostics):
un.fit <- lm( lifeExpF ~ fertility, data=UN11)
summary(un.fit)

# compute the correlation coefficient
cor( UN11$fertility, UN11$lifeExpF )

