library(tidyverse)

# DATA ACQUISITION AND EXPLORATION -----

# get the births and enrollment data -----
enroll <- read_csv("enrollment.csv")

# plot the data -----
ggplot(enroll, aes(Births, Enrollment)) +
  geom_point()

# change units to millions to simplify plots etc. -----
enroll <- enroll %>%
  mutate(Births = Births / 1000000,
         Enrollment = Enrollment / 1000000)

# plot the results -----
ggplot(enroll, aes(Births, Enrollment)) +
  geom_point() +
  labs( x = "Births (millions)",
        y = "Enrollment (millions)",
        title = "Undergraduate enrollments versus births")

# extract then plot the enrollment trends; will combine with predictions below -----
trend <- enroll %>%
  filter(EnrollmentYear > 1980 & EnrollmentYear < 2019 )

ggplot( trend, aes(EnrollmentYear, Enrollment)) +
  geom_line() + 
  geom_point() +
  labs( x = "Year",
        y = "Enrollment (millions)",
        title = "Undergraduate enrollments, 1981 to 2018")

# build a training set for the regression model -----
train <- enroll %>%
  filter(BirthYear < 1999)

# plot the training data -----
# note: this doesn't _look_ any different, but actually look at the
# training set versus the original to see the rows of NAs that have
# now been removed!
ggplot(train, aes(Births, Enrollment)) +
  geom_point() +
  labs(x = "Births (millions)",
       y = "Enrollment (millions)")

# add a grouping variable -----
# then use the grouping variable to see what's going on in the data 
train <- train %>%
  mutate(BirthDecade = BirthYear - BirthYear %% 10)

# the following could also be accomplished with a `mutate` call if you prefer...
train$BirthDecade <- as.factor(train$BirthDecade)
levels(train$BirthDecade)

ggplot(train, aes(Births, Enrollment, color = BirthDecade)) +
  geom_point(size = 2)  +
  labs(x = "Births (millions)",
       y = "Enrollment (millions)")

# the linear relationship holds for those born after 1978, 
# so modify the training set accordingly -----
train <- train %>%
  filter(BirthYear > 1978)

# ta-da! a very strong linear relationship -----
ggplot(train, aes(Births, Enrollment, color = BirthDecade)) +
  geom_point(size = 2)  +
  labs(x = "Births (millions)",
       y = "Enrollment (millions)")

cor(train$Births, train$Enrollment)

# MODELS -----

# null model via lm -----
null.fit <- lm(Enrollment ~ 1, data = train)
null.fit

# note: the null model is just the average of the enrollments in the training set
mean( train$Enrollment)

# simple linear regression model -----
enroll.fit <- lm(Enrollment ~ Births, data = train)
summary(enroll.fit)

# ggplot _can_ display separate regression lines for groups -----
ggplot(train, aes(Births, Enrollment, color = BirthDecade)) +
  geom_point(size = 2)  +
  geom_smooth(method = "lm", level = 0) +
  labs(x = "Births (millions)",
       y = "Enrollment (millions)")

# but we really just want one overall regression model here -----
ggplot(train, aes(Births, Enrollment)) +
  geom_point(size = 2)  +
  geom_smooth(method = "lm", level = 0) +
  labs(x = "Births (millions)",
       y = "Enrollment (millions)")

# prepare data for making predictions -----
new_data <- enroll %>%
  filter(BirthYear > 1998) %>%
  select(BirthYear, Births, EnrollmentYear)

# make the predictions -----
predictions <- predict(enroll.fit, new_data)
predictions <- data.frame(
  BirthYear = new_data$BirthYear,
  Births = new_data$Births,
  EnrollmentYear = new_data$EnrollmentYear,
  Enrollment = predictions
)

# check the results -----
ggplot( train, aes(Births, Enrollment)) +
  geom_point() +
  geom_smooth( method="lm", level=0) + 
  geom_point( data = predictions, aes( Births, Enrollment), color="tomato", size=2) +
  labs(x = "Births (millions)",
       y = "Enrollment (millions)")

# add a factor to distinguish between observations and predictions
predictions <- predictions %>%
  mutate( Type = "Predicted" )

trend <- trend %>% 
  mutate( Type = "Observed" )

combined <- rbind( trend, predictions) %>%
  mutate( Type = as.factor(Type))

# finally! visualize the observed and predicted undergraduate enrollments over time -----
# note the _demographic cliff_ around 2026/2027!
ggplot( combined, aes(EnrollmentYear, Enrollment, color=Type)) +
  geom_point() + 
  geom_line() +
  geom_hline( yintercept=19, linetype="dashed") +
  labs( x = "Year",
        y = "Enrollment (millions)",
        title = "Observed and predicted enrollments, 1981 to 2038")
