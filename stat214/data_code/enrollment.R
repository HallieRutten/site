library(tidyverse)

enroll <- read_csv("enrollment.csv")

ggplot(enroll, aes(Births, Enrollment)) +
  geom_point()

enroll <- enroll %>%
  mutate(Births = Births / 1000000,
         Enrollment = Enrollment / 1000000)

ggplot(enroll, aes(Births, Enrollment)) +
  geom_point()

# enrollment trends ; will combine with predictions below
trend <- enroll %>%
  filter(EnrollmentYear > 1980 & EnrollmentYear < 2019 )

ggplot( trend, aes(EnrollmentYear, Enrollment)) +
  geom_line() + 
  geom_point() +
  labs( x = "Year",
        y = "Enrollment (millions)",
        title = "Enrollment trend, 1981 to 2018")

train <- enroll %>%
  filter(BirthYear < 1999)

ggplot(train, aes(Births, Enrollment)) +
  geom_point() +
  labs(x = "Births (millions)",
       y = "Enrollment (millions)")

train <- train %>%
  mutate(BirthDecade = BirthYear - BirthYear %% 10)

train$BirthDecade <- as.factor(train$BirthDecade)
levels(train$BirthDecade)

ggplot(train, aes(Births, Enrollment, color = BirthDecade)) +
  geom_point(size = 2)  +
  labs(x = "Births (millions)",
       y = "Enrollment (millions)")

train <- train %>%
  filter(BirthYear > 1978)

ggplot(train, aes(Births, Enrollment, color = BirthDecade)) +
  geom_point(size = 2)  +
  labs(x = "Births (millions)",
       y = "Enrollment (millions)")

cor(train$Births, train$Enrollment)

# null model via lm 
null.fit <- lm(Enrollment ~ 1, data = train)
null.fit

# note: this is just the average of the enrollments in the training set
mean( train$Enrollment)

enroll.fit <- lm(Enrollment ~ Births, data = train)
summary(enroll.fit)

# ggplot can display separate regression lines for groups:
ggplot(train, aes(Births, Enrollment, color = BirthDecade)) +
  geom_point(size = 2)  +
  geom_smooth(method = "lm", level = 0) +
  labs(x = "Births (millions)",
       y = "Enrollment (millions)")

# but we just want one overall regression model here:
ggplot(train, aes(Births, Enrollment)) +
  geom_point(size = 2)  +
  geom_smooth(method = "lm", level = 0) +
  labs(x = "Births (millions)",
       y = "Enrollment (millions)")

new_data <- enroll %>%
  filter(BirthYear > 1998) %>%
  select(BirthYear, Births, EnrollmentYear)

predictions <- predict(enroll.fit, new_data)
predictions <- data.frame(
  BirthYear = new_data$BirthYear,
  Births = new_data$Births,
  EnrollmentYear = new_data$EnrollmentYear,
  Enrollment = predictions
)

ggplot( train, aes(Births, Enrollment)) +
  geom_point() +
  geom_smooth( method="lm", level=0) + 
  geom_point( data = predictions, aes( Births, Predictions), color="tomato", size=2) +
  labs(x = "Births (millions)",
       y = "Enrollment (millions)")

predictions <- predictions %>%
  mutate( Type = "Predicted" )

trend <- trend %>% 
  mutate( Type = "Observed" )

combined <- rbind( trend, predictions)

ggplot( combined, aes(EnrollmentYear, Enrollment, color=Type)) +
  geom_point() + 
  geom_line() +
  geom_hline( yintercept=19, linetype="dashed") +
  labs( x = "Year",
        y = "Enrollment (millions)",
        title = "Observed and predicted enrollments, 1981 to 2038")
