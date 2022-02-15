#needed libraries----
library(tidyverse)
library (haven)

#read data----
sleepBehavior<- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/SLQ_I.XPT")
view(sleepBehavior)

#plot data----
train<- sleepBehavior %>%
  select(SLD012, SLQ120) %>%
  mutate (HourOfSleep = SLD012 , FeltSleepy = SLQ120) %>%
  select (HourOfSleep , FeltSleepy)

 view(train)

 ggplot(train, aes(x= HourOfSleep, y = FeltSleepy)) +
   geom_point()
 