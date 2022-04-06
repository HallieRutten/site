library(tidyverse)
library(gridExtra)


births <- read.csv("births.csv")

births$Pop2010 <- births$Pop2010/1000
births_by_county <- births %>%
  group_by(county) %>% 
  summarize( Mean = mean(births),
             Variance = var(births),
             Ratio = Variance/Mean ) %>% 
  arrange( desc(Ratio))

# init dec17
dec17_all <- births %>% 
  filter( month=="December" & year==2017)
ggplot( dec17_all, aes( x=Pop2010, y=births)) +
  geom_point() +
  geom_smooth( method="glm.nb" )
dec17_all_fit <- glm.nb( births ~ Pop2010, data=dec17_all)
summary( dec17_all_fit )

# nbm
dec17_all$Pop2010 <- log( dec17_all$Pop2010 )
ggplot( dec17_all, aes( x=Pop2010, y=births)) +
  geom_point() +
  geom_smooth( method="glm.nb" )
dec17_all_fit <- glm.nb( births ~ Pop2010, data=dec17_all)
summary( dec17_all_fit )

# san diego ----
diego <- births %>% 
  filter( county == "San Diego County, CA")
new_data <- data.frame( Pop2010 = log(658.466))
mu <- predict( dec17_all_fit, newdata = new_data, type="response")
diego <- diego %>% 
  mutate( simulation = rnbinom( nrow(diego), mu=mu[1], size=dec17_all_fit$theta))
ggplot( diego %>% dplyr::select( births, simulation) %>% pivot_longer(births:simulation), 
        aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol=1) +
  labs( title="San Diego County, CA")

# bartow ----
bartow <- births %>% 
  filter( county == "Bartow County, GA")
new_data <- data.frame( Pop2010 = log(658.466))
mu <- predict( dec17_all_fit, newdata = new_data, type="response")
bartow <- bartow %>% 
  mutate( simulation = rnbinom( nrow(bartow), mu=mu[1], size=dec17_all_fit$theta))
ggplot( bartow %>% dplyr::select( births, simulation) %>% pivot_longer(births:simulation), 
        aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol=1) +
  labs( title="Bartow County, GA")