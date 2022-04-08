# regression homework 3/31 (new counties poisson and negative binomial)

library(readr)
library(tidyverse)
library(gridExtra)
library(MASS)

births <- read_csv("births.csv")

births$Pop2010 <- births$Pop2010 / 1000
births_by_county <- births %>%
  group_by(county) %>% 
  summarize( Mean = mean(births),
             Variance = var(births),
             Ratio = Variance/Mean ) %>% 
  arrange( desc(Ratio))

poisson_births <- births_by_county %>% 
  filter( abs( Ratio - 1 ) < 0.1 ) %>%
  pull( county )
poisson_births <- births %>%
  filter( county %in% poisson_births )

ggplot( poisson_births, aes( x=Pop2010, y=births)) +
  geom_point() +
  geom_smooth( method="glm", method.args=list(family="poisson")) 

poisson_births <- poisson_births %>%
  group_by(county) %>%
  mutate( Mean = mean(births)) 

jan16 <- poisson_births %>% 
  filter( month=="January" & year==2016) %>%
  arrange(Pop2010)

jan16_fit <- glm( births ~ Pop2010, data=jan16, family="poisson")
summary(jan16_fit)

ggplot( jan16, aes( x=Pop2010, y=births)) +
  geom_point() +
  geom_smooth( method="glm", method.args=list(family="poisson"))

# in Catawba County, NC:
new_data <- data.frame( Pop2010 = c(154.358, 235.406))
(lambda <-predict( jan16_fit, newdata=new_data, type="response" ))
catawba <- poisson_births %>%
  filter( county == "Catawba County, NC")

catawba <- catawba %>%
  mutate( simulation = rpois( n=nrow(catawba), lambda=lambda[1]))

ggplot( catawba %>% dplyr::select( births, simulation) %>% pivot_longer(births:simulation), 
        aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol=1, scales = "free") +
  labs( title="Catawba County, NC")

# ended up better than Sheboygan County

# in Sheboygan County, WI (change 2nd number above when doing this county based on pop)
sheboygan <- poisson_births %>%
  filter( county == "Sheboygan County, WI")
sheboygan <- sheboygan %>%
  mutate( simulation = rpois( n=nrow(sheboygan), lambda=lambda[2]))

ggplot( sheboygan %>% dplyr::select( births, simulation) %>% pivot_longer(births:simulation), 
        aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol=1) +
  labs( title="Sheboygan County, WI")

# ended up not very good

# negative binomial regression

jan16_all <- births %>% 
  filter( month=="January" & year==2016) %>%
  arrange (desc(Pop2010))

ggplot( jan16_all, aes( x=Pop2010, y=births)) +
  geom_point() +
  geom_smooth( method="glm.nb")

jan16_all_fit <- glm.nb( births ~ Pop2010, data=jan16_all)

summary( jan16_all_fit )

new_data <- data.frame( Pop2010 = c(1748.066, 9818.605))
(mu <-predict( jan16_all_fit, newdata=new_data, type="response" ))

broward <- births %>%
  filter( county == "Broward County, FL")

broward <- broward %>%
  mutate( simulation = rnbinom( n=nrow(broward), mu=mu[1], size=jan16_all_fit$theta))

ggplot( broward %>% dplyr::select( births, simulation) %>% pivot_longer(births:simulation), 
        aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol=1, scales = "free") +
  labs( title="Broward County, FL")

# broward not very good

losangeles <- births %>%
  filter( county == "Los Angeles County, CA")

losangeles <- losangeles %>%
  mutate( simulation = rnbinom( n=nrow(losangeles), mu=mu[1], size=jan16_all_fit$theta))

ggplot( losangeles %>% dplyr::select( births, simulation) %>% pivot_longer(births:simulation), 
        aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol=1, scales = "free") +
  labs( title="Los Angeles County, CA")

# worse than broward