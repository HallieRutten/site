
#libraries imported ---------------------
library(kableExtra)
library(tidyverse)
library(gridExtra)
library(DT)
library(MASS)

#data imported ----------------------
(births <- read_csv("site/stat214/data_code/counts/births.csv"))

#organize data ------------------------
births$Pop2010 <- births$Pop2010 / 1000

births_by_county <- births %>%
  group_by(county) %>% 
  summarize( Mean = mean(births),
             Variance = var(births),
             Ratio = Variance/Mean ) %>% 
  arrange( desc(Ratio))

datatable( births_by_county, caption = "Monthly birth statistics for U.S. counties")

#set up data for NB---------------------
jul18<-births%>%
  filter(month == "July" & year == "2018") %>%
  filter(Pop2010 < 1000)

ggplot(jul18, aes(x=Pop2010, y=births)) +
  geom_point() +
  geom_smooth(method="glm.nb")

#start bad? simulation-----------------------
starkpop = 375.586
etowahpop <- 104.430
newdata<-data.frame(Pop2010 = c(starkpop, etowahpop))

jul18allfit<-glm.nb(births~Pop2010,data=jul18)

mu<-predict(jul18allfit, newdata=newdata, type="response")

#neg binomial model

theta<-jul18allfit$theta

#simulation
eto<-births %>%
  filter(county == "Etowah County, AL")
eto <- eto %>%
  mutate( simulation = rnbinom( n=nrow(eto), size=theta, mu=mu[2]))

ggplot( eto %>% dplyr::select( births, simulation) %>% pivot_longer(births:simulation), 
        aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol=1) +
  labs( title="Etowah County, AL")

#start good? simulation---------------------------
(stark<- births %>%
  filter(county == "Stark County, OH"))
#simulation
stark <- stark %>%
  mutate( simulation = rnbinom( n=nrow(stark), size=theta ,mu=mu[1]))

ggplot(stark %>% dplyr::select( births, simulation) %>% pivot_longer(births:simulation), 
        aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol=1) +
  labs( title="Stark County, OH")

