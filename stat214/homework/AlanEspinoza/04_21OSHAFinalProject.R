#Alan Espinoza
#April, 19th, 2022
#Final Project

#Count regression on injuries vs hours worked
#data from Osha

#libraries---------------------------------------------------------------------
library(tidyverse)
library(MASS)
library(gridExtra)

#set directory-----------------------------------------------------------------
setwd("D:/1_Semester4/STATS214/Data")

#scan data --------------------------------------------------------------------
osha<-read_csv("OshaData2016.csv")

#shape data into usable data---------------------------------------------------
train<- osha%>%
  mutate( injuries = osha$total_injuries, hours = osha$total_hours_worked,
         group_size = osha$size, employees = osha$annual_average_employees) %>%
  dplyr::select(injuries, hours, group_size, state, employees)%>%
  drop_na()%>%
  filter( injuries < 1000, hours < 500000000, hours > 1)
train$group_size<-as.factor(train$group_size)

#just plot itself!
ggplot(train, aes(x = hours, y = injuries, color = group_size))+
  geom_point()+
  labs(title = "Initial graph Gours vs Injuries")

#plot with geom smooth. bad
ggplot(train, aes(x = hours, y = injuries, color = group_size))+
  geom_point()+
  geom_smooth()+
  labs(title = " Initial graph line fitted with group factor" )

#log transform with factor
trainA<-train
trainA$group_size<-as.factor(trainA$group_size)
trainA$hours<-log(trainA$hours)

ggplot(trainA, aes(x = hours, y = injuries, color = group_size))+
  geom_point()+
  geom_smooth()+
  labs(title = "Improved graph with Log(Hours) vs Injuries ")

#graph by size-----------------------------------------------------------------
#same scale each model??
MeanT<- mean(train$injuries)
VarianceT<-var(train$injuries)
(RatioT<- VarianceT/MeanT)
#use negative binomial model for whole data because var and mean don't match!!
#start with group size 1 (small)________________________________________________
{train1<-train%>%
  filter(group_size == 1, hours < 10000000 )

Mean1<- mean(train1$injuries)
Variance1<-var(train1$injuries)
(Ratio1<- Variance1/Mean1)

train1$hours<- log(train1$hours)

Plot1<-ggplot(train1, aes(x = hours, y = injuries)) +
  geom_point()+
  geom_smooth(method = "glm.nb")+
  labs(title = "size 1")+
  xlim(0,21)
}
#start with group size 2 (medium)_______________________________________________
{train2<-train%>%
  filter(group_size == 2 , hours<150000000)

Mean2<- mean(train2$injuries)
Variance2<-var(train2$injuries)
(Ratio2<- Variance2/Mean2)

train2$hours<- log(train2$hours)

Plot2<-ggplot(train2, aes(x = hours, y = injuries)) +
  geom_point()+
  geom_smooth(method = "glm.nb")+
  labs(title = "size 2")+
  xlim(0,21)
}
#start with group size 3 (larger)_______________________________________________
{train3<-train%>%
  filter(group_size == 3, hours < 200000000)

Mean3<- mean(train3$injuries)
Variance3<-var(train3$injuries)
(Ratio3<- Variance3/Mean3)

train3$hours<- log(train3$hours)

Plot3<-ggplot(train3, aes(x = hours, y = injuries)) +
  geom_point()+
  geom_smooth(method= "glm.nb")+
  labs(title = "size 3")+
  xlim(0,21)
}
#graph all______________________________________________________________________
grid.arrange(Plot1, Plot2, Plot3, ncol=1)

#models------------------------------------------------------------------------
#null model_____________________________________________________________________
{trainA.fit<-glm.nb(injuries~hours, data = trainA)
summary(trainA.fit)

#null model to simulate peak of group 3 peak of all data
new_data <- data.frame( hours = log(24537640))
mu <- predict( trainA.fit, newdata = new_data, type="response")
trainAS <- trainA %>%
  mutate( simulation = rnbinom( nrow(trainA), mu=mu[1], size=trainA.fit$theta))

ggplot( trainAS %>% dplyr::select( injuries, simulation) %>% pivot_longer(injuries:simulation), 
        aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol=1) +
  labs( title="Simulation of peak injuries with whole data included")
}
#model using size factor________________________________________________________
{train.fit<-glm.nb(injuries~hours+group_size, data = trainA)
summary(train.fit)
}
#simulations peak in all group 3 
{new_data <- data.frame( hours = log(24537640), group_size = "3")
mu <- predict( train.fit, newdata = new_data, type="response")
train3S <- train3 %>%
  mutate( simulation = rnbinom( nrow(train3), mu=mu[1], size=train.fit$theta))

ggplot( train3S %>% dplyr::select( injuries, simulation) %>% pivot_longer(injuries:simulation), 
        aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol=1) +
  labs( title="Simulation count vs count of peak injuries in size 3 with only size 3 data")
}
#model with observations similar to hours of the peak of group 3________________
{trainP3<- train3%>%
  filter( 16.5<hours, hours < 17.5 )

#plot these observations
ggplot(trainP3, aes(injuries))+
  geom_histogram()+
  labs(title = "Count from hour range from {16.5,17.5} in size 3")

#simulations peak in 16.5 - 17.5
new_data <- data.frame( hours = log(24537640), group_size = "3")
mu <- predict( train.fit, newdata = new_data, type="response")
trainP3S <- trainP3 %>%
  mutate( simulation = rnbinom( nrow(trainP3), mu=mu[1], size=train.fit$theta))

ggplot( trainP3S %>% dplyr::select( injuries, simulation) %>% pivot_longer(injuries:simulation), 
        aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol=1) +
  labs( title="Simulation count vs count from hour range from {16.5,17.5} in size 3")
}
#model with observations similar to hours of the peak of group 2________________
{trainP2<- train2%>%
  filter( 16.2<hours, hours < 17.2)

#plot these observations
ggplot(trainP2, aes(injuries))+
  geom_histogram()+
  labs(title = "Count from hour range from {16.2,17.2} in size 2")

#simulations peak in 16.2 - 17.2
new_data <- data.frame( hours = log(17793934), group_size = "2")
mu <- predict( train.fit, newdata = new_data, type="response")
trainP2S <- trainP2 %>%
  mutate( simulation = rnbinom( nrow(trainP2), mu=mu[1], size=train.fit$theta))

ggplot( trainP2S %>% dplyr::select( injuries, simulation) %>% pivot_longer(injuries:simulation), 
        aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol=1) +
  labs( title="Simulation count vs count from hour range from {16.3,17.3} in size 2 ")
}
#model with observations similar to hours of the peak of group 1________________
{trainP1<- train1%>%
  filter( 15.3< hours, hours < 16.3)

#plot these observations
ggplot(trainP1, aes(injuries))+
  geom_histogram()+
  labs(title = "Count from hour range from {15.3,16.3} in size 1")

#simulations peak in 15.3 - 16.3
new_data <- data.frame( hours = log(7494987), group_size = "1")
mu <- predict( train.fit, newdata = new_data, type="response")
trainP1S <- trainP1 %>%
  mutate( simulation = rnbinom( nrow(trainP1), mu=mu[1], size=train.fit$theta))

ggplot( trainP1S %>% dplyr::select( injuries, simulation) %>% pivot_longer(injuries:simulation), 
        aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol=1) +
  labs( title="Simulation count vs count from hour range from {15.3,16.3} in size 1 ")
}
#good models came from group 2, 3 for simulating peak, but not for group 1.
#lets see if it was good for count in middles of model?
#get other observations with similar hours of the peak of group 3_______________
{(median(train3$hours))
trainM3<- train3%>%
  filter( 13<hours, hours < 14 )

#plot these observations
ggplot(trainM3, aes(injuries))+
  geom_histogram()+
  labs(title = "hours = 13 - 14")

#simulations peak in 13 - 14
new_data <- data.frame( hours = log(681545), group_size = "3")
mu <- predict( train.fit, newdata = new_data, type="response")
trainM3S <- trainM3 %>%
  mutate( simulation = rnbinom( nrow(trainM3), mu=mu[1], size=train.fit$theta))

ggplot( trainM3S %>% dplyr::select( injuries, simulation) %>% pivot_longer(injuries:simulation), 
        aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol=1) +
  labs( title="GroupSize 3 of hours 13 - 14 ")
}
#get other observations with similar hours of the peak of group 2_______________
{(median(train2$hours))
trainM2<- train2%>%
  filter( 11<hours, hours < 12)

#plot these observations
ggplot(trainM2, aes(injuries))+
  geom_histogram()+
  labs(title = "hours = 11 - 12")

#simulations peak in 11 - 12
new_data <- data.frame( hours = log(94393), group_size = "2")
mu <- predict( train.fit, newdata = new_data, type="response")
trainM2S <- trainM2 %>%
  mutate( simulation = rnbinom( nrow(trainM2), mu=mu[1], size=train.fit$theta))

ggplot( trainM2S %>% dplyr::select( injuries, simulation) %>% pivot_longer(injuries:simulation), 
        aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol=1) +
  labs( title="GroupSize 2 of hours 11 - 12 ")
}
#get other observations with similar hours of the peak of group 1_______________
{(median(train1$hours))
trainM1<- train1%>%
  filter( 9< hours, hours < 10)

#plot these observations
ggplot(trainM1, aes(injuries))+
  geom_histogram()+
  labs(title = "hours = 9 - 10")

#simulations peak in 9 - 10
new_data <- data.frame( hours = log(12773), group_size = "1")
mu <- predict( train.fit, newdata = new_data, type="response")
trainM1S <- trainM1 %>%
  mutate( simulation = rnbinom( nrow(trainM1), mu=mu[1], size=train.fit$theta))

ggplot( trainM1S %>% dplyr::select( injuries, simulation) %>% pivot_longer(injuries:simulation), 
        aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol=1) +
  labs( title="GroupSize 1 of hours 9 - 10 ")
}
#Bootstrap interval estimates for coefficients!--------------------------------

#model says is average is this while bootstrap says is this
# set number of bootstrap samples
n_boot <- 10
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(trainA), nrow(trainA), replace = TRUE )
  boot <- trainA[ idx, ]
  fit <- glm.nb(injuries ~ hours, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))
}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

#group size 3 coefficient bootstrap
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(trainA), nrow(trainA), replace = TRUE )
  boot <- trainA[ idx, ]
  fit <- glm.nb(injuries ~ hours+group_size, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[4]]))
}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

#group size 2 coefficient bootstrap
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(trainA), nrow(trainA), replace = TRUE )
  boot <- trainA[ idx, ]
  fit <- glm.nb(injuries ~ hours+group_size, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[3]]))
}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))

#group size 1 coefficient bootstrap
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(trainA), nrow(trainA), replace = TRUE )
  boot <- trainA[ idx, ]
  fit <- glm.nb(injuries ~ hours+group_size, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))
}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))
