#libraries----
library(tidyverse)

#read data----
violets<-read_csv("https://mbrudd.sewanee.edu/stat214/
                  _site/data_code/violets.csv")
view(violets)

#make data we will change, make budcounts into binary
#only look at chasmogamous and find association between
#budcounts and phototperiod----
train<-violets

train <- train%>%
  select(`Bud type`,`Bud counts`,Photoperiod)%>%
  filter(`Bud type` == "Chasmogamous" )
train$`Bud counts`<-ifelse(train$`Bud counts`>0,1,0)
view(train)

#useless plot----

ggplot(train, aes (Photoperiod, `Bud counts`)) +
  geom_point()

#usefull plots----

train1<-train %>%
  group_by(Photoperiod) %>%
  summarize (N=n(), Percentage = sum(`Bud counts`)/N )
view(train1)

ggplot(train1 , aes (Photoperiod, Percentage)) +
  geom_point() +
  ylim(0,1)

#Fitting the logistic curve----

violet.fit<-glm(`Bud counts` ~ Photoperiod,
                data=train,
                family = "binomial")
violet.fit

ggplot(train1 , aes (Photoperiod, Percentage)) +
  geom_point() +
  geom_smooth(method = "glm",
              method.args=list(family=binomial),
              size = 1)

#threshold----

thres<-violet.fit$coefficients[1]/violet.fit$coefficients[2]
thres

#likelihood ratio
G2<-violet.fit$null.deviance-violet.fit$deviance

#probability of getting higher than 137 with 1DF----

pvalue<-pchisq(G2,1,lower.tail=FALSE)
pvalue
