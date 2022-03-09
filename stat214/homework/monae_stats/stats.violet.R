#Monae Scott
#February 2022

#load packages and get data 
library(tidyverse)

violets<-read.csv("http://mbrudd.sewanee.edu/stat214/_site/data_code/violets.csv")
violets

violets<-violets%>%
  filter(Bud.type=="Chasmogamous")%>%
  mutate(Bud.present=as.integer(Bud.counts>0))

violets.1<-ggplot(violets, aes(x=Photoperiod, y=Bud.present))+
  geom_smooth(method="glm",
              method.args=list(family="binomial"),
              level=0)+
  geom_point(alpha=.08)

violets.1
#
group.size = 0.5

violets <- violets %>%
  mutate(Photoperiod.group = round(Photoperiod / group.size) * group.size)

violets.2 <- violets %>%
  group_by(Photoperiod.group) %>%
  summarise(Bud.mean = mean(Bud.present))


violets.1 +
  geom_point(
    data = violets.2,
    aes(x = Photoperiod.group, y = Bud.mean),
    size = 2)
