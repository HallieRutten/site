#needed libraries----
library(tidyverse)
library (haven)

#read data----
weight<- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/WHQ_I.XPT")
view(weight)

#plot data----
nor.plot<-ggplot(weight, aes(x=WHD010, y=WHD020)) +
  geom_point()
nor.plot

#simplify data----
train1<- weight %>%
  select(WHD010, WHD020) %>%
  mutate (Height = WHD010 , Weight = WHD020) %>%
  select (Height , Weight) %>%
  filter(Height < 80, Height > 55, Weight < 500)

view(train1)

#view correlation----
cor(train1$Height,train1$Weight)

#average guess----
y.bar<-(mean(train1$Weight))
#plot data----
nor.plot<-ggplot(train1, aes(x=Height, y=Weight)) +
  geom_point() +
  geom_hline( yintercept = y.bar,
              color ="blue",
              linetype = "dashed")
nor.plot

#create averages of weight with each height---- 

 Weight.means <-train1 %>% 
    group_by(Height) %>%
    summarize(N = n(),
              Mean = mean(Weight),
              SD = sd(Weight))
view(Weight.means)

#plot graph with group averages----
nor.plot<-ggplot(train1, aes(x=Height, y=Weight)) +
  geom_point() +
  geom_hline( yintercept = y.bar,
              color ="blue",
              linetype = "dashed") +
  geom_point(data = Weight.means,
             aes(x=Height,y=Mean),
             color="hotpink",
             size=2)
nor.plot
#make LM model----
weight.fit<-lm(Height~Weight, train1) 
summary(weight.fit) 

#plot graph with averages and LM ----
nor.plot + 
  geom_smooth(method = "lm", level = 0)
