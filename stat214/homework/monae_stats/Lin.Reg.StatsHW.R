#Monae Scott
#February 22nd, 2022 

#Load packages and data
library(tidyverse)
library(haven)

df<-read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.XPT")

#Plot of caffine and sugar

ggplot(df, aes(BMXARML,BMXLEG))+
  geom_point()+
  geom_smooth()

#Filter outliers and drop NA
df<-df%>%
  filter(BMXARML<40)%>%
  filter(BMXLEG<50)%>%
  select(BMXARML,BMXLEG )%>%
  drop_na()
#new plot
ggplot(df, aes(BMXARML,BMXLEG))+
  geom_point()+
  geom_smooth()

#null model
ybar=mean(df$BMXARML)

df<-df%>%
  rename(arm.length=BMXARML)%>%
  rename(leg.length=BMXLEG)

df<-df%>%
  mutate(Group=round(leg.length))

arm.length.means<-df%>%
  group_by(Group)%>%
  summarize(N=n(),
            Mean=mean(arm.length),
            SD= sd(arm.length))


df.fit<-lm(arm.length~leg.length, data=df)
df.fit$coefficients

summary(df.fit)
