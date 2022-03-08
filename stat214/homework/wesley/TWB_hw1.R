# T Wesley Bailey
# STAT214
# 
# Using NHANES 2017-2018 Lab Data

library(haven)
library(tidyverse)
library(dplyr)
data1 = read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/UM_J.XPT")
data2 = read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/VIC_J.XPT")
View(data1)
View(data2)
dat <- merge(data1, data2, by="SEQN")
View(dat)
cor.matrix <- (cor(dat, use="complete.obs"))
cor_df <- as.data.frame(as.table(cor.matrix))
cor_df %>%
  arrange(desc(Freq)) %>%
  filter(Freq>0.5)

ggplot(dat, aes(x=log(10*URXUTL), y=log(10*URXUCS)))+
  geom_point() +
  geom_smooth(method="lm")+
  labs(x='Thallium', y='Cesium')
(dat.fit <- lm( URXUTL ~ URXUCS, data=dat))
(cor(log(dat$URXUTL), log(dat$URXUCS), use="complete.obs"))

dat.fit <- lm(URXUTL ~ URXUCS, data = dat)


x.bar <- mean(dat$URXUTL)
y.bar <- mean(dat$URXUCS)
sd.x <- sd(dat$URXUTL)
sd.y <- sd(dat$URXUCS)
b <- (sd.y / sd.x)
a <- y.bar - b*x.bar
ggplot( dat, aes(x=log(10*URXUTL), log(10*URXUCS))) +
  geom_point() +
  geom_abline(intercept = a, slope = b, color="tomato")