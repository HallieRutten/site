library(tidyverse)

# load GSS data -- check your working directory!
load(file = "gss2018.rda")

# rename as in class:
gss.2018 <- GSS
rm(GSS)

# examine the variables
summary(object = gss.2018)

# clean data for marital status, sex, use tech
gss.2018.cleaned <- gss.2018 %>%
  select(SEX, USETECH, MARITAL) %>%
  mutate(USETECH = na_if(x = USETECH, y = -1)) %>%
  mutate(USETECH = na_if(x = USETECH, y = 999)) %>%
  mutate(USETECH = na_if(x = USETECH, y = 998)) %>%
  mutate(SEX = factor(x = SEX, labels = c("male","female"))) %>%
  mutate(MARITAL = na_if(x = MARITAL, y = 9)) %>% 
  mutate(MARITAL = factor(x = MARITAL, labels = c("Married",
                                              "Widowed",
                                              "Divorced",
                                              "Separated",
                                              "Never married")))
# check recoding
summary(object = gss.2018.cleaned)

# descriptive stats for usetech
gss.2018.cleaned %>%
  drop_na(USETECH) %>% 
  summarize(m.tech = mean(USETECH),
            sd.tech = sd(USETECH))

# graph for usetech
gss.2018.cleaned %>%
  ggplot(aes(x = USETECH)) +
  geom_histogram()

# usetech by sex
gss.2018.cleaned %>%
  drop_na(USETECH, SEX) %>% 
  group_by(SEX) %>%
  summarize(m.tech = mean(USETECH),
            sd.tech = sd(USETECH))

# usetech by marital
gss.2018.cleaned %>%
  drop_na(USETECH, MARITAL) %>% 
  group_by(MARITAL) %>%
  summarize(m.tech = mean(USETECH),
            sd.tech = sd(USETECH))

# graph usetech by marital status
gss.2018.cleaned %>%
  drop_na(USETECH, MARITAL) %>% 
  ggplot(aes(y = USETECH, x = MARITAL)) +
  geom_boxplot() +
  labs(x = "Marital status category", 
       y = "Percent of work time spent using technology") +
  ylim(0, 100)

# usetech by sex & marital status
gss.2018.cleaned %>%
  drop_na(USETECH, MARITAL, SEX) %>% 
  group_by(SEX, MARITAL) %>%
  summarize(m.tech = mean(USETECH),
            sd.tech = sd(USETECH))

# mean tech use percent by marital status groups
oneway.test(formula = USETECH ~ MARITAL,
            data = gss.2018.cleaned,
            var.equal = TRUE)

# find differences in mean tech use by marital status groups
pairwise.t.test(x = gss.2018.cleaned$USETECH,
                g = gss.2018.cleaned$MARITAL,
                pool.sd = FALSE,
                p.adjust.method = "bonferroni")

# Divorced and never married people had statistically significantly different 
# mean time using tech at work compared to married people based on Bonferroni
# post-hoc tests. 
