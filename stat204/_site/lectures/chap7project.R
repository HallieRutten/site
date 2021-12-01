library(tidyverse)

# load GSS data -- check your working directory!
load(file = "gss2018.rda")

# assign GSS to gss.2018 (as done in class)
gss.2018 <- GSS
rm(GSS)

# check out the data
summary(object = gss.2018)

# clean the data of interest:
gss.2018.cleaned <- gss.2018 %>%
  select(HAPPY, SEX, DEGREE, USETECH, AGE) %>%
  mutate(USETECH = na_if(x = USETECH, y = -1)) %>%
  mutate(USETECH = na_if(x = USETECH, y = 999)) %>%
  mutate(USETECH = na_if(x = USETECH, y = 998)) %>%
  mutate(AGE = na_if(x = AGE, y = 98)) %>%
  mutate(AGE = na_if(x = AGE, y = 99)) %>%
  mutate(DEGREE = na_if(x = DEGREE, y = 8)) %>%
  mutate(DEGREE = na_if(x = DEGREE, y = 9)) %>%
  mutate(HAPPY = na_if(x = HAPPY, y = 8)) %>%
  mutate(HAPPY = na_if(x = HAPPY, y = 9)) %>%
  mutate(HAPPY = na_if(x = HAPPY, y = 0)) %>%
  mutate(SEX = factor(x = SEX, labels = c("male","female"))) %>%
  mutate(DEGREE = factor(x = DEGREE, labels = c("< high school",
                                                "high school", "junior college",
                                                "bachelor", "graduate"))) %>%
  mutate(HAPPY = factor(x = HAPPY, labels = c("very happy",
                                              "pretty happy",
                                              "not too happy")))
# check recoding
summary(object = gss.2018.cleaned)

# mean and sd of age by group
use.stats <- gss.2018.cleaned %>%
  drop_na(USETECH) %>%
  group_by(DEGREE) %>%
  summarize(m.techuse = mean(x = USETECH),
            sd.techuse = sd(x = USETECH))
use.stats

# graph usetech (Figure 7.4)
gss.2018.cleaned %>%
  drop_na(USETECH) %>%
  ggplot(aes(y = USETECH, x = DEGREE)) +
  geom_jitter(aes(color = DEGREE), alpha = .6) +
  geom_boxplot(aes(fill = DEGREE), alpha = .4) +
  scale_fill_brewer(palette = "Dark2", guide = "none") +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  theme_minimal() +
  labs(x = "Highest educational attainment", y = "Percent of time spent using technology")

# mean tech use percent by degree groups
techuse.by.deg <- oneway.test(formula = USETECH ~ DEGREE,
                              data = gss.2018.cleaned,
                              var.equal = TRUE)
techuse.by.deg

bonf.tech.by.deg <- pairwise.t.test(x = gss.2018.cleaned$USETECH,
                                    g = gss.2018.cleaned$DEGREE,
                                    p.adjust.method = "bonferroni")
bonf.tech.by.deg

# Tukey's post hoc test for tech.by.deg
tukey.tech.by.deg <- TukeyHSD(x = aov(formula = USETECH ~ DEGREE,
                                      data = gss.2018.cleaned))
tukey.tech.by.deg
