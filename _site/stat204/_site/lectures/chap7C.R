library(tidyverse)

# GET, CLEAN, AND EXPLORE THE DATA
##################################

# Get the GSS data -- check your working directory!

load(file = "gss2018.rda")

# rename dataset for convenience (?) and remove other copy
gss.2018 <- GSS
rm(GSS)

# examine the variables
summary(object = gss.2018)

# recode variables of interest to valid ranges
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

# mean and sd of USETECH by DEGREE groups
use.stats <- gss.2018.cleaned %>%
  drop_na(USETECH) %>%
  group_by(DEGREE) %>%
  summarize(m.techuse = mean(x = USETECH),
            sd.techuse = sd(x = USETECH))
use.stats

# graph USETECH (Figure 7.4)
gss.2018.cleaned %>%
  drop_na(USETECH) %>%
  ggplot(aes(y = USETECH, x = DEGREE)) +
  geom_jitter(aes(color = DEGREE), alpha = .6) +
  geom_boxplot(aes(fill = DEGREE), alpha = .4) +
  scale_fill_brewer(palette = "Spectral", guide = FALSE) +
  scale_color_brewer(palette = "Spectral", guide = FALSE) +
  theme_minimal() +
  labs(x = "Highest educational attainment", y = "Percent of time spent using technology")

# ONE-WAY ANOVA
################

# mean tech use percent by degree groups
techuse.by.deg <- oneway.test(formula = USETECH ~ DEGREE,
                              data = gss.2018.cleaned,
                              var.equal = TRUE)
techuse.by.deg

# POST HOC TESTS
################

# find differences in mean tech use by degree groups
bonf.tech.by.deg <- pairwise.t.test(x = gss.2018.cleaned$USETECH,
                                    g = gss.2018.cleaned$DEGREE,
                                    p.adj = "bonf")
bonf.tech.by.deg

# mean age by groups
use.stats

# Tukey's post hoc test for tech.by.deg
tukey.tech.by.deg <- TukeyHSD(x = aov(formula = USETECH ~ DEGREE,
                                      data = gss.2018.cleaned))
tukey.tech.by.deg

# run the ANOVA and get a new object
anova.for.Tukey <- aov(formula = USETECH ~ DEGREE,
                       data = gss.2018.cleaned)

# use the newly created ANOVA object in TukeyHSD
tukey.tech.by.deg <- TukeyHSD(x = anova.for.Tukey)
tukey.tech.by.deg

# graph tech use by degree (Figure 7.12)
gss.2018.cleaned %>%
  drop_na(USETECH) %>%
  ggplot(aes(x = USETECH)) +
  geom_density(aes(fill = DEGREE)) +
  facet_wrap(facets = vars(DEGREE), nrow = 2) +
  scale_fill_brewer(palette = "Spectral", guide = FALSE) +
  theme_minimal() +
  labs(x = "Percent of time using tech",
       y = "Probability density")

# graph tech use by degree (Figure 7.13)
gss.2018.cleaned %>%
  drop_na(USETECH) %>%
  ggplot(aes(sample = USETECH)) +
  geom_abline(aes(intercept = mean(USETECH), slope = sd(USETECH), linetype =
                    "Normally distributed"),
              color = "gray60", size = 1) +
  stat_qq(aes(color = DEGREE)) +
  scale_color_brewer(palette = "Spectral", guide = FALSE) +
  scale_linetype_manual(values = 1, name = "") +
  labs(x = "Theoretical normal distribution",
       y = "Observed values of percent time using tech") +
  theme_minimal() +
  facet_wrap(facets = vars(DEGREE), nrow = 2)

# statistical test of normality for groups
gss.2018.cleaned %>%
  drop_na(USETECH) %>%
  group_by(DEGREE) %>%
  summarize(shapiro.pval = shapiro.test(x = USETECH)$p.value)


# equal variances for systolic by sex
car::leveneTest(y = USETECH ~ DEGREE, data = gss.2018.cleaned)

# welch test for unequal variances
welch.usetech.by.degree <- oneway.test(formula = USETECH ~ DEGREE,
                                       data = gss.2018.cleaned,
                                       var.equal = FALSE)
welch.usetech.by.degree

#compare usetech by degree
kw.usetech.by.degree <- kruskal.test(formula = USETECH ~ DEGREE,
                                     data = gss.2018.cleaned)
kw.usetech.by.degree

# post hoc test for usetech by degree
dunn.usetech.by.degree <- dunn.test::dunn.test(x =gss.2018.cleaned$USETECH,
                                               g = gss.2018.cleaned$DEGREE,
                                               method = "bonferroni")
# TWO-WAY ANOVA
###############

# graph usetech by sex (Figure 7.18)
gss.2018.cleaned %>%
  ggplot(aes(y = USETECH, x = SEX)) +
  geom_jitter(aes(color = SEX), alpha = .4) +
  geom_boxplot(aes(fill = SEX), alpha = .6) +
  scale_fill_manual(values = c("gray70", "#7463AC"), guide = FALSE) +
  scale_color_manual(values = c("gray70", "#7463AC"), guide = FALSE) +
  theme_minimal() +
  labs(x = "Sex", y = "Percent of time spent using technology")

# graph usetech by degree and sex (Figure 7.19)
gss.2018.cleaned %>%
  ggplot(aes(y = USETECH, x = DEGREE)) +
  geom_boxplot(aes(fill = SEX), alpha = .4) +
  scale_fill_manual(values = c("gray70", "#7463AC")) +
  theme_minimal() +
  labs(x = "Educational attainment", y = "Percent of time spent using technology")

# means plots graph (Figure 7.20)
gss.2018.cleaned %>%
  ggplot(aes(y = USETECH, x = DEGREE, color = SEX)) +
  stat_summary(fun.y = mean, geom="point", size = 3) +
  stat_summary(fun.y = mean, geom="line", aes(group = SEX), size = 1) +
  scale_color_manual(values = c("gray70", "#7463AC")) +
  theme_minimal() +
  labs(x = "Educational attainment", y = "Percent of time spent using technology") +
  ylim(0, 100)

# means by degree and sex
use.stats.2 <- gss.2018.cleaned %>%
  group_by(DEGREE, SEX) %>%
  drop_na(USETECH) %>%
  summarize(m.techuse = mean(USETECH),
            sd.techuse = sd(USETECH))
use.stats.2

# two-way ANOVA technology use by degree and sex
techuse.by.deg.sex <- aov(formula = USETECH ~ DEGREE * SEX, data =
                            gss.2018.cleaned)
summary(techuse.by.deg.sex)

#Tukey's HSD post hoc test
TukeyHSD(x = techuse.by.deg.sex)

# statistical test of normality for groups
shapiro.test(x = techuse.by.deg.sex$residuals)
