library(tidyverse)
library(lsr)

# GET AND EXPLORE THE DATA
##########################

# Get the NHANES data -- check your working directory!
nhanes.2016 <- read.csv(file = "nhanes_2015-2016_ch6.csv")

# graph systolic blood pressure variable BPXSY1 (Figure 6.1)
nhanes.2016 %>%
  ggplot(aes(x = BPXSY1)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  theme_minimal() +
  labs(x = "Systolic blood pressure (mmHg)",
       y = "NHANES participants")

# graph systolic blood pressure BPXSY1 with risk indicator (Figure 6.2)
nhanes.2016 %>%
  ggplot(aes(x = BPXSY1, fill = BPXSY1 > 120)) +
  geom_histogram(color = "white") +
  theme_minimal() +
  scale_fill_manual(values = c("#7463AC","gray"),
                    labels = c("Normal range", "At-risk or high"),
                    name = "Systolic\nblood pressure") +
  labs(x = "Systolic blood pressure (mmHg)",
       y = "Number of NHANES participants")

# graph diastolic blood pressure BPXDI1 with risk indicator (Figure 6.3)
nhanes.2016 %>%
  ggplot(aes(x = BPXDI1, fill = BPXDI1 > 80)) +
  geom_histogram(color = "white") +
  theme_minimal() +
  scale_fill_manual(values = c("#7463AC", "gray"),
                    labels = c("Normal range", "At-risk or high"),
                    name = "Blood pressure") +
  labs(x = "Diastolic blood pressure (mmHg)",
       y = "Number of NHANES participants")

# mean and sd of systolic blood pressure with sample size
nhanes.2016 %>%
  drop_na(BPXSY1) %>%
  summarize(m.sbp = mean(x = BPXSY1),
            sd.sbp = sd(x = BPXSY1),
            n.spb = n())

# t-TESTS :
# One sample ; two sample ; paired
##################################

# direct calculation of t-statistic a.k.a. t-score and relevant p-values
m.sbp <- mean(nhanes.2016$BPXSY1,na.rm=TRUE)
sd.sbp <- sd(nhanes.2016$BPXSY1,na.rm=TRUE)
n.sbp <- 7145
t.statistic <- (m.sbp - 120) / (sd.sbp/sqrt(n.sbp))
t.statistic

# 1-sided p-value
pt(t.statistic, df=n.sbp-1, lower.tail = FALSE)
ggplot() + 
  stat_function(fun=dt, args=list(df=7144)) + 
  stat_function(fun=dt, args=list(df=7144), geom="area", xlim=c(t.statistic,4)) + 
  xlim(-4,4)

# 2-sided p-value 
2*pt(t.statistic, df=n.sbp-1, lower.tail = FALSE)
ggplot() + 
  stat_function(fun=dt, args=list(df=7144)) + 
  stat_function(fun=dt, args=list(df=7144), geom="area", xlim=c(t.statistic,4)) + 
  stat_function(fun=dt, args=list(df=7144), geom="area", xlim=c(-4,-1*t.statistic)) + 
  xlim(-4,4)

# built-in t.test
t.test(x = nhanes.2016$BPXSY1, mu = 120)

#6.5.5 Achievement 2
#create a subset of the data frame of people 65+ years old
nhanes.2016.65plus <- nhanes.2016 %>%
  filter(RIDAGEYR >= 65)

# comparing mean of BPXSY1 to 120
t.test(x = nhanes.2016.65plus$BPXSY1, mu = 120)

# compare means of BPXSY1 across groups
# sex variable is RIAGENDR
nhanes.2016 %>%
  drop_na(BPXSY1) %>%
  group_by(RIAGENDR) %>%
  summarize(m.sbp = mean(x = BPXSY1))

# add labels to sex and rename variables
nhanes.2016.cleaned <- nhanes.2016 %>%
  mutate(RIAGENDR = recode_factor(.x = RIAGENDR,
                                  `1` = 'Male',
                                  `2` = 'Female')) %>%
  rename(sex = RIAGENDR) %>%
  rename(systolic = BPXSY1)

# compare means of systolic by sex
nhanes.2016.cleaned %>%
  drop_na(systolic) %>%
  group_by(sex) %>%
  summarize(m.sbp = mean(x = systolic))
  
# density plot of systolic by sex (Figure 6.8)
nhanes.2016.cleaned %>%
  ggplot(aes(x = systolic,
             fill = sex)) +
  geom_density(alpha = .8) +
  theme_minimal() +
  labs(x = "Systolic blood pressure", y = "Probability density") +
  scale_fill_manual(values = c('gray', '#7463AC'),
                    name = "Sex")

# compare means of systolic by sex
nhanes.2016.cleaned %>%
  drop_na(systolic) %>%
  group_by(sex) %>%
  summarize(m.sbp = mean(x = systolic),
            var.sbp = var(x = systolic),
            samp.size = n())

# compare systolic blood pressure for males and females
t.test(formula = nhanes.2016.cleaned$systolic ~ nhanes.2016.cleaned$sex)

# rename second systolic measure and create diff variable for
# difference between measure 1 and 2 for systolic BP
nhanes.2016.cleaned <- nhanes.2016.cleaned %>%
  rename(systolic2 = BPXSY2) %>%
  mutate(diff.syst = systolic - systolic2)

# mean of the differences
nhanes.2016.cleaned %>%
  drop_na(diff.syst) %>%
  summarize(m.sbp = mean(x = diff.syst),
            var.sbp = var(x = diff.syst),
            n = n())

# histogram of the differences between first and second (Figure 6.10)
# blood pressure measures
nhanes.2016.cleaned %>%
  ggplot(aes(x = diff.syst)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  theme_minimal() +
  labs(x = "Difference between SBP Measures 1 and 2",
       y = "Number of NHANES participants")

# dependent-samples t-test for systolic measures 1 and 2
t.test(x = nhanes.2016.cleaned$systolic,
       y = nhanes.2016.cleaned$systolic2,
       paired = TRUE)

# COHEN'S D -- FOR MEASURING EFFECT SIZES 
#########################################

# Cohen's d effect size for one-sample t
lsr::cohensD(x = nhanes.2016.cleaned$systolic, mu = 120)

#6.8.2
# compare means of systolic by sex
nhanes.2016.cleaned %>%
  drop_na(systolic) %>%
  group_by(sex) %>%
  summarize(m.sbp = mean(systolic),
            var.sbp = var(systolic))

# Cohen's d effect size for independent samples test
lsr::cohensD(x = systolic ~ sex,
             data = nhanes.2016.cleaned,
             method = "unequal")

# variance and sample size of the difference variable
nhanes.2016.cleaned %>%
  drop_na(diff.syst) %>%
  summarize(m.sbp = mean(x = diff.syst),
            sd.sbp = sd(x = diff.syst))

# Cohen's d effect size for dependent samples test
lsr::cohensD(x = nhanes.2016.cleaned$systolic,
             y = nhanes.2016.cleaned$systolic2,
             method = "paired")

# CHECKING ASSUMPTIONS: NORMALITY
#################################

# graph systolic bp (Figure 6.11)
nhanes.2016.cleaned %>%
  ggplot(aes(x = systolic)) +
  geom_histogram(fill = "#7463AC", col = "white") +
  theme_minimal() +
  labs(x = "Systolic blood pressure (mmHg)",
       y = "NHANES participants")

#graph systolic bp (Figure 6.12)
nhanes.2016.cleaned %>%
  drop_na(systolic) %>%
  ggplot(aes(sample = systolic)) +
  stat_qq(aes(color = "NHANES participant"), alpha = .6) +
  geom_abline(aes(intercept = mean(x = systolic),
                  slope = sd(x = systolic), linetype = "Normally distributed"),
              color = "gray", size = 1) +
  theme_minimal() +
  labs(x = "Theoretical normal distribution",
       y = "Observed systolic blood pressure (mmHg)")+
  scale_color_manual(values = "#7463AC", name = "") +
  scale_linetype_manual(values = 1, name = "")

# skewness of systolic bp
# semTools::skew(object = nhanes.2016.cleaned$systolic)

# graph systolic bp by sex (Figure 6.13)
nhanes.2016.cleaned %>%
  ggplot(aes(x = systolic)) +
  geom_histogram(fill = "#7463AC", col = "grey") +
  facet_grid(cols = vars(sex)) +
  theme_minimal() +
  labs(x="Systolic blood pressure (mmHg)",
       y="NHANES participants")

#graph systolic bp (Figure 6.14)
nhanes.2016.cleaned %>%
  drop_na(systolic) %>%
  ggplot(aes(sample = systolic)) +
  stat_qq(aes(color = "NHANES participant"), alpha = .6) +
  facet_grid(cols = vars(sex)) +
  geom_abline(aes(intercept = mean(x = systolic),
                  slope = sd(x = systolic), linetype = "Normally distributed"),
              color = "gray", size = 1) +
  theme_minimal() +
  labs(x = "Theoretical normal distribution",
       y = "Observed systolic blood pressure (mmHg)")+
  scale_color_manual(values = "#7463AC", name = "") +
  scale_linetype_manual(values = 1, name = "")

# statistical test of normality for systolic bp by sex
# nhanes.2016.cleaned %>%
#   drop_na(systolic) %>%
#   group_by(sex) %>%
#   summarize(z.skew = semTools::skew(object = systolic)[3])

# graph systolic difference between systolic and systolic2 (Figure 6.15)
nhanes.2016.cleaned %>%
  ggplot(aes(x = diff.syst)) +
  geom_histogram(fill = "#7463AC", col = "white") +
  theme_minimal() +
  labs(x = "Difference between measures of systolic blood pressure (mmHg)",
       y = "NHANES participants")

# Q-Q plot difference between systolic and systolic2 (Figure 6.16)
nhanes.2016.cleaned %>%
  drop_na(diff.syst) %>%
  ggplot(aes(sample = diff.syst)) +
  stat_qq(aes(color = "NHANES participant"), alpha = .6) +
  geom_abline(aes(intercept = mean(x = diff.syst),
                  slope = sd(x = diff.syst), linetype = "Normally distributed"),
              color = "gray", size = 1) +
  theme_minimal() +
  labs(x = "Theoretical normal distribution",
       y = "Observed differences between SBP measures")+
  scale_color_manual(values = "#7463AC", name = "") +
  scale_linetype_manual(values = 1, name = "")

# equal variances for systolic by sex
car::leveneTest(y = systolic ~ sex, data = nhanes.2016.cleaned)

# ALTERNATIVES TO t-TEST WHEN ASSUMPTIONS NOT SATISFIED
#######################################################

library(BSDA)

# examine median for systolic variable
median(x = nhanes.2016.cleaned$systolic, na.rm = TRUE)

# Sign test with medians
# compare observed median SBP to 120
BSDA::SIGN.test(x = nhanes.2016.cleaned$systolic, md = 120)

# Wilcoxon signed  rank test 
# test the distribution of SBP by time period
wilcox.test(x = nhanes.2016.cleaned$systolic,
            y = nhanes.2016.cleaned$systolic2,
            paired = TRUE)

# Wilcoxon rank sum test
# test the distribution of systolic by sex
u.syst.by.sex <- wilcox.test(formula = nhanes.2016.cleaned$systolic ~ nhanes.2016.cleaned$sex, 
                             paired = FALSE)
u.syst.by.sex

# use qnorm to find z from p-value
qnorm(p = u.syst.by.sex$p.value)

# new data frame with no NA
nhanes.2016.cleaned.noNA <- nhanes.2016.cleaned %>%
  drop_na(systolic)

# use new data frame to get r
rcompanion::wilcoxonR(x = nhanes.2016.cleaned.noNA$systolic,
                      g = nhanes.2016.cleaned.noNA$sex)

# get vectors for male and female systolic
males.systolic <- nhanes.2016.cleaned %>%
  filter(sex == "Male") %>%
  pull(var = systolic)

females.systolic <- nhanes.2016.cleaned %>%
  filter(sex == "Female") %>%
  pull(var = systolic)

# conduct the Kolmogorov-Smirnov test
ks.test(x = males.systolic,
        y = females.systolic)

# ECDF for male and female SBP (Figure 6.19)
nhanes.2016.cleaned %>%
  ggplot(aes(x = systolic, color = sex)) +
  stat_ecdf(size = 1) +
  theme_minimal() +
  labs(x = "Systolic blood pressure (mmHg)",
       y = "Cumulative probability") +
  scale_color_manual(values = c("Male" = "gray", "Female" = "#7463AC"),
                     name = "Sex")
