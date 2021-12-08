library(tidyverse)

dist.ssp <- read_csv("dist_ssp_amfar_ch9.csv")

# check sample
summary(object = dist.ssp)

dist.ssp$county <- as.character(dist.ssp$county)
dist.ssp$STATEABBREVIATION <- as.character(dist.ssp$STATEABBREVIATION)
dist.ssp$metro <- as.character(dist.ssp$metro)
dist.ssp$opioid_RxRate <- as.numeric(dist.ssp$opioid_RxRate)

# recoding -1 to NA for HIVprevalence
dist.ssp <- dist.ssp %>%
  mutate(HIVprevalence = na_if(x = HIVprevalence, y = -1))

# check recoding
summary(object = dist.ssp)

# check distribution of HIV prevalence (Figure 9.1)
dist.ssp %>%
  ggplot(aes(x = HIVprevalence)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  labs(x = "HIV cases per 100,000 people", y = "Number of counties") +
  theme_minimal()

# descriptive statistics for syringe data
syringe.desc <- tableone::CreateTableOne(data = dist.ssp,
                                         vars = c('dist_SSP',
                                                  'HIVprevalence',
                                                  'opioid_RxRate',
                                                  'pctunins',
                                                  'metro'))
print(x = syringe.desc, nonnormal = c("HIVprevalence"))

# percent without health insurance and distance to needle exchange (Figure 9.2)
dist.ssp %>%
  ggplot(aes(x = pctunins, y = dist_SSP)) +
  geom_point(aes(size = "County"), color = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(x = "Percent without health insurance", y = "Miles to syringe program") +
  scale_size_manual(values = 2, name = "")

# percent without health insurance and distance to needle exchange (Figure 9.3)
dist.ssp %>%
  ggplot(aes(x = pctunins, y = dist_SSP)) +
  geom_point(aes(size = "County"), color = "#7463AC", alpha = .6) +
  geom_smooth(aes(linetype = "Linear fit line"), method = "lm", se = FALSE,
              color = "gray60") +
  theme_minimal() +
  labs(x = "Percent uninsured", y = "Miles to syringe program") +
  scale_size_manual(values = 2, name = "") +
  scale_linetype_manual(values = 1, name = "")

#Box 9.2
# percent without health insurance and distance to needle exchange
# (Figure 9.4)
dist.ssp %>%
  ggplot(aes(x = pctunins, y = dist_SSP)) +
  geom_point(aes(shape = "County"), color = "#7463AC", alpha = .6) +
  geom_smooth(aes(linetype = "Linear fit line"), method = "lm", se =
                FALSE, color = "gray60") +
  theme_minimal() +
  labs(x = "Percent uninsured", y = "Miles to syringe program") +
  scale_shape_manual(values = 0, name = "") +
  scale_linetype_manual(values = 1, name = "")

# percent without health insurance and distance to needle exchange
# (Figure 9.5)
dist.ssp %>%
  ggplot(aes(x = pctunins, y = dist_SSP)) +
  geom_point(aes(shape = "County"), color = "#7463AC", alpha = .6) +
  geom_smooth(aes(size = "Linear fit line"), method = "lm", se = FALSE, color = "gray60") +
  theme_minimal() +
  labs(x = "Percent uninsured", y = "Miles to syringe program") +
  scale_shape_manual(values = 17, name = "") +
  scale_size_manual(values = 1, name = "")

#9.4.4
# correlation between percent uninsured and distance
dist.ssp %>%
  summarize(cor.dist.uninsur = cor(x = dist_SSP,
                                   y = pctunins),
            samp.n = n())

#9.4.5
# bivariate relationships with distance to SSP
dist.ssp %>%
  summarize(cor.rx.rate = cor(x = dist_SSP, y = opioid_RxRate),
            cor.hiv = cor(x = dist_SSP, y = HIVprevalence, use =
                            "complete"),
            cor.unins = cor(x = dist_SSP, y = pctunins))

# metro and distance to SSP
dist.ssp %>%
  group_by(metro) %>%
  summarize(m.dist = mean(x = dist_SSP))

#9.4.6
# metro and distance to SSP (Figure 9.6)
dist.ssp %>%
  ggplot(aes(x = metro, y = dist_SSP, fill = metro)) +
  geom_jitter(aes(color = metro), alpha = .6) +
  geom_boxplot(aes(fill = metro), alpha = .4) +
  labs(x = "Type of county",
       y = "Distance to nearest syringe program") +
  scale_fill_manual(values = c("#78A678", "#7463AC"), guide = FALSE) +
  scale_color_manual(values = c("#78A678", "#7463AC"), guide = FALSE) +
  theme_minimal()

# metro and distance to SSP (Figure 9.7)
dist.ssp %>%
  ggplot(aes(x = metro, y = dist_SSP, fill = metro)) +
  geom_jitter(aes(color = metro), alpha = .8) +
  geom_violin(aes(fill = metro), alpha = .4) +
  labs(x = "Type of county",
       y = "Distance to nearest syringe program") +
  scale_fill_manual(values = c("#78A678", "#7463AC"), guide = FALSE) +
  scale_color_manual(values = c("#78A678", "#7463AC"), guide = FALSE) +
  theme_minimal()

# metro and distance to SSP (Figure 9.8)
dist.ssp %>%
  ggplot(aes(x = metro, y = dist_SSP, fill = metro)) +
  geom_violin(aes(color = metro), fill = "white", alpha = .8) +
  geom_boxplot(aes(fill = metro, color = metro), width = .2, alpha = .3) +
  geom_jitter(aes(color = metro), alpha = .4) +
  labs(x = "Type of county",
       y = "Miles to syringe program") +
  scale_fill_manual(values = c("#78A678", "#7463AC"), guide = FALSE) +
  scale_color_manual(values = c("#78A678", "#7463AC"), guide = FALSE) +
  theme_minimal() +
  coord_flip()

#9.4.7 CHECK YOUR UNDERSTANDING
#Check distributions of Opioid Rx Rate and Pct Uninsured
dist.ssp %>%
  ggplot(aes(x = opioid_RxRate, fill = "grey", color = "white"))+
  geom_histogram() +
  theme_minimal()+
  scale_fill_manual(values = "#78A678", guide = FALSE) +
  scale_color_manual(values = "white", guide = FALSE) +
  geom_vline(xintercept = median(dist.ssp$opioid_RxRate, na.rm =T), linetype = 1)+
  annotate("text", x = median(dist.ssp$opioid_RxRate, na.rm = T)+17, y = 75, label = "Median")

dist.ssp %>%
  ggplot(aes(x = pctunins, fill = "grey", color = "white"))+
  geom_histogram() +
  theme_minimal()+
  scale_fill_manual(values = "#7463AC", guide = FALSE) +
  scale_color_manual(values = "white", guide = FALSE) +
  geom_vline(xintercept = median(dist.ssp$pctunins, na.rm =T), linetype = 1)+
  annotate("text", x = median(dist.ssp$pctunins, na.rm = T)+.05, y = 50, label = "Median")

#Neither of the variables is normally distributed. Leslie could report medians/IQRs and use
#non-parametric tests, or could try to transform the variables to approximate normal distributions.

#Part 2

dist.ssp %>%
  ggplot(aes(x = metro, y = dist_SSP, fill = metro)) +
  geom_jitter(aes(color = metro), alpha = .4) +
  geom_boxplot(aes(fill = metro, color = metro), width = .2, alpha = .3) +
  geom_violin(aes(color = metro), fill = "white", alpha = .8) +
  labs(x = "Type of county",
       y = "Miles to syringe program") +
  scale_fill_manual(values = c("#78A678", "#7463AC"), guide = FALSE) +
  scale_color_manual(values = c("#78A678", "#7463AC"), guide = FALSE) +
  theme_minimal() +
  coord_flip()

#9.6
# linear regression of distance to syringe program by percent uninsured
dist.by.unins <- lm(formula = dist_SSP ~ pctunins,
                    data = dist.ssp, na.action = na.exclude)
summary(object = dist.by.unins)

#9.6.7 ACHIEVEMENT 3: CHECK YOUR UNDERSTANDING
#Washington County, NE: pctunins = 13
(distance <- 12.48 + 7.82 * 13)

#9.7.4
# use predict to find predicted value of distance for 10% uninsured
pred.dist.ssp <- predict(object = dist.by.unins,
                         newdata = data.frame(pctunins = 10),
                         interval = "confidence")
pred.dist.ssp

# use predict to find predicted value for all observed x
pred.dist.ssp.all <- predict(object = dist.by.unins,
                             interval = "confidence")

# print out the first six predicted values and CI
head(x = pred.dist.ssp.all)

#9.7.5 ACHIEVEMENT 4: CHECK YOUR UNDERSTANDING
#first, get predicted distances for county with 2% uninsured...
pred.dist.ssp.2perc <- predict(object = dist.by.unins,
                         newdata = data.frame(pctunins = 2),
                         interval = "confidence")

#.. and a county with 12% uninsured
pred.dist.ssp.12perc <- predict(object = dist.by.unins,
                               newdata = data.frame(pctunins = 12),
                               interval = "confidence")

#The difference between the two can be derived reproducibly by substracting:
(diff.2.to.12.perc <- pred.dist.ssp.2perc[1,1] - pred.dist.ssp.12perc[1,1])

#We can also add a confidence interval around this:
(diff.2.to.12.perc.lower <- pred.dist.ssp.2perc[1,2] - pred.dist.ssp.12perc[1,2])
(diff.2.to.12.perc.upper <- pred.dist.ssp.2perc[1,3] - pred.dist.ssp.12perc[1,3])

#9.8.4 ACHIEVEMENT 5: CHECK YOUR UNDERSTANDING
dist.by.rx <- lm(formula = dist_SSP ~ opioid_RxRate,
                 data = dist.ssp, na.action = na.exclude)
summary(dist.by.rx)

