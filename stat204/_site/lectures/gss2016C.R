# script to clean up and plot the GSS marijuana legalization data

library(tidyverse)

gss.2016 <- read.csv("legal_weed_age_GSS2016_ch1.csv")

gss.2016$grass <- as.factor( gss.2016$grass )
gss.2016$age[ gss.2016$age == "89 OR OLDER" ] <- "89"
gss.2016$age <- as.integer( gss.2016$age )

gss.2016.cleaned <- gss.2016 %>%
  mutate( grass = as.factor(x = grass)) %>%
  mutate( grass = na_if(x = grass, y="DK")) %>%
  mutate( grass = na_if(x = grass, y="IAP")) %>%
  mutate( grass = droplevels(grass)) %>% 
  mutate( age.cat = cut(x = age, 
                        breaks = c(-Inf, 29, 59, 74, Inf ),
                        labels = c( "< 30", "30-59", "60-74", "75+")
                        ))

gss.2016.cleaned %>%
  drop_na(grass) %>%
  drop_na(age) %>%
  mutate(grass = recode_factor(.x = grass,
                               `LEGAL` = "Yes",
                               `NOT LEGAL` = "No")) %>%
  ggplot(aes(x = age.cat,
             y = 100*(..count..)/sum(..count..),
             fill = grass)) +
  geom_bar(position = 'dodge') +
  theme_minimal() +
  scale_fill_manual(values = c("#78A678", '#7463AC'),
                    name = "Should marijuana\nbe legal?") +
  labs(x = "Age category",
       y = "Percent of total responses")

