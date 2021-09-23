# Chapter 2
# code for analyzing transgender health data

library(tidyverse)

trans.2014 <- read.csv("transgender_hc_ch2.csv")

table( trans.2014$TRNSGNDR )

trans.2014.cleaned <- trans.2014 %>%
  mutate( TRNSGNDR = recode_factor(.x = TRNSGNDR,
                                   "1" = "Male to female",
                                   "2" = "Female to male",
                                   "3" = "Gender non-conforming",
                                   "4" = "No",
                                   "7" = "Don't know",
                                   "9" = "Refused" )) %>%
  mutate( PHYSHLTH = na_if( x=PHYSHLTH, y=77 )) %>%
  mutate( PHYSHLTH = na_if( x=PHYSHLTH, y=99 )) %>%
  mutate( PHYSHLTH = as.numeric( recode(.x=PHYSHLTH, `88`=0L )))

table( trans.2014.cleaned$TRNSGNDR )

descr::freq( trans.2014.cleaned$TRNSGNDR, plot=FALSE )

trans.2014.cleaned %>% 
  group_by( TRNSGNDR ) %>% 
  summarize( Frequency = n() ) %>% 
  mutate( Percent = Frequency / sum(Frequency) ) %>% 
  mutate( Valid.percent = Frequency / sum(Frequency[ na.omit(object=TRNSGNDR)] ))

descr::freq( trans.2014.cleaned$HADMAM, plot=FALSE )

trans.2014.cleaned %>% 
  ggplot( aes(PHYSHLTH)) +
  geom_histogram()

table( trans.2014.cleaned$PHYSHLTH )

mean( trans.2014.cleaned$PHYSHLTH, na.rm=TRUE )
median( trans.2014.cleaned$PHYSHLTH, na.rm=TRUE )
names( sort( table( trans.2014.cleaned$PHYSHLTH ), decreasing=TRUE) )[1]

quantile( trans.2014.cleaned$PHYSHLTH, probs= seq(0,1,.10), na.rm=TRUE )
quantile( trans.2014.cleaned$PHYSHLTH, na.rm=TRUE )

trans.2014.cleaned %>%
  drop_na( PHYSHLTH ) %>% 
  summarize( Average = mean( PHYSHLTH ),
             SD = sd( PHYSHLTH ),
             Median = median( PHYSHLTH ),
             IQR = IQR( PHYSHLTH ),
             Mode = names( sort( table( trans.2014.cleaned$PHYSHLTH ), decreasing=TRUE) )[1] )

trans.2014.small <- trans.2014.cleaned %>%
  filter( TRNSGNDR == "Male to female" | 
          TRNSGNDR == "Female to male" |        
          TRNSGNDR == "Gender non-conforming" ) %>%
  filter( X_AGEG5YR > 4 & X_AGEG5YR < 12 ) %>%
  filter( !is.na( HADMAM ))  %>%
  select( TRNSGNDR, X_AGEG5YR, X_RACE, X_INCOMG, X_EDUCAG, HLTHPLN1, HADMAM ) %>%
  mutate_all( as.factor ) %>%
  mutate(X_AGEG5YR = recode_factor(.x = X_AGEG5YR,
                                   `5` = '40-44',
                                   `6` = '45-49',
                                   `7` = '50-54',
                                   `8` = '55-59',
                                   `9` = '60-64',
                                   `10` = '65-69',
                                   `11` = '70-74')) %>%
  mutate(X_INCOMG = recode_factor(.x = X_INCOMG,
                                  `1` = 'Less than $15,000',
                                  `2` = '$15,000 to less than $25,000',
                                  `3` = '$25,000 to less than $35,000',
                                  `4` = '$35,000 to less than $50,000',
                                  `5` = '$50,000 or more',
                                  `9` = 'Don\'t know/not sure/missing')) %>%
  mutate(X_EDUCAG = recode_factor(.x = X_EDUCAG,
                                  `1` = 'Did not graduate high school',
                                  `2` = 'Graduated high school',
                                  `3` = 'Attended college/technical school',
                                  `4` = 'Graduated from college/technical school',
                                  `9` = NA_character_)) %>%
  mutate(HLTHPLN1 = recode_factor(.x = HLTHPLN1,
                                  `1` = 'Yes',
                                  `2` = 'No',
                                  `7` = 'Don\'t know/not sure/missing',
                                  `9` = 'Refused')) %>%
  mutate(X_RACE = recode_factor(.x = X_RACE,
                                `1` = 'White',
                                `2` = 'Black',
                                `3` = 'Native American',
                                `4` = 'Asian/Pacific Islander',
                                `5` = 'Other',
                                `6` = 'Other',
                                `7` = 'Other',
                                `8` = 'Other',
                                `9` = 'Other')) %>%
  mutate(HADMAM = recode_factor(.x = HADMAM,
                                `1` = 'Yes',
                                `2` = 'No',
                                `7` = 'Don\'t know/not sure/missing',
                                `9` = 'Refused'))


summary(trans.2014.small)

