# Chapter 2
# code for analyzing transgender health and breast cander screening 

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
                                   "9" = "Refused"
                                   )) %>%
  mutate( PHYSHLTH = na_if( x=PHYSHLTH, y=77 )) %>%
  mutate( PHYSHLTH = na_if( x=PHYSHLTH, y=99 )) %>%
  mutate( PHYSHLTH = as.numeric( recode(.x=PHYSHLTH, `88`=0L )))
  
table( trans.2014.cleaned$TRNSGNDR )

descr::freq( trans.2014.cleaned$TRNSGNDR, plot=FALSE )

trans.2014.cleaned %>% 
  group_by(TRNSGNDR) %>% 
  summarize( Frequency = n() )%>%
  mutate( Percent = 100*Frequency / sum(Frequency) ) %>% 
  mutate( Valid.percent = 100*Frequency / sum(Frequency[na.omit(object=TRNSGNDR)]) )

trans.2014.cleaned %>% 
  ggplot( aes(x=PHYSHLTH)) +
  geom_histogram( )

mean( trans.2014.cleaned$PHYSHLTH, na.rm=TRUE )
median( trans.2014.cleaned$PHYSHLTH, na.rm=TRUE )

names( sort( table( trans.2014.cleaned$PHYSHLTH ), decreasing = TRUE ))[1]

quantile( trans.2014.cleaned$PHYSHLTH, probs=seq(0,1,.10), na.rm=TRUE)
quantile( trans.2014.cleaned$PHYSHLTH, na.rm=TRUE)
IQR( trans.2014.cleaned$PHYSHLTH, na.rm=TRUE )

trans.2014.cleaned %>% 
  drop_na(PHYSHLTH) %>%
  summarize( Average = mean(PHYSHLTH), 
             SD = sd(PHYSHLTH),
             Median = median(PHYSHLTH), 
             IQR = IQR(PHYSHLTH),
             Mode = names( sort( table( trans.2014.cleaned$PHYSHLTH ), decreasing = TRUE ))[1]) 
