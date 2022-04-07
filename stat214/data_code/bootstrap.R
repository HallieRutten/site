# the bootstrap (developed by Bradley Efron in the 70s)

library(tidyverse)

# general method:
# (1) create a bootstrap sample: sample the available data with replacement
#     to create a dataset of the same size
# (2) compute whatever sample statistic you care about for your bootstrap sample 
#     (e.g., sample mean, linear regression coefficient, etc.)
# (3) repeat many many many many times
# (4) visualize/analyze the resulting approximation of the sampling distribution

concrete <- read_csv("concrete.csv")

concrete <- concrete %>%
  filter( Age == 28 ) %>% 
  select( Cement, Water, Strength) %>%
  mutate( Ratio = Cement / Water )

ggplot( concrete, aes(x=Ratio, y=Strength)) +
  geom_point() +
  geom_smooth( method="lm", level=0)

# now there is a linear relationship!!
# safe to proceed with regression!!
cor( concrete$Ratio, concrete$Strength)
concrete.fit <- lm( Strength ~ Ratio, data=concrete)
summary( concrete.fit) 

# set number of bootstrap samples
n_boot <- 2000
coefficients <- data.frame( Intercept = c(), Slope = c() ) 
for( i in 1:n_boot ){
  idx <- sample( 1:nrow(concrete), nrow(concrete), replace = TRUE )
  boot <- concrete[ idx, ]
  fit <- lm( Strength ~ Ratio, data=boot) 
  coefficients <- rbind( coefficients, 
                         data.frame( Intercept = fit$coefficients[[1]],
                                     Slope = fit$coefficients[[2]]))
}

ggplot( coefficients %>% pivot_longer( everything() ), aes(value)) +
  geom_histogram() +
  facet_wrap( ~name, scales = "free" )

quantile( coefficients$Slope, probs = c(0.025,.975))
