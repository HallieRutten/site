library(tidyverse)

# get the data -----
africa <- read_csv("africa.csv")
names( africa )

# look at a scatterplot ----
( africa.plot <- ggplot( africa, aes(x=Literacy, y=IMR)) +
  geom_point() )

# null model -----
( y.bar <- mean( africa$IMR ) )
ggplot( africa, aes(IMR)) + geom_histogram( binwidth=3 )

# correlation coefficient -----

africa.plot + 
  geom_hline( yintercept = y.bar, color ="blue", linetype="dashed" ) +
  geom_vline( xintercept = mean( africa$Literacy), color="blue", linetype="dashed")

( africa.r <- cor( africa$Literacy, africa$IMR ))

# residuals for null model -----
africa.plot + 
  geom_hline( yintercept = y.bar, color ="blue" )

africa <- africa %>% 
  mutate( Null = y.bar, 
          Residual = IMR - y.bar )

sum( africa$Residual^2 )
