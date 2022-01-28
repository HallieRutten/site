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
          Null.residual = IMR - y.bar )

ggplot( africa, aes(Null.residual)) + geom_histogram()

( africa.tss <- sum( africa$Null.residual^2 ) )

# try fitting the SD line to the data 
x.bar <- mean( africa$Literacy)
y.bar <- mean(africa$IMR)
sd.x <- sd( africa$Literacy)
sd.y <- sd( africa$IMR )
b <- -1*(sd.y / sd.x) 
a <- y.bar - b*x.bar 
ggplot( africa, aes(x=Literacy, y=IMR)) +
  geom_point() +
  geom_abline( intercept=a, slope = b, color="tomato" )

africa <- africa %>% 
  mutate( Model = a + b*Literacy, 
          Residual = IMR - Model )

sum( africa$Residual^2 )

# optimal coefficients from formulae -----
b <- africa.r*( sd.y / sd.x )
a <- y.bar - b*x.bar 

ggplot( africa, aes(x=Literacy, y=IMR)) +
  geom_point() +
  geom_abline( intercept=a, slope = b, color="blue" )

# update the residual column 
africa <- africa %>% 
  mutate( Model = a + b*Literacy, 
          Residual = IMR - Model )

# compute residual sum of squares (RSS)
africa.rss <- sum( africa$Residual^2 )
n.countries <- nrow( africa )
# compute residual standard error (RSE)
africa.rse <- sqrt( africa.rss / (n.countries - 2))

africa.fit <- lm( IMR ~ Literacy, data=africa)
africa.fit
summary( africa.fit )
sigma( africa.fit ) 
africa.fit$coefficients
africa.fit$fitted.values
africa.fit$residuals
plot( africa.fit$residuals )

1 - (africa.rss / africa.tss )
