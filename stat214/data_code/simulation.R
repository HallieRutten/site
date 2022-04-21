library( tidyverse )

# Simulated linear regression -----

x <- rnorm( n = 30, mean=100, sd=15 )

# henceforth, assume that the predictor values (x) are constant, exact, etc.

alpha <- 9
beta <- 2.7
# normally distributed error
# epsilon <- rnorm( n=30, mean=0, sd = 45 )

# non-normal error
epsilon <- rexp( n=30, rate = 0.2 )

# response depends linearly on x :
y <- alpha + beta*x + epsilon

plot( x, y )
cor( x, y )

fit <- lm( y ~ x )
abline( fit )
summary( fit )
plot( fit )

# Simulated logistic regression -----

logfam <- binomial()

x <- seq(-6,6,by=.01)
y <- logfam$linkinv( x )
plot( x, y )

# Simulated count regression -----
