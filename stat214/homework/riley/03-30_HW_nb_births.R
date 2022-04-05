# Riley Barrett
# 30 Mar 2022

# Setup -----
library(tidyverse)
library(gridExtra)
library(zoo)
setwd("/home/rileybarrettfg/214")

# Get births data -----
births <- read.csv("datasets/counts/births.csv")
#view(births)

# Function to simulate one county's data with the NB distribution -----
countyNBSimulation <- function(countyName) {
  cData <- births %>% filter(county == countyName)
  
  cData.mu <- mean(cData$births)
  cData.var <- var(cData$births)
  cData.size <- cData.mu ^ 2 / (cData.var - cData.mu)
  
  # Used na.lof from zoo to merge simulation and table, as per StackOverflow
  births.simulation <-
    rnbinom(n = nrow(cData),
            size = cData.size,
            mu = cData.mu)
  cData <-
    cData %>% mutate(simulation = na.locf(births.simulation))
  
  xMax <- ceiling(max(cData$births) * 1.1)
  xMin <-   floor(min(cData$births) * 0.9)
  binW <- ceiling(max(cData$births) / 100)
  
  plot.observed.births <- ggplot(cData, aes(births)) + 
    geom_histogram(binwidth = binW) + 
    xlim(xMin, xMax)
  plot.simulated.births <- ggplot(cData, aes(simulation)) +
    geom_histogram(binwidth = binW) + 
    xlim(xMin, xMax)
  
  grid.arrange(plot.observed.births, plot.simulated.births, ncol = 1)
  
  cat("Actual and simulated births for ", countyName, "\n")
  cat("Using a histogram binwidth of ", binW, "\n")
}

# Test the simulation function -----
#countyNBSimulation("Blount County, TN")
#countyNBSimulation("Houston County, AL")
#countyNBSimulation("Cook County, IL")
#countyNBSimulation("Broward County, FL")

# Pick a random county, and simulate with it
countyList <- births %>% select(county) %>% distinct()
sample_n(countyList, 1)$county %>% countyNBSimulation()

