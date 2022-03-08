library(tidyverse)
library(gridExtra)

crashes <- read_csv("TDOT.csv")
crashes <- crashes %>%
  pivot_longer(cols=January:December, names_to="Month", values_to="Crashes")

county_pops <- read_csv("tn_county_pops.csv")
county_pops <- county_pops %>%
  filter( CTYNAME != "Tennessee") 
colnames( county_pops ) <- c("County", "Pop2010")
county_pops <- county_pops %>%
  separate( col="County", into=c("County","Useless"), sep=" ") %>%
  select(-Useless)

crashes <- inner_join(crashes, county_pops, by="County")

# Meigs County and the Poisson distribution
meigs <- crashes %>% 
  filter( County == "Meigs")

observed.crashes <- ggplot( meigs, aes(Crashes)) + 
  geom_histogram( binwidth = 1) + 
  xlim(0,36)

(meigs.lambda <- mean( meigs$Crashes ))
var( meigs$Crashes )

# simulate crashes from Meigs county
meigs.simulation <- rpois( nrow(meigs), meigs.lambda)
meigs <- meigs %>% 
  mutate( Simulation = meigs.simulation )

simulated.crashes <- ggplot( meigs, aes(Simulation)) + 
  geom_histogram( binwidth = 1) +
  xlim(0,36)

grid.arrange( observed.crashes, simulated.crashes, ncol=1 )

# Other counties

crash_stats <- crashes %>%
  group_by(County) %>% 
  summarize( Mean = mean(Crashes),
             Var = var(Crashes), 
             Ratio = Mean/Var ) %>% 
  arrange( desc(Ratio) )

# Sevier county crashes are NOT Poisson distributed!
sevier <- crashes %>% 
  filter( County == "Sevier" )

observed.crashes <- ggplot( sevier, aes(Crashes)) + 
  geom_histogram( binwidth = 1) +
  xlim(0, max(sevier$Crashes))

(sevier.lambda <- mean( sevier$Crashes ))
var( sevier$Crashes )

# simulate crashes from Sevier county
sevier.simulation <- rpois( nrow(sevier), sevier.lambda)
sevier <- sevier %>% 
  mutate( Simulation = sevier.simulation )

simulated.crashes <- ggplot( sevier, aes(Simulation)) + 
  geom_histogram( binwidth = 1) +
  xlim(0,max(sevier$Crashes))

grid.arrange( observed.crashes, simulated.crashes, ncol=1 )
