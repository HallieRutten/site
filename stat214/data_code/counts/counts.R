library(tidyverse)
library(gridExtra)

# Crash data -----

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

# crashes in Sevier county are not Poisson distributed; 
# do they follow a negative binomial distribution?

(sevier.mu <- mean( sevier$Crashes ))
(sevier.var <- var( sevier$Crashes ))
(sevier.size <- (sevier.mu^2)/(sevier.var - sevier.mu) )

# simulate crashes from Sevier county using NB
sevier.simulation <- rnbinom( n=nrow(sevier), size=sevier.size, mu=sevier.mu)
sevier <- sevier %>% 
  mutate( Simulation = sevier.simulation )

observed.crashes <- ggplot( sevier, aes(Crashes)) + 
  geom_histogram( binwidth = 1) +
  xlim(0, max(sevier$Crashes)) + 
  ylim(0,6)

simulated.crashes <- ggplot( sevier, aes(Simulation)) + 
  geom_histogram( binwidth = 1) +
  xlim(0,max(sevier$Crashes)) + 
  ylim(0,6)

grid.arrange( observed.crashes, simulated.crashes, ncol=1 )

# Births data -----

births <- read_csv("births.csv")

births_by_county <- births %>%
  group_by(county) %>% 
  summarize( Mean = mean(births),
             Variance = var(births),
             Ratio = Variance/Mean ) %>% 
  arrange( desc(Ratio))

blount <- births %>% filter(county == "Blount County, TN")

observed.crashes <- ggplot( blount, aes(births)) + 
  geom_histogram( binwidth = 1) +
  xlim(0, max(blount$births))

(blount.lambda <- mean( blount$births ))
var( blount$births )

# simulate crashes from Meigs county
blount.simulation <- rpois( nrow(blount), blount.lambda)
blount <- blount %>% 
  mutate( Simulation = blount.simulation )

simulated.crashes <- ggplot( blount, aes(Simulation)) + 
  geom_histogram( binwidth = 1)  +
  xlim(0, max(blount$births))

grid.arrange( observed.crashes, simulated.crashes, ncol=1 )

kanawha <- births %>% filter(county == "Kanawha County, WV")
(kanawha.mu <- mean(kanawha$births))
(kanawha.var <- var(kanawha$births))
(kanawha.size <- (kanawha.mu^2)/(kanawha.var - kanawha.mu))

# births in Kanawha County are not Poisson distributed...

observed.crashes <- ggplot( kanawha, aes(births)) + 
  geom_histogram( binwidth = 1) +
  xlim(0, max(kanawha$births))

# simulate crashes from Meigs county
kanawha.simulation <- rpois( nrow(kanawha), kanawha.mu)
kanawha <- kanawha %>% 
  mutate( Simulation = kanawha.simulation )

simulated.crashes <- ggplot( kanawha, aes(Simulation)) + 
  geom_histogram( binwidth = 1)  +
  xlim(0, max(kanawha$births))

grid.arrange( observed.crashes, simulated.crashes, ncol=1 )

# but they do follow the NB distribution

# simulate crashes from Meigs county
kanawha.simulation <- rnbinom( n=nrow(kanawha), size=kanawha.size, mu=kanawha.mu)
kanawha <- kanawha %>% 
  mutate( Simulation = kanawha.simulation )

simulated.crashes <- ggplot( kanawha, aes(Simulation)) + 
  geom_histogram( binwidth = 1)  +
  xlim(0, max(kanawha$births))

grid.arrange( observed.crashes, simulated.crashes, ncol=1 )

# Cough data -----

primera <- read_csv("primera.csv")

ggplot( primera, aes(x=datetime, y=coughs)) + geom_line()
ggplot( primera ) + geom_segment( aes(x=datetime, y=0, xend=datetime, yend=coughs ))
ggplot( primera, aes(x=coughs)) + geom_histogram()

(primera.mu <- mean(primera$coughs))
(primera.var <- var(primera$coughs))
(primera.size <- (primera.mu^2)/(primera.var - primera.mu))

# coughs are not Poisson distributed... 

observed.coughs <- ggplot( primera ) + 
  geom_segment( aes(x=datetime, y=0, xend=datetime, yend=coughs )) 

primera.simulation <- rpois( nrow(primera), primera.mu )
primera <- primera %>%
  mutate( Simulation = primera.simulation)

simulated.coughs <- ggplot( primera ) + 
  geom_segment( aes(x=datetime, y=0, xend=datetime, yend=Simulation )) 

grid.arrange( observed.coughs, simulated.coughs, ncol=1 )

# but they do follow NB! 

observed.coughs <- ggplot( primera ) + 
  geom_segment( aes(x=datetime, y=0, xend=datetime, yend=coughs )) 

primera.simulation <- rnbinom( n=nrow(primera), size=primera.size, mu=primera.mu )
primera <- primera %>%
  mutate( Simulation = primera.simulation)

simulated.coughs <- ggplot( primera ) + 
  geom_segment( aes(x=datetime, y=0, xend=datetime, yend=Simulation )) 

grid.arrange( observed.coughs, simulated.coughs, ncol=1 )

# How to relate counts to a predictor? -----

# use crashes, not births!
