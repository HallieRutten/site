library( tidyverse )

imr <- read_csv("imr2011.csv", show_col_types = FALSE)
c_section <- read_csv("c_section.csv", show_col_types = FALSE)
early_breastfeeding <- read_csv("early_breastfeeding.csv", show_col_types = FALSE)

c_section <- c_section %>%
  rename( "Country" = `Country or Area`,
          "C_Section" = Value ) %>%
  select( Country, C_Section)

imr <- imr %>%
  left_join( c_section, by="Country")
  
early_breastfeeding <- early_breastfeeding %>%
  rename( "Country" = `Country or Area`,
          "Breastfeeding" = Value ) %>%
  select( Country, Breastfeeding)

imr <- imr %>%
  left_join( early_breastfeeding, by="Country")

skilled_birth <- read_csv("skilled_birth.csv", show_col_types = FALSE)
skilled_birth <- skilled_birth %>%
  filter( Subgroup == "Total") %>%
  rename( "Country" = `Country or Area`,
          "Skilled_Birth" = Value ) %>%
  select(Country, Skilled_Birth)

imr <- imr %>%
  left_join( skilled_birth, by="Country")

fertility <- read_csv("fertility.csv", show_col_types = FALSE)
fertility <- fertility %>%
  filter( Year == 2011) %>%
  rename( "Country" = `Country or Area`,
          "Fertility" = Value ) %>%
  select(Country, Fertility)

imr <- imr %>%
  left_join( fertility, by="Country")

urbanisation <- read_csv("urbanisation.csv", show_col_types = FALSE)
urbanisation <- urbanisation %>%
  rename( "Country" = `Country or Area`,
          "Urbanisation" = Value ) %>%
  select(Country, Urbanisation)

imr <- imr %>%
  left_join( urbanisation, by="Country")

literacy <- read_csv("adult_literacy.csv", show_col_types = FALSE)
literacy <- literacy %>%
  rename( "Country" = `Country or Area`,
          "Literacy" = Value ) %>%
  select(Country, Literacy)

imr <- imr %>%
  left_join( literacy, by="Country")

# see all pairs of scatterplots 
pairs( imr %>% select( IMR:Literacy) )

# it looks like C_section is useful, Breastfeeding is not...

# check simple models ...
imr_fit <- lm( IMR ~ C_Section, data = imr )
summary( imr_fit )

imr_fit <- lm( IMR ~ Breastfeeding, data = imr )
summary( imr_fit )

# check multiple regression model...
imr_fit <- lm( IMR ~ C_Section + Breastfeeding, data = imr )
summary( imr_fit )

# but we know we shouldn't inlude Breastfeeding here!

cor( imr$IMR, imr$C_Section, use="complete.obs")
cor( imr$IMR, imr$Breastfeeding, use="complete.obs")

imr_fit <- lm( IMR ~ C_Section + Continent, data =imr )
summary( imr_fit )

ggplot( imr, aes(x=C_Section, y=IMR, color=Continent)) + 
  geom_point() + 
  geom_smooth( method="lm") 

# ecological fallacy : relationships appear stronger when groups are combined, 
# weaker when groups are disaggregated

continent_means <- imr %>%
  drop_na() %>% 
  group_by(Continent) %>% 
  summarize( IMR = mean(IMR),
             C_Section = mean(C_Section))

ggplot( continent_means, aes(C_Section, IMR)) + 
  geom_point() + 
  geom_smooth( method="lm")
