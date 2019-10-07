install.packages("pacman")
pacman::p_load(ggparliament, dplyr, ggplot2)

# set wd
setwd("C:/Projects/Personal/kosovo-elections/src/")

## Parliament graph

kosovo_parliament_data <- read.csv2("../data/interim/parlamenti_2019.csv",stringsAsFactors=FALSE)

ksp_2019_data <- kosovo_parliament_data %>%
  filter(ï..year == 2019)

ks_parliament_2019 <- parliament_data(election_data = ksp_2019_data,
                                      type = "semicircle",
                                      parl_rows = 5,
                                      party_seats = ksp_2019_data$seats)

deputies_2017 <- ggplot(ks_parliament_2019, aes(x, y, colour = party_short)) +
  geom_parliament_seats(size=5) + 
  #highlight the party in control of the House with a black line
  geom_highlight_government(government == 1, size=5) +
  #draw majority threshold
  draw_majoritythreshold(n = 61, label = TRUE, type = 'semicircle')+
  #set theme_ggparliament
  theme_ggparliament() +
  #other aesthetics
  labs(colour = "Partia ose Koalicionet", 
       title = "Parlamenti i Republikes së Kosovës në vitin 2019",
       subtitle = "Partia që kontrollon parlamentin është e theksuar") +
  scale_colour_manual(values = ks_parliament_2019$colour, limits = ks_parliament_2019$party_short)

deputies_2017

ggsave("../figures/kosovo-parliament-2019-shqip-v2.png", width=12, height = 7)


### Horizontal bar plot