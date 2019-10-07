install.packages("pacman")
pacman::p_load(ggparliament, tidyverse, ggplot2)

# set wd
setwd("C:/Projects/Personal/kosovo-elections-2019/src/")

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

df <- read.csv("../data/interim/municipalities_2019.csv",stringsAsFactors=FALSE, encoding="UTF-8")

ggplot(data = df, aes(x = Komuna, y = Përqindja.e.votave.të.vlefshme, fill = Subjekti.Politik)) + 
  geom_bar(stat = "identity") +
  coord_flip()

ggplot(df, aes(x = Komuna, y = Përqindja.e.votave.të.vlefshme))+
  geom_col(aes(fill = Subjekti.Politik), width = 0.7) +
  coord_flip() +
  theme(legend.position = "none")

### Top 7 Municipalities

df <- df %>% filter(
  Komuna == "Prishtinë" | 
  Komuna == "Prizren" | 
  Komuna == "Pejë" | 
  Komuna == "Gjilan" | 
  Komuna == "Mitrovicë e Jugut" | 
  Komuna == "Ferizaj" |
  Komuna == "Gjakovë") %>%
  filter(Përqindja.e.votave.të.vlefshme >= 5.0)

# add the remaining percent
for(i in unique(df$Komuna)){
  df <- df %>% 
    rbind(list("Tjera",0,100-sum(df$Përqindja.e.votave.të.vlefshme[which(df$Komuna==i)]),i))
}

df <- arrange(df, Përqindja.e.votave.të.vlefshme)

ggplot(df, aes(Komuna, Përqindja.e.votave.të.vlefshme, fill = Subjekti.Politik, 
               order = -as.numeric(Përqindja.e.votave.të.vlefshme)))+
  geom_bar(stat="identity") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  labs(y="Përqindja e votave të vlefshme", 
       title = "Përqindjet e subjekteve politike në 7 komunat kryesore",
       fill="Subjekti Politik")

### Parties that got more than 

df <- read.csv("../data/raw/overall_results.csv",stringsAsFactors=FALSE, encoding="UTF-8")


df <- df %>% filter(year == 2019) %>% arrange(df$votes)
df$party_short <- factor(df$party_short, levels = df$party_short)
df$party_long <- factor(df$party_long, levels = df$party_long)

ggplot(data=df, aes(x=party_short, y=votes, fill=party_short)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_manual(values=df$color)


ggplot(data=df.2019, aes(x=party_short, y=votes)) +
  geom_bar(stat="identity") +
  coord_flip()


















