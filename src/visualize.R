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
  labs(colour = "Subjekti Politik", 
       title = "Parlamenti i Republikes së Kosovës në vitin 2019",
       subtitle = "Partia që kontrollon parlamentin është e theksuar", 
       caption = "Burimi: https://rezultatet2019.org/") +
  scale_colour_manual(values = ks_parliament_2019$colour, limits = ks_parliament_2019$party_short) +
  theme(
    plot.title = element_text(hjust = 0, size = 18),    # Center title position and size
    plot.subtitle = element_text(hjust = 0),            # Center subtitle
    plot.caption = element_text(hjust = 0.025, face = "italic")# move caption to the left
  )

deputies_2017

ggsave("../figures/kosovo-parliament-2019-shqip-v4.png", width=12, height = 8)


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

df <- read.csv("../data/raw/overall_results_v2.csv",stringsAsFactors=FALSE, encoding="UTF-8")

df_2019 <- df %>% filter(year == 2019)
df_2017 <- df %>% filter(year == 2017)

df_2019$diff <- round(df_2019$percentage - df_2017$percentage,1)
df_2019 <- df_2019 %>% arrange(percentage)
df_2019$party_short <- factor(df_2019$party_short, levels = df_2019$party_short)
df_2019$party_long <- factor(df_2019$party_long, levels = df_2019$party_long)

for (i in 1:length(df_2019$diff)) {
  if (df_2019$diff[i] > 0) {
    df_2019$diff[i] <- paste("+",df_2019$diff[i],sep="")
  }
}

ggplot(data=df_2019, aes(x=party_short, y=percentage, fill=party_short)) +
  geom_bar(stat="identity", width = 0.55) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_manual(values=df_2019$color) +
  labs(fill = "Subjekti Politik", x = "", y = "Përqindja e votave", title="Përqindja e votave sipas subjektit politik",
       subtitle="Pa vota me kusht dhe me postë") +
  geom_text(aes(label=paste(percentage, 
                            "%", " (",df_2019$diff, ")", sep=""), size=3.5)) +
  ylim(0,35)

ggsave("../figures/example1-v2.png", width=12,height = 8)


df_2017 <- df_2017 %>% arrange(percentage)
df_2017$party_short <- factor(df_2017$party_short, levels = df_2019$party_short)
df_2017$party_long <- factor(df_2017$party_long, levels = df_2019$party_long)

ggplot(data=df_2017, aes(x=party_short, y=percentage, fill=party_short)) +
  geom_bar(stat="identity", width = 0.55, alpha=1) +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values=df_2019$color) +
  theme(legend.position="top",
          panel.background = element_rect(fill = "transparent"),
          panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank()) +
  ylim(0,35) +
  labs(fill = "Subjekti Politik", x = "", y = "Vota", title="Numri i votave sipas subjektit politik",
       subtitle="Pa vota me kusht dhe me poste")

ggsave("../figures/example2-v2.png", width=12,height = 8, bg = "transparent")

###
### Party seats
###

df <- read.csv("../data/raw/overall_seats.csv",stringsAsFactors=FALSE, encoding="UTF-8")

df_2019 <- df %>% filter(year == 2019)
df_2017 <- df %>% filter(year == 2017)

df_2019$diff <- round(df_2019$seats - df_2017$seats,1)
df_2019 <- df_2019 %>% arrange(seats)
df_2019$party_short <- factor(df_2019$party_short, levels = df_2019$party_short)
df_2019$party_long <- factor(df_2019$party_long, levels = df_2019$party_long)

for (i in 1:length(df_2019$diff)) {
  if (df_2019$diff[i] > 0) {
    df_2019$diff[i] <- paste("+",df_2019$diff[i],sep="")
  }
}

ggplot(data=df_2019, aes(x=party_short, y=seats, fill=party_short)) +
  geom_bar(stat="identity", width = 0.55) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_manual(values=df_2019$color) +
  labs(fill = "Subjekti Politik", x = "", y = "Ulëse", title="Numri i ulëseve sipas subjektit politik",
       subtitle="Pa vota me kusht dhe me postë") +
  geom_text(aes(label=paste(seats, 
                            " ulëse", " (",df_2019$diff, ")", sep=""), size=3.5, hjust=-.10)) +
  ylim(0,35) +
  theme(
    plot.title = element_text(hjust = 0, size = 24),    # Center title position and size
    plot.subtitle = element_text(hjust = 0),            # Center subtitle
  )

ggsave("../figures/uleset-2019.png", width=12,height = 8)


df_2017 <- df_2017 %>% arrange(seats)
df_2017$party_short <- factor(df_2017$party_short, levels = df_2019$party_short)
df_2017$party_long <- factor(df_2017$party_long, levels = df_2019$party_long)

ggplot(data=df_2017, aes(x=party_short, y=seats, fill=party_short)) +
  geom_bar(stat="identity", width = 0.35, alpha=1) +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values=df_2019$color) +
  theme(legend.position="top",
        panel.background = element_rect(fill = "transparent"),
        panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  ylim(0,35) +
  labs(fill = "Subjekti Politik", x = "", y = "Vota", title="Numri i ulëseve sipas subjektit politik",
       subtitle="Pa vota me kusht dhe me poste") +
  theme(
    plot.title = element_text(hjust = 0, size = 24),    # Center title position and size
    plot.subtitle = element_text(hjust = 0),            # Center subtitle
  )

ggsave("../figures/uleset-2017.png", width=12,height = 8, bg = "transparent")

















