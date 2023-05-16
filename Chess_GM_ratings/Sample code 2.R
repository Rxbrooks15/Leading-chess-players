
#Chess_Project
#Name: Raine Brookshire
#Date: 5/15/23
#DALI sample code 2

# Libraries ---------------------------------------------------------------

library(ggplot2)
library(gganimate)
library(animation)
library(grid)
library(tidyverse)
library(lubridate)
library(readr)
library(readxl)
library(zoo)
library(png)
library(chess)
library(chessfont)
library(jpeg)
library(ggimage)
library(gg)
library(ggimg)
library(ggpubr)
library(magick)

# Intro  ------------------------------------------------------------------
#Chess has become an inspiring and popular "sport" over the years, especially from the series the queens gambit
#I am greatly interested in this topic as an avid chess player

# To come up with a project, I first could think of using my own chess data to show my ELo increase over the years
# but a more complex example could be openings I play and how likely I am to draw, lose or win with them. 
# I could potentially create a animated chess board that shows the evolution of the top 3 moves over the board but that seems to complex 

#From the many grandmasters, I can use some of their games to show how they have improved as players 

#Vocab
#Elo: measures the relative strength of a player in some games, such as chess, compared to other players. 
#The higher the better 
#Elo <-  2500-2700 = most grand masters (GM)
#2400-2500 = Most IMs and some Grandmasters 
#2300-2400 = Most Fide Masters (FM) and some IMs
#2200-2300 = Fide Candidate Masters (CM), most NMs

chess <- read_csv(file.choose())
chess
glimpse(chess)
head(chess)
tail(chess)
str(chess)

#interesting things I notice are the rank and the Elo
#from these I can create some sort of relationship between the two or 
#show individual plots that show the evolution of Elo rating or a players ranking over the years 


# Data Exploration and Testing--------------------------------------------------------


chess <- read_csv(file.choose())
chess <- read_csv(file.choose())
#fide_historical 
chess

chess %>% 
  distinct(ranking_date)
  filter(name == "Carlsen, Magnus") %>%
  View
#Magnus carlsen and Garry Kasparov are known to be one of the best chess players after the 2000s so 
#maybe I can compare the two players
  #Magnus carlsen is one of the most sensational players so maybe I can tell his unique story
  #using my data 
  

  
chess %>% 
  #converts ranking_date to years 
  mutate(year = year(as.Date(ranking_date, format = "%Y-%m-%d"))) %>% 
  View


chess %>% 
  #Creates a year column that has the year as in the desired format 
  mutate(ranking_new = paste0("20",
                              str_sub(ranking_date,
                                      start = nchar(ranking_date) - 1))) %>% 
  select(ranking_new) %>% 
  unique()

  
chess %>% 
  #Creates a year column that has the year as in the desired format 
  mutate(ranking_new = paste0("20",
                              str_sub(ranking_date,
                                      start = nchar(ranking_date) - 1)))
  
 

# create a new column with only the year from the ranking_date column
chess %>% 
  mutate(year = year(ranking_date))

chess %>% 
  mutate(year = str_pad(as.numeric(str_sub(ranking_date,
                                           start = 1, end = 2))) + 2000)
  

#Basic bar plot showing the relative rank of players
#The lower the rank the better 
chess %>% 
  #Creates a year column that has the year as in the desired format 
  mutate(year = paste0("20",
                              str_sub(ranking_date,
                                      start = nchar(ranking_date) - 1))) %>% 
  rename(ranking = rank) %>% 
  group_by(year) %>% 
  mutate(adjusted_rank = rank(-rating)) %>% 
  filter(adjusted_rank <= 15) %>% 

  ggplot(.,aes(y = adjusted_rank,
             x = name ,fill = name))+
  geom_bar(stat = "identity",
           show.legend = FALSE)+
  # geom_text(aes(label = name, hjust = 2))
  scale_y_reverse()+
  coord_flip()


unique(chess$ranking_new)


chess %>% 
  mutate(year = paste0("20",
                       str_sub(ranking_date,
                               start = nchar(ranking_date) - 1))) %>% 
  rename(ranking = rank) %>% 
  group_by(year,ranking, name) %>% 
  # filter(name == "Kasparov, Garry" |name == "Ivanchuk, Vassily") %>% 
  summarize(adjusted_rank = 1/ranking, na.rm = TRUE) %>% 
  group_by(year) %>%
  filter(ranking <= 10) %>% 
  mutate(ranked_names = rank(adjusted_rank)) %>%
  filter(ranked_names <= 7.5) %>% 


  ggplot(., aes(x = reorder(name,-adjusted_rank), y = adjusted_rank, fill = name)) + 
  geom_bar(stat = "identity",
           show.legend = FALSE) +
  geom_text(aes(label = name, hjust = 2)) +
  coord_flip()+
  # scale_x_reverse()+
  # transition_states(year, state_length = 4000) +
  # ease_aes("linear") +
  theme_minimal()
?rank

help(rank)

5+5


# Top 10 chess  -----------------------------------------------------------


chess$name[which(chess$name=='Bologan, Victor')] <- 'Bologan, Viktor'
chess$name[which(chess$name=='Bruzon Batista, Lazaro')] <- 'Bruzon, Lazaro'
chess$name <- ifelse(chess$name %in% c('Dominguez Perez, Lenier',
                                       'Dominguez Perez, Leinier',
                                       'Dominguez, Lenier'), 'Dominguez, Leinier', chess$name)
chess$name[which(chess$name=='Dreev, Aleksey')] <- 'Dreev, Alexey'
chess$name[which(chess$name=='Iturrizaga Bonelli, Eduardo')] <- 'Iturrizaga, Eduardo'
chess$name[which(chess$name=='Kasparov, Gary')] <- 'Kasparov, Garry'
chess$name[which(chess$name=='Mamedyarov, Shakhriyaz')] <- 'Mamedyarov, Shakhriyar'
chess$name[which(chess$name=='McShane, Luke J')] <- 'McShane, Luke J.'
chess$name[which(chess$name=='Polgar, Judit (GM)')] <- 'Polgar, Judit'
chess$name[which(chess$name=='Sadler, Matthew D')] <- 'Sadler, Matthew'
chess$name[which(chess$name=='Short, Nigel D')] <- 'Short, Nigel D.'

#Bar chart showing
chess %>%
  View
rating_animation <- chess %>% 
  mutate(year = paste0("20",
                       str_sub(ranking_date,
                               start = nchar(ranking_date) - 1))) %>% 
  rename(ranking = rank) %>% 
  group_by(year, name) %>% 
  summarize(mean_rating = mean(rating, na.rm = TRUE)) %>%
  group_by(year) %>% 
  mutate(ranked_names = rank(-mean_rating)) %>% 
  filter(ranked_names <= 10) %>%
  filter(mean_rating >= 2400) %>% 
  ggplot(., aes(x = ranked_names,
                y = mean_rating,
                fill = name)) + 
  geom_bar(stat = "identity",
           show.legend = FALSE) +
  #ylim(2000,2900)+
  geom_text(aes(label = name, hjust = -0.5), size = 5) +
  # geom_text(aes(label = mean_rating, hjust -2))+
  coord_flip()+
  #to get the top players on the top 
  # top gives you the number 1 ranked to number 10
  scale_x_reverse()+
  # ylim(5000,200000)
  # scale_y_continuous(limits = c(5000,200000))
  transition_states(year,
                    transition_length = 20,
                    state_length = 4) +
  ease_aes("linear") +
  labs(title = "top 10 ranked chess players over the years ",
       x = "Rank",
       y = "Average rating")+
  
  theme_minimal()
#Run this line 
rating_animation
rating_animation

animate(rating_animation, fps = 1)  

?scale_x_continuous




# Final animated chess plot -----------------------------------------------



# Convert data types
chess$ranking_date <- dmy(chess$ranking_date)
chess$name <- as.character(chess$name)

# Create age variable from birth year
chess$age <- year(chess$ranking_date) - chess$birth_year
chess

# Check for duplicate names
chess.check <- as.data.frame(as.character(sort(unique(chess$name))))
# 
# chess %>%
#   filter(rank <= 10)

chess %>% 
  group_by(year = year(ranking_date)) %>%
  summarise(count = n())

  chess %>%
    mutate(ranking_date = as.Date(ranking_date, format = "%Y-%m-%d")) %>%
    group_by(year = year(ranking_date)) %>%
    summarise(count = n())


# Create a quarter variable (yearly quarter)
chess$quarter <- quarter(chess$ranking_date, with_year = TRUE)

# New dataframe summarised by player and quarter
chess.quarterly <- chess %>% 
  group_by(name, quarter) %>% 
  summarise(avg_rating = mean(rating))
chess %>% 

  View

# Create a quarter variable (yearly quarter)
chess$quarter <- quarter(chess$ranking_date, with_year = TRUE)
chess %>% 
  View

# New dataframe summarised by player and quarter
chess.quarterly <- chess %>% 
  group_by(name, quarter) %>% 
  summarise(avg_rating = mean(rating))
chess
# Making a new dummy variable to color the following plot           
chess.quarterly$Player <- ifelse(chess.quarterly$name == 'Carlsen, Magnus', 'Carlsen',
                              ifelse(chess.quarterly$name == 'Kasparov, Garry', 'Kasparov',
                                     ifelse(chess.quarterly$name == 'Polgar, Judit','Polgar',
                                            ifelse(chess.quarterly$name == 'Kramnik, Vladimir','Kramnik',
                                                   ifelse(chess.quarterly$name == 'Anand, Viswanathan', 'Anand',
                                                          'Other Players'))))) 

#chess.quarterly$pawn <- pawn

chess.quarterly %>% View
#pawn <- "https://www.google.com/url?sa=i&url=https%3A%2F%2Fwww.123rf.com%2Fphoto_108931772_pawn-vector-icon-isolated-on-transparent-background-pawn-logo-concept.html&psig=AOvVaw2Iv5QTyADLn7B3F0hJK3YG&ust=1679004983675000&source=images&cd=vfe&ved=0CA8QjRxqFwoTCIiRiov73v0CFQAAAAAdAAAAABAD"

#Anand image to be used to identify the player 
A <- readPNG(file.choose())
A

#Will use the pawn image to represent the points 
pawn <- image_read(file.choose())
5+5
chess.quarterly <- image
pawn1 <- readJPEG(file.choose())
pawn1
class(pawn)
as.data.frame(pawn)
pawn
class(pawn)
hi <- c(pawn)
class(hi)

chess.quarterly$quarter_date <- as.Date(paste0(chess.quarterly$quarter, "-01"))

#Final code showing averge rating of different players
# Ratings by quarter, highlighting Kasparov and Carlsen                             
chess_animation <- ggplot(data = chess.quarterly,
                          aes(x = quarter,
                              y = avg_rating,
                              color = Player)) +
  # annotation_custom(rasterGrob(A), xmin=2015, xmax=2017, ymin=2700, ymax=2850)+
  
  geom_point(size = 3) +
  geom_path(size = 1)+ 
  # geom_point_img(data = chess.quarterly,
  #                    aes(x = quarter,
  #                        y = avg_rating,
  #                        img = pawn1))+
  
  #gives unique color to the specified names selected earlier
  scale_color_manual(values = c('black', 'red','blue','orange','lightgrey', "brown")) +
  #For some reason using the full names of the chess players reorders them in alphabetical order 
  # and causes some paths to be hidden under the other players
  # ylim(2600,2900)+
  theme(legend.position=c(0.75, 0.1), 
        legend.direction = 'horizontal', 
        legend.title = element_blank()) +
  labs(title = "The Dominant Rise Of Magnus Carlesen ",
       x = 'Year',
       y = 'Average Elo rating',
       subtitle = "Rating evolution 5 popular players")+
  transition_reveal(quarter)+
  ease_aes("linear")+
  theme_minimal()
# transition_states(states = year(chess.quarterly$quarter),
#                     transition_length = 1,
#                     state_length = 2) +
# ease_aes("linear")\
anim_save()


chess_animation
chess_animation

#comments 
#From the animation we can see the appearance of magnus in the year
#2005 and the animation documents his sudden rise from being an average player
# He then becomes the higest rated player and dominates the competition for years 
#This not only documents the dominace of Carlsen over his competition
#but depicts his story of starting from the bottom and going up 
# It is also interesting because the graph documents the story of Carlsen's future potential and strength
#As of this moment he is still the number 1 ranked player and even though
# the data collected stopped at 2017 he is still the number 1 player with the highest ELO, thus 
#indicating his sheer superiority




# Static plot  ------------------------------------------------------------



chess

# Convert data types
chess$ranking_date <- dmy(chess$ranking_date)
chess$name <- as.character(chess$name)

# Create age variable from birth year
chess$age <- year(chess$ranking_date) - chess$birth_year

# Check for duplicate names
chess.check <- as.data.frame(as.character(sort(unique(chess$name))))
chess

chess$name[which(chess$name=='Bologan, Victor')] <- 'Bologan, Viktor'
chess$name[which(chess$name=='Bruzon Batista, Lazaro')] <- 'Bruzon, Lazaro'
chess$name <- ifelse(chess$name %in% c('Dominguez Perez, Lenier',
                                       'Dominguez Perez, Leinier',
                                       'Dominguez, Lenier'), 'Dominguez, Leinier', chess$name)
chess$name[which(chess$name=='Dreev, Aleksey')] <- 'Dreev, Alexey'
chess$name[which(chess$name=='Iturrizaga Bonelli, Eduardo')] <- 'Iturrizaga, Eduardo'
chess$name[which(chess$name=='Kasparov, Gary')] <- 'Kasparov, Garry'
chess$name[which(chess$name=='Mamedyarov, Shakhriyaz')] <- 'Mamedyarov, Shakhriyar'
chess$name[which(chess$name=='McShane, Luke J')] <- 'McShane, Luke J.'
chess$name[which(chess$name=='Polgar, Judit (GM)')] <- 'Polgar, Judit'
chess$name[which(chess$name=='Sadler, Matthew D')] <- 'Sadler, Matthew'
chess$name[which(chess$name=='Short, Nigel D')] <- 'Short, Nigel D.'
chess



# Create a quarter variable (yearly quarter)
chess$quarter <- quarter(chess$ranking_date, with_year = TRUE)
chess
# New dataframe summarised by player and quarter
chess.quarterly <- chess %>% 
  group_by(name, quarter) %>% 
  summarise(avg_rating = mean(rating))


# New dataframe summarised by player and quarter
chess.quarterly <- chess %>% 
  group_by(name, quarter) %>% 
  summarise(avg_rating = mean(rating))



# Ratings by quarter, highlighting the dominance of Kasparov and Carlsen                             
ggplot(data=chess.quarterly,
       aes(x=quarter,
           y=avg_rating)) +
  geom_point(aes(col = Player)) +
  #to see the general path of Elo increase over the years 
  geom_smooth(aes(col = Player),
              linetype=2,
              size=0.5,
              se=FALSE) +
  scale_color_manual(values = c('blue',
                                      'darkgreen',
                                      'black',
                                      'red',
                                      'lightgrey',
                                      "brown")) +
  theme(legend.position=c(0.75, 0.1), 
        legend.direction = 'horizontal', 
        legend.title = element_blank()) +
  labs(title="Carlsen's Dominant Rise",
       x='Year',
       y='Average Elo rating',
       subtitle = "Comparison of Carlsen vs Other chess players ")+
  theme_minimal()

chess
