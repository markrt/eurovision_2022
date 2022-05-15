## adding a variable from the semi qualifiers etc

# load packages
library(tidyverse)

# load data
spotify_data <- 
  read_csv("eurovision_spotify_data_2022.csv")

# creat three categories:
# qualifiers
# NQs
# big five
semi_1_qualifiers <- 
  c("Lithuania",
    "Switzerland",
    "Ukraine",
    "Netherlands",
    "Moldova",
    "Portugal",
    "Iceland",
    "Greece",
    "Norway",
    "Armenia")
semi_1_nq <- 
  c("Albania",
    "Latvia",
    "Slovenia",
    "Bulgaria",
    "Croatia",
    "Denmark",
    "Austria")
semi_2_qualifiers <- 
  c("Finland",
    "Serbia",
    "Azerbaijan",
    "Australia",
    "Estonia",
    "Romania",
    "Poland",
    "Belgium",
    "Sweden",
    "Czech Republic")
semi_2_nq <- 
  c("Israel",
    "Georgia",
    "Malta",
    "San Marino",
    "Cyprus",
    "Ireland",
    "North Macedonia",
    "Montenegro")
big_five <- 
  c("United Kingdom",
    "France",
    "Italy",
    "Germany",
    "Spain")

# add flags to data
spotify_data <- 
  spotify_data %>% 
  mutate(semi_outcome = 
           case_when(
             country %in% semi_1_qualifiers ~ "Qualified",
             country %in% semi_2_qualifiers ~ "Qualified",
             country %in% semi_1_nq ~ "NQ",
             country %in% semi_2_nq ~ "NQ",
             country %in% big_five ~ "Big Five"
           )) %>% 
  mutate(which_semi = 
           case_when(
             country %in% semi_1_qualifiers ~ "Semi 1",
             country %in% semi_1_nq ~ "Semi 1",
             country %in% semi_2_qualifiers ~ "Semi 2",
             country %in% semi_2_nq ~ "Semi 2",
             country %in% big_five ~ "Big Five"
           ))

# some quick exploratory work
# just to check it's working ok

spotify_data %>% 
  group_by(semi_outcome) %>% 
  summarise(mean_tempo = 
              mean(tempo))

# mean tempo among qualifiers is actually 9 bpm faster than nqs

spotify_data %>% 
  group_by(semi_outcome) %>% 
  mutate(mean_tempo = 
           mean(tempo)) %>% 
  ggplot() + 
  aes(x = tempo,
      y = semi_outcome) + 
  geom_point(aes(x = mean_tempo),
             size = 10,
             colour = "skyblue") + 
  geom_point() + 
  theme_minimal() + 
  labs(x = "BPM",
       y = "",
       title = "On average, qualifiers were a bit quicker",
       subtitle = "Big Five looking slow",
       caption = "@markrt")



### look i'm a bit sceptical of the "popularity" variable
# but let's see if one semi was tougher than the other

spotify_data %>% 
  filter(semi_outcome != "Big Five") %>% 
  group_by(which_semi) %>% 
  mutate(mean_popularity = 
           mean(track.popularity)) %>% 
  ggplot() + 
  aes(y = fct_rev(which_semi),
      x = track.popularity) + 
  geom_point(aes(x = mean_popularity),
             size = 10,
             colour = "skyblue") +
  geom_point() + 
  theme_minimal() + 
  labs(x = "Spotify's mysterious popularity variable",
       y = "")

# with the lot
spotify_data %>% 
  filter(semi_outcome != "Big Five") %>% 
  group_by(which_semi) %>% 
  summarise(mean_popularity = 
           mean(track.popularity)) 
# 60.5 vs 58.2
# wipe out the one weird outlier
spotify_data %>% 
  filter(semi_outcome != "Big Five" &
           track.popularity > 40) %>% 
  group_by(which_semi) %>% 
  summarise(mean_popularity = 
              mean(track.popularity)) 
# leaves them fairly similar to one another 

# all three of these qualified!!!
spotify_data %>% 
  filter(tempo > 124 & 
           tempo < 132)
# and these two!!!
spotify_data %>% 
  filter(tempo > 82 & 
           tempo < 89)
