### get features for the eurovision 2021 tracks from spotify

# packages
library(tidyverse)
library(spotifyr)

# login details
# sub in your own obviously
# you'll need an API key
Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXX')

# pull data
spotify_data <- 
  get_playlist_audio_features(username = "Spotify",
                              "37i9dQZF1DWVCKO3xAlT1Q")



# simpler artists variable
# so the data's not in a really annoying format
# nb it's only the first artist listed
# if there's several (eg Italy, France)
artist <- 
  unlist(spotify_data$track.artists) %>% 
  as_tibble() %>% 
  filter(lead(value == "artist")) %>% 
  rename(artist = value) %>% 
  filter(artist != "artist") %>% 
  pull()

# limit to relevant variables
# nb track popularity is as of 2022.05.14 at 15:15ish
spotify_data_shorter <- 
  spotify_data %>% 
  select(danceability:tempo,
         time_signature,
         key_name:key_mode,
         track.name,
         track.popularity)

# add artist variable
spotify_data_shorter$artist <- artist

# add country manually
spotify_data_shorter <- 
  spotify_data_shorter %>% 
  mutate(country = case_when(
    artist == "Subwoolfer" ~ "Norway",
    artist == "KALUSH" ~ "Ukraine",
    artist == "S10" ~ "Netherlands",
    artist == "BLANCO" ~ "Italy",
    artist == "Cornelia Jakobs" ~ "Sweden",
    artist == "Chanel" ~ "Spain",
    artist == "Ahez" ~ "France",
    artist == "Sam Ryder" ~ "United Kingdom", # will need to clean this up
    artist == "Amanda Tenfjord" ~ "Greece",
    artist == "MARO" ~ "Portugal",
    artist == "Rosa Linn" ~ "Armenia",
    artist == "Jérémie Makiese" ~ "Belgium",
    artist == "We Are Domi" ~ "Czech Republic",
    artist == "Fratii Advahov" ~ "Moldova",
    artist == "Ochman" ~ "Poland",
    artist == "wrs" ~ "Romania",
    artist == "STEFAN" ~ "Estonia",
    artist == "Konstrakta" ~ "Serbia",
    artist == "Sheldon Riley" ~ "Australia",
    artist == "Monika LIU" ~ "Lithuania",
    artist == "The Rasmus" ~ "Finland",
    artist == "Systur" ~ "Iceland",
    artist == "Malik Harris" ~ "Germany",
    artist == "Nadir Rustamli" ~ "Azerbaijan",
    artist == "Marius Bear" ~ "Switzerland",
    artist == "PIA MARIA" ~ "Austria",
    artist == "Citi Zēni" ~ "Latvia",
    artist == "LPS" ~ "Slovenia",
    artist == "Ronela Hajati" ~ "Albania",
    artist == "Intelligent Music Project" ~ "Bulgaria",
    artist == "Mia Dimšić" ~ "Croatia",
    artist == "REDDI" ~ "Denmark",
    artist == "Achille Lauro" ~ "San Marino",
    artist == "Andromache" ~ "Cyprus",
    artist == "Andrea" ~ "North Macedonia",
    artist == "Brooke" ~ "Ireland",
    artist == "Circus Mircus" ~ "Georgia",
    artist == "Michael Ben David" ~ "Israel",
    artist == "Emma Muscat" ~ "Malta",
    artist == "Vladana" ~ "Montenegro")) %>% 
  # now clean up the cases with multiple artists
  # shout if i've missed any
  mutate(artist = fct_recode(artist,
                             "Mahmood & Blanco" = 
                               "BLANCO",
                             "Alvan & Ahez" = 
                               "Ahez",
                             "Zdob și Zdub and Advahov Brothers" = 
                               "Fratii Advahov"))

# reorganise columns to make them more user-friendly, export
spotify_data_shorter %>% 
  relocate(artist, 
           country,
           track.name,
           time_signature,
           tempo,
           key_mode,
           key_name,
           mode_name) %>% 
  write_csv("eurovision_spotify_data_2022.csv")
