library(tidyverse)
library(janitor)
# setwd("~/Documents/class/stats-final-project/")
raw_data <- read.csv("data/SpotifyClustersData.csv") %>% clean_names() 
tempo <- read.csv("tempo.csv") %>% clean_names() 

key_alpha <- c('C','C#/Db','D','D#/Eb','E','F','F#/Gb','G','G#/Ab','A','A#/Bb','B')

# creating a new df for mapping keys
key_map <- data.frame(key = c(0:11),
                      key_alpha = key_alpha)

data <- raw_data %>%
  full_join(key_map, "key") %>% 
  mutate(mode = str_replace(as.character(mode), "0", "minor"),
         mode = str_replace(as.character(mode), "1", "major"),
         time_signature = as.character(time_signature),
         # converting keys to alphabet
         key = key_alpha,
         # adding a column for whether the artist is one or multiple. True for Multiple, false for single
         multiple_artists = grepl(";", artists)
  ) %>% 
  select(-1, -22) %>%
  # removing duplicates in songs, because some tracks are in multiple albums
  distinct(track_name, artists, .keep_all = TRUE)


popular_artists <- data %>% group_by(artists) %>% 
  summarize(count = n()) %>% 
  filter(count >= 20) 

filtered <- data %>% filter(artists %in% popular_artists$artists) %>% 
  filter(duration_ms <= 600000) %>%
  #removing the track_id, title, artists, albums because it's not needed.
  select(5:21) %>%
  mutate(
    tempo_cat = cut(tempo,
                    breaks=c(0, 20, 40, 60, 66, 76, 108, 120, 168, 176, 200, 1000),
                    labels=c('Larghissimo','Grave','Lento/Largo','Larghetto','Adagio','Andante','Moderato','Allegro','Vivace','Presto','Prestissimo'))
  )