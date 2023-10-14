library(tidyverse)
library(jsonlite)
library(spotifyr)


# This file reads the data, and potentially pulls a lot of data from Spotify
source('get_spotify_track_data.R')


# Eventually this will be

user <- 'xm44d9m4rimzpbr0riuihsw0w'




# Total (and trend?)

summarise(p, total_streams = n())

distinct(t, track.id) %>%
  summarise(n_distinct_tracks = n())

filter(artists_tracks, track.id %in% t$track.id) %>%
  distinct(artist.id) %>%
  summarise(n_distinct_artists = n())

distinct(t, album.id) %>%
  summarise(n_distinct_albums = n())


paste('Mins streamed:', sum(p$ms_played / 60000))




