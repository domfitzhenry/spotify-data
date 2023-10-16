library(tidyverse)
library(jsonlite)
library(spotifyr)


# This file reads the data, and potentially pulls a lot of data from Spotify
source('get_spotify_track_data.R')


# Generate datasets for the user that will be loaded and referenced in the qmd

# User ----
u <- 'xm44d9m4rimzpbr0riuihsw0w'

# This will hold a filtered and transformed plays dataset used for the remaining
# analysis. When counting the number of plays of a track, we will exclude any
# that didn't get played for very long - arbitrarily 10% of the track length.

user_plays <- 
  filter(plays, username == u) %>%
  inner_join(track, by = 'track.id') %>%
  inner_join(album, by = 'album.id', suffix = c('_track', '_album')) %>%
  left_join(track_features, by = 'track.id') %>%
  mutate(track_played = duration_played / duration >= 0.1)
  

last_date <- max(date(user_plays$ts))
first_date <- last_date - years(1)


current_user_plays <- filter(user_plays, ts >= first_date)


# Summary numbers ----

## Total streams
total_streams <- filter(user_plays, track_played) %>% nrow()

current_streams <- filter(current_user_plays, track_played) %>% nrow()


## Total tracks
total_tracks <- filter(user_plays, track_played) %>% 
  distinct(track.id) %>%
  nrow()

current_tracks <- filter(current_user_plays, track_played) %>% 
  distinct(track.id) %>%
  nrow()


## Total artists
total_artists <- filter(user_plays, track_played) %>% 
  inner_join(artists_tracks, by = 'track.id', relationship = 'many-to-many') %>%
  distinct(artist.id) %>%
  nrow()

current_artists <- filter(current_user_plays, track_played) %>% 
  inner_join(artists_tracks, by = 'track.id', relationship = 'many-to-many') %>%
  distinct(artist.id) %>%
  nrow()


## Total albums
total_albums <- filter(user_plays, track_played) %>% 
  distinct(album.id) %>%
  nrow()

current_albums <- filter(current_user_plays, track_played) %>% 
  distinct(album.id) %>%
  nrow()


## Total Duration (don't need to filter by played here)

# We want a function that pretty prints the duration like lubridate does
pretty_seconds <- function(secs) {
  
  result <- paste(secs, 'seconds')
  
  SECONDS_IN_ONE <- c(
    second = 1,
    minute = 60,
    hour   = 3600,
    day    = 86400,
    week   = 604800,
    year   = 31557600
  )
  
  if (secs >= SECONDS_IN_ONE[1]) {
    unit_index <- max(which(secs >= SECONDS_IN_ONE))
    
    result <- paste0(
      "~", round(secs / SECONDS_IN_ONE[unit_index], 2), " ", 
      names(SECONDS_IN_ONE)[unit_index], 's'
    )
  }
  
  return(result)
}


total_duration <- pretty_seconds(sum(user_plays$duration_played))
current_duration <- pretty_seconds(sum(current_user_plays$duration_played))


# Date transforms ----

date_df <- 
  data.frame(
    date = seq.Date(first_date, last_date, by = 'day')
  ) %>%
  mutate(
    weekday = wday(date, label = TRUE),
    week_start = floor_date(date, "week")
  )


plays_by_date <-
  mutate(current_user_plays, date = date(ts)) %>%
  right_join(date_df, by = 'date') %>%
  group_by(week_start, weekday) %>%
  summarise(
    tracks_played = sum(!is.na(track.id)),
    hours_played = coalesce(round(sum(duration_played) / 3600, 2), 0.0),
    .groups = 'drop'
  )



plays_by_hour <- 
  mutate(
    current_user_plays,
    start_time = ts,
    end_time = ts + duration_played,
    hour_start = map2(
      floor_date(start_time, unit = 'hours'),
      floor_date(end_time, unit = 'hours'),
      seq.POSIXt, by = 'hours'
    )
  ) %>%
  unnest(cols = c('hour_start')) %>%
  mutate(
    hour_end = hour_start + dhours(1),
    duration_in_hour = 
      pmin(hour_end, end_time) - pmax(hour_start, start_time),
    hour = as.factor(hour(hour_start))
  ) %>%
  group_by(hour) %>%
  summarise(
    duration = sum(duration_in_hour)
  ) %>%
  mutate(
    norm_duration = seconds(duration) / seconds(max(duration))
  )


# Save data to be loaded to qmd for viz
save(
  last_date, first_date,
  current_streams, current_tracks, current_albums, current_artists, current_duration,
  total_streams, total_tracks, total_albums, total_artists, total_duration,
  plays_by_date, plays_by_hour,
  file = 'data/summary_date_data.RData'
)



