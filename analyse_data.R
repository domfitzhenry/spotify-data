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

user_plays <- mutate(user_plays, is_current = first_date <= ts)


# Summary numbers ----

play_summary <- list()

play_summary['total_streams'] <- 
  filter(user_plays, track_played) %>% nrow()

play_summary['current_streams'] <- 
  filter(user_plays, track_played, is_current) %>% nrow()


play_summary['total_tracks'] <- 
  filter(user_plays, track_played) %>% 
  distinct(track.id) %>%
  nrow()

play_summary['current_tracks'] <- 
  filter(user_plays, track_played, is_current) %>% 
  distinct(track.id) %>%
  nrow()


play_summary['total_artists'] <- 
  filter(user_plays, track_played) %>% 
  unnest_longer(artist.id) %>%
  distinct(artist.id) %>%
  nrow()

play_summary['current_artists'] <- 
  filter(user_plays, track_played, is_current) %>% 
  unnest_longer(artist.id) %>%
  distinct(artist.id) %>%
  nrow()


play_summary['total_albums'] <- 
  filter(user_plays, track_played) %>% 
  distinct(album.id) %>%
  nrow()

play_summary['current_albums'] <- 
  filter(user_plays, track_played, is_current) %>% 
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


play_summary['total_duration'] <- 
  sum(user_plays$duration_played) %>%
  pretty_seconds()

play_summary['current_duration'] <- 
  filter(user_plays, is_current) %>% 
  select(duration_played) %>%
  sum() %>%
  pretty_seconds()


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
  filter(user_plays, is_current) %>% 
  mutate(date = date(ts)) %>%
  right_join(date_df, by = 'date') %>%
  group_by(week_start, weekday) %>%
  summarise(
    hours_played = coalesce(round(sum(duration_played) / 3600, 2), 0.0),
    .groups = 'drop'
  )



plays_by_hour <- 
  filter(user_plays, is_current) %>% 
  mutate(
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
  last_date, first_date, play_summary, plays_by_date, plays_by_hour,
  file = 'data/summary_date_data.RData'
)



# What we listened to ----



# Track Ranking
track_ranks <- 
  filter(user_plays, track_played) %>%
  group_by(track.id) %>%
  summarise(
    all_plays = n(),
    current_plays = sum(is_current)
  ) %>%
  inner_join(track, by = 'track.id') %>%
  mutate(
    all_rank = min_rank(desc(all_plays)),
    current_rank = min_rank(desc(current_plays)),
    rank_diff = all_rank - current_rank
  ) %>%
  select(
    track_name, track_popularity, current_plays, current_rank, all_plays,
    all_rank, rank_diff, track.id, album.id, artist.id
  )


# Top 100
track_ranks %>%
  filter(current_rank <= 100) %>%
  arrange(current_rank) %>%
  mutate(
    artists = map_chr(
      artist.id, 
      function(a) str_flatten_comma(
        artist[artist$artist.id %in% a, 'artist_name'])
      )
  ) %>%
  select(current_rank, track_name, artists, current_plays, all_plays, rank_diff, track_popularity)
