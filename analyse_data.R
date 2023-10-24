library(tidyverse)

# This file reads the data, and potentially pulls a lot of data from Spotify

# Set this to true to call Spotify API
force_refresh <- FALSE

if (force_refresh) source('get_spotify_track_data.R')


load('data/spotify_play_history.RData')


# Generate datasets for the user that will be loaded and referenced in the qmd

# User ----
u <- filter(user, display_name == 'Sammy') %>% pull(id)

# This will hold a filtered and transformed plays dataset used for the remaining
# analysis. When counting the number of plays of a track, we will exclude any
# that didn't get played for very long - arbitrarily 10% of the track length.

# We'll also convert our categorical integers into factors.
# Key is in pitch class notation
pcn <- data.frame(
  key_index = seq(0, 11),
  key_factor = 
    ordered(c('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'))
)

user_plays <- 
  filter(plays, username == u) %>%
  inner_join(track, by = 'track.id') %>%
  inner_join(album, by = 'album.id', suffix = c('_track', '_album')) %>%
  left_join(track_features, by = 'track.id') %>%
  left_join(pcn, by = c('key' = 'key_index')) %>%
  mutate(
    track_played = duration_played / duration >= 0.1,
    
    # Time signature is an estimate of the meter / 4, values range from 3-7
    # other values will be forced to NA.
    time_signature = 
      factor(str_c(time_signature, "/4"), levels = str_c(seq(3, 7), "/4")),
    
    key = key_factor,
    
    # Mode: 0 = minor, 1 = major
    mode = cut(mode, breaks = 2, labels = c('minor', 'major'))
  ) %>%
  select(!key_factor)

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


## Ordered Tables ----

### Tracks
top_tracks <-
  filter(user_plays, track_played) %>%
  group_by(track.id) %>%
  summarise(
    all_plays = n(),
    current_plays = sum(is_current),
    old_plays = all_plays - current_plays,
    first_listen = min(ts)
  ) %>%
  filter(all_plays >= 5) %>%
  inner_join(track, by = 'track.id') %>%
  mutate(
    artist_names = 
      map_chr(
      artist.id, 
      function(a) str_flatten_comma(
        artist[artist$artist.id %in% a, 'artist_name'])
      ),
    old_rank = min_rank(desc(old_plays)),
    current_rank = min_rank(desc(current_plays)),
    rank_diff = if_else(old_plays > 0, old_rank - current_rank, NA),
    track_url = paste0('http://open.spotify.com/track/', track.id),
    ) %>%
  arrange(current_rank) %>%
  select(
    rank_diff, current_rank, track_name, artist_names, first_listen, 
    current_plays, all_plays, track_popularity, track_url
  )



### Artists
top_artists <- 
  filter(user_plays, track_played) %>%
  # We'll credit the song only to the first listed artist, as it might cause
  # obscure collaborations to appear high in the list.
  mutate(artist.id = map_chr(artist.id, 1)) %>%
  group_by(artist.id) %>%
  summarise(
    all_plays = n(),
    current_plays = sum(is_current),
    old_plays = all_plays - current_plays,
    first_listen = min(ts),
    distinct_tracks = n_distinct(track.id)
    ) %>%
  filter(all_plays >= 5) %>%
  inner_join(artist, by = 'artist.id') %>%
  mutate(
    old_rank = min_rank(desc(old_plays)),
    current_rank = min_rank(desc(current_plays)),
    rank_diff = if_else(old_plays > 0, old_rank - current_rank, NA),
    artist_url = paste0('http://open.spotify.com/artist/', artist.id),
    ) %>%
  arrange(current_rank) %>%
  select(
    rank_diff, current_rank, artist_name, distinct_tracks, first_listen, 
    current_plays, all_plays, followers, artist_popularity, artist_url
  )


### Albums
top_albums <- 
  filter(user_plays, track_played) %>%
  mutate(
    artist_names = 
      map_chr(
        artist.id, 
        function(a) str_flatten_comma(
          artist[artist$artist.id %in% a, 'artist_name'])
      )
    ) %>%
  group_by(album.id, artist_names, album.release_date) %>%
  summarise(
    all_plays = n(),
    current_plays = sum(is_current),
    old_plays = all_plays - current_plays,
    first_listen = min(ts),
    distinct_tracks = n_distinct(track.id),
    .groups = 'drop'
    ) %>%
  filter(all_plays >= 5) %>%
  inner_join(album, by = 'album.id') %>%
  mutate(
    old_rank = min_rank(desc(old_plays)),
    current_rank = min_rank(desc(current_plays)),
    rank_diff = if_else(old_plays > 0, old_rank - current_rank, NA),
    album_url = paste0('http://open.spotify.com/album/', album.id),
    release_year = year(album.release_date)
    ) %>%
  arrange(current_rank) %>%
  select(
    current_rank, rank_diff, album_name, artist_names, distinct_tracks, 
    first_listen, current_plays, all_plays, album_popularity, release_year, 
    album_url
  )







## Genres ----

# Genres are associated with an artist, so taking a similar approach to the top
# artist list.

top_genres <- 
  filter(user_plays, track_played) %>%
  mutate(artist.id = map_chr(artist.id, 1)) %>%
  inner_join(artist, by = 'artist.id') %>%
  select(ts, track.id, genres, is_current) %>%
  unnest_longer(genres) %>%
  group_by(genres) %>%
  summarise(
    all_plays = n(),
    current_plays = sum(is_current),
    old_plays = all_plays - current_plays,
    first_listen = min(ts),
    distinct_tracks = n_distinct(track.id),
    .groups = 'drop'
  ) %>%
  filter(all_plays >= 5) %>%
  mutate(
    old_rank = min_rank(desc(old_plays)),
    current_rank = min_rank(desc(current_plays)),
    rank_diff = if_else(old_plays > 0, old_rank - current_rank, NA)
  ) %>%
  arrange(current_rank) %>%
  select(
    current_rank, rank_diff, genres, distinct_tracks, first_listen, 
    current_plays, all_plays
  )




save(
  top_tracks, top_artists, top_albums, top_genres,
  file = 'data/top_things.RData'
)






















## Track audio features ----


# Loudness is the average dB across the track in a typical range from -60 to 0
# so is best used to compare relatively and the actual value is less important.


# Tempo is BPM


# Time signature is an estimate of the meter / 4, values range from 3-7 but this
# is categorical.


# Key is in pitch class notation, -1 indicates not detected.



# Mode: 0 = minor, 1 = major



# Acousticness is a confidence measure of whether the track is acoustic


# Danceability is continuous from 0 (not danceable) to 1 (very danceable)


# Energy is continuous from 0 (not energetic) to 1 (very energetic)


# Speechiness is a confidence measure if the track is spoken word, with 3
# categories. 0-0.33 no spoken word, 0.33-0.66 a mixture, >0.66 spoken word.



# Instrumentalness is a confidence measure of whether there are vocals (0.5+)


# Liveness is a confidence measure of whether there is an audience (0.8+)


# Valence s continuous from 0 (negative emotion) to 1 (positive emotion)




# Artist popularity vs user popularity
top_artists %>%
  mutate(
    user_popularity = percent_rank(all_plays) * 100
  ) %>%
  ggplot(aes(x = user_popularity, y = artist_popularity)) +
  geom_point()


# Top albums by release year
top_albums %>%
  ggplot(aes(x = release_year)) +
  geom_bar()
