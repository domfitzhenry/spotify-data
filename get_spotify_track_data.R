library(tidyverse)
library(jsonlite)
library(spotifyr)
library(keyring)

# Data on track plays is held in JSON files, to start with we need to pull
# all of these into a dataframe.

# Parameters ----

# Set this to true to call Spotify API
force_refresh <- FALSE

# The list of columns that are retained from the JSON files
retain_cols <- 
  c('ts', 'username', 'reason_start', 'reason_end',
    # These are transformed/renamed from the response - id and duration_ms
    'track.id', 'duration_played')


# Load Local Files ----

message('Loading Local Data.')

# JSON files in source-data

# Just keep tracks rather than episodes, also note the ugly timezone conversion,
# which is definitely not the cleanest way to achieve this...
current_plays <-
  list.files(
    path = 'data-raw', recursive = TRUE,
    pattern = '.json', full.names = TRUE) %>%
  map_df(~read_json(path = ., simplifyVector = TRUE)) %>%
  filter(!is.na(spotify_track_uri)) %>%
  mutate(
    ts = with_tz(strptime(ts, '%Y-%m-%dT%H:%M:%S', tz="UTC"), Sys.timezone()),
    track.id = str_remove(spotify_track_uri, 'spotify:track:'),
    duration_played = dmilliseconds(ms_played)
  ) %>%
  select(all_of(retain_cols))
  

# RData file from prior executions
if (file.exists('data/spotify_play_history.RData')) {
  load('data/spotify_play_history.RData')
}


if (exists('plays')) {
  plays <- union(plays, current_plays)
} else {
  # Starting from scratch
  plays <- current_plays
}



# Spotify API Setup ----

# Given a named vector of ids, call the function f with an access token in 
# chunks return a row-bound data frame. Used for batching API calls.
call_by_chunks <- function(ids, FUN, token, n = 50, sleep = 0.1){
  
  result <- data.frame()
  
  message(
    paste('\n ', as.character(now()), '- Calling', nrow(ids), 'ids on the', 
    as.character(substitute(FUN)), 'endpoint.')
  )
  
  # Split into list of n comma separated values
  ids <- rename_with(ids, ~ 'id')
  chunks <- split(ids$id, ceiling(seq_along(ids$id) / n)) %>%
    lapply(paste, collapse = ',')
  
  # We could just vectorise this, but less control over frequency of calls, 
  # and no progress bar!
  # result <- map_df(chunks, ~ f(., authorization = token))
  
  # Add a progress bar
  pb <- txtProgressBar(min = 0, max=length(chunks), width = 50, style = 3)
  
  for (i in seq(1, length(chunks))) {
    r <- FUN(chunks[i], authorization = token)
    result <- bind_rows(result, r)
    setTxtProgressBar(pb, i)
    Sys.sleep(sleep)
  }
  
  close(pb)
  
  return(result)
}

# AUTHENTICATION

# If the keys haven't been setup, prompt for them
check_key <- function(key_name) {
  key_exists <- key_name %in% key_list()$service
  
  if(!key_exists){
    key_set(key_name, prompt = paste0(key_name, ": "))
  }
  
  return(key_get(key_name))
}


Sys.setenv(SPOTIFY_CLIENT_ID = check_key('SPOTIFY_CLIENT_ID'))
Sys.setenv(SPOTIFY_CLIENT_SECRET = check_key('SPOTIFY_CLIENT_SECRET'))

access_token <- get_spotify_access_token()



# Get Spotify Data ----

t <- distinct(plays, track.id)

if (force_refresh) {
  
  message('Getting Spotify Data.')
  
  # Get all track details
  track_raw <- call_by_chunks(t, get_tracks, access_token)
  
  track <- track_raw %>%
    mutate(
      duration = dmilliseconds(duration_ms),
      album.release_date = 
        parse_date_time(album.release_date, c('y', 'ym', 'ymd'))
    ) %>%
    hoist(artists, artist.id = 'id') %>%
    select(
      id, external_ids.isrc, name, album.release_date, album.id, duration, 
      popularity, explicit, artist.id
    ) %>%
    rename(
      track.id = id,
      track_name = name,
      track_popularity = popularity
    )
  
  track_features_raw <- 
    call_by_chunks(t, get_track_audio_features, access_token, 100)
  
  track_features <- track_features_raw %>%
    select(
      id, loudness, tempo, time_signature, key, mode, acousticness, 
      danceability, energy, speechiness, instrumentalness, liveness, valence
    ) %>%
    rename(track.id = id)
  
  # Get album details
  album_raw <- distinct(track, album.id) %>%
    call_by_chunks(get_albums, access_token, 20)
    
  album <- album_raw %>%
    select(
      id, external_ids.upc, label, name, popularity
    ) %>%
    rename(
      album.id = id,
      album_name = name,
      album_popularity = popularity
    )

  # Get all artist details
  artist_raw <- 
    unnest_longer(track, artist.id) %>%
    distinct(artist.id) %>%
    call_by_chunks(get_artists, access_token)
  
  artist <- artist_raw %>%
    select(id, name, popularity, followers.total, genres) %>%
    rename(
      artist.id = id,
      artist_name = name,
      artist_popularity = popularity,
      followers = followers.total
    )
  
  # For user display names
  user_raw <- 
    distinct(plays, username) %>%
    call_by_chunks(get_user_profile, access_token, 1)
  
  user <- user_raw %>%
    select(id, display_name, any_of('images'))
  
}


# Save output and cleanup
save(
  plays, track, track_features, album, artist, user, 
  file = 'data/spotify_play_history.RData'
)

save(
  track_raw, track_features_raw, album_raw, artist_raw, user_raw, 
  file = 'data/spotify_data_raw.RData'
)

remove(ls())

message('Data Loaded.')


