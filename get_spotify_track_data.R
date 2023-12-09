library(tidyverse)
library(jsonlite)
library(spotifyr)
library(keyring)

# Data on track plays is held in JSON files, to start with we need to pull
# all of these into a dataframe.

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
  

# RData files from prior executions
if (file.exists('data/spotify_play_history.RData')) {
  load('data/spotify_play_history.RData')
}

if (file.exists('data/spotify_data_raw.RData')) {
  load('data/spotify_data_raw.RData')
}



if (exists('plays')) {
  plays <- union(plays, current_plays)
} else {
  # Starting from scratch
  plays <- current_plays
}



# Spotify API Setup ----

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
token_expires <- now() + hours()


# Function to check and refresh the token if it will expire in refresh_mins mins

check_token_expiry <- function(refresh_mins = 5) {
  if(token_expires < now() + minutes(refresh_mins)) {
    assign('token_expires', now() + hours(), envir = .GlobalEnv)
    assign('access_token', get_spotify_access_token(), envir = .GlobalEnv)
  }
}


# Given a named vector of ids, call the function f with an access token in 
# chunks return a row-bound data frame. Used for batching API calls.

# keep_id will save whatever id was used to call the function, which is needed
# for related artists. The column name will be '1'.
call_by_chunks <- function(ids, FUN, n = 50, keep_id = FALSE, sleep = 0.5) {
    
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
      # Refresh the token as needed - this happens here in case we end up
      # taking over on hour on a single endpoint.
      check_token_expiry()
      
      x <- chunks[i]
      names(x) <- 'original.id'
      r <- FUN(x, authorization = access_token)
      
      if(keep_id) {
        r <- bind_cols(x, r)
      }
      
      result <- bind_rows(result, r)
      
      # We have limited control over handling rate limiting, spotifyr will just
      # wait for the prescribed wait time. After a series of foolish calls I
      # ended up in API jail for a day and lost all progress, so we'll 
      # periodically copy where we are up to just in case.
      # We can spare the memory more than the time...
      if(i %% 10 == 0) {
        assign('tmp_results', result, envir = .GlobalEnv)
      }
      
      setTxtProgressBar(pb, i)
      Sys.sleep(sleep)
    }
    
    close(pb)
    
    if(exists('tmp_results')) {
      rm('tmp_results', envir = .GlobalEnv)
    }
    
    return(result)
  }



# Get Spotify Data ----

message('Getting Spotify Data.')

# We don't need to call the API for things we saved from earlier runs, so each
# time we have a list of ids, check they don't already exist in our raw data
# and bind results for the new ones. 


## Tracks ----
x <- distinct(plays, track.id)

if(exists('track_raw')) {
  x <- filter(x, !track.id %in% track_raw$id)
  
  if(nrow(x) > 0) {
    track_raw <- track_raw %>%
      bind_rows(call_by_chunks(x, get_tracks))
  }
  
} else {
  track_raw <- call_by_chunks(x, get_tracks)
}


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


### Track Features ----
x <- select(track_raw, id)

if(exists('track_features_raw')) {
  x <- filter(x, !id %in% track_features_raw$id)
  
  if(nrow(x) > 0) {
    track_features_raw <- track_features_raw %>%
      bind_rows(call_by_chunks(x, get_track_audio_features, 100))
  }
  
} else {
  track_features_raw <- call_by_chunks(x, get_track_audio_features, 100)
}

track_features <- track_features_raw %>%
  select(
    id, loudness, tempo, time_signature, key, mode, acousticness, 
    danceability, energy, speechiness, instrumentalness, liveness, valence
  ) %>%
  rename(track.id = id)



## Albums ----
x <- distinct(track, album.id)

if(exists('album_raw')) {
  x <- filter(x, !album.id %in% album_raw$id)
  
  if(nrow(x) > 0) {
    album_raw <- album_raw %>%
      bind_rows(call_by_chunks(x, get_albums, 20))
  }
  
} else {
  album_raw <- call_by_chunks(x, get_albums, 20)
}

  
album <- album_raw %>%
  mutate(
    name = if_else(
      album_type == 'album',
      name,
      str_c(name, ' (', album_type, ')')
      )
  ) %>%
  select(
    id, external_ids.upc, label, name, popularity
  ) %>%
  rename(
    album.id = id,
    album_name = name,
    album_popularity = popularity
  )



## Artists ----
x <- unnest_longer(track, artist.id) %>%
  distinct(artist.id)

if(exists('artist_raw')) {
  x <- filter(x, !artist.id %in% artist_raw$id)
  
  if(nrow(x) > 0) {
    artist_raw <- artist_raw %>%
      bind_rows(call_by_chunks(x, get_artists, 20))
  }
  
} else {
  artist_raw <- call_by_chunks(x, get_artist, 20)
}

artist <- artist_raw %>%
  select(id, name, popularity, followers.total, genres) %>%
  rename(
    artist.id = id,
    artist_name = name,
    artist_popularity = popularity,
    followers = followers.total
  )



## Related Artists ----

# We'll limit this to artists with more than one track played as we only need
# the top artists and this is an expensive process. 

x <- unnest_longer(track, artist.id) %>%
  group_by(artist.id) %>%
  summarise(n = n(), .groups = 'drop') %>%
  filter(n > 1) %>%
  select(artist.id) %>%
  mutate(
    artist.id = sapply(artist.id, toString)
  )

if(exists('related_artist_raw')) {
  x <- filter(x, !artist.id %in% related_artist_raw$original.id)
  
  if(nrow(x) > 0) {
    related_artist_raw <- related_artist_raw %>%
      bind_rows(call_by_chunks(x, get_related_artists, 1, keep_id = TRUE))
  }
  
} else {
  related_artist_raw <- 
    call_by_chunks(x, get_related_artists, 1, keep_id = TRUE)
}


related_artist <- related_artist_raw %>%
  select(original.id, id, name, external_urls.spotify, followers.total) %>%
  rename(
    artist.id = original.id,
    related_artist_id = id,
    related_artist_name = name,
    related_artist_url = external_urls.spotify,
    related_artist_followers = followers.total
  )


## Users ----
user_raw <- 
  distinct(plays, username) %>%
  call_by_chunks(get_user_profile,  1)

user <- user_raw %>%
  select(id, display_name, any_of('images.url1'))


message('Data Loaded.')


# Save output and cleanup ----

save(
  plays, 
  file = 'data/spotify_play_history.RData')

save(
  track, track_features, album, artist, related_artist, user, 
  file = 'data/spotify_data.RData'
)


save(
  track_raw, track_features_raw, album_raw, artist_raw, related_artist_raw,
  user_raw, 
  file = 'data/spotify_data_raw.RData'
)

remove(list = ls())
