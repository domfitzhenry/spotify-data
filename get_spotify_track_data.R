# Data on track plays is held in endsong_# files, to start with we need to pull
# all of these into a dataframe.

# Parameters ----

# Set this to true to call Spotify API
force_refresh <- TRUE

# The list of columns that are retained from the JSON files
retain_cols <- c('ts', 'username', 'reason_start', 'reason_end')

# Where we expect to find the RData file from prior executions
db_path <- 'data/spotify_play_history.RData'



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
  select(all_of(retain_cols), track.id, duration_played)
  

# RData file from prior executions
if (file.exists(db_path)) load(db_path)


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
    result <- rbind(result, r)
    setTxtProgressBar(pb, i)
    Sys.sleep(sleep)
  }
  
  close(pb)
  
  return(result)
}

# AUTHENTICATION
Sys.setenv(SPOTIFY_CLIENT_ID = keyring::key_get("SPOTIFY_CLIENT_ID"))
Sys.setenv(SPOTIFY_CLIENT_SECRET = keyring::key_get("SPOTIFY_CLIENT_SECRET"))

access_token <- get_spotify_access_token()



# Get Spotify Data ----

t <- distinct(plays, track.id)

if (force_refresh) {
  
  message('Getting Spotify Data.')
  
  # Get all track details
  track <- 
    call_by_chunks(t, get_tracks, access_token) %>%
    select(
      id, external_ids.isrc, name, album.release_date, album.id, duration_ms, 
      popularity, explicit, artists
    ) %>%
    mutate(
      duration = dmilliseconds(duration_ms),
      album.release_date = 
        parse_date_time(album.release_date, c('y', 'ym', 'ymd'))
    ) %>%
    rename(
      track.id = id,
      track_name = name,
      track_popularity = popularity
    )
  
  # M-M mapping for artist - track
  artists_tracks <- 
    select(track, track.id, artists) %>%
    unnest_longer(artists) %>%
    transmute(
      track.id = track.id,
      artist.id = artists$id
    )
    
  track <- select(track, !artists)
  
  track_features <- 
    call_by_chunks(t, get_track_audio_features, access_token, 100) %>%
    select(
      id, loudness, tempo, time_signature, key, mode, acousticness, 
      danceability, energy, speechiness, instrumentalness, liveness, valence
    ) %>%
    rename(track.id = id)
  
  # Get album details
  album <- 
    distinct(track, album.id) %>%
    call_by_chunks(get_albums, access_token, 20) %>%
    select(
      id, external_ids.upc, label, name, popularity
    ) %>%
    rename(
      album.id = id,
      album_name = name,
      album_popularity = popularity
    )

  # Get all artist details
  artist <- 
    distinct(artists_tracks, artist.id) %>%
    call_by_chunks(get_artists, access_token) %>%
    select(id, name, popularity, followers.total, genres) %>%
    rename(
      artist.id = id,
      followers = followers.total
    )
  
  # Create M-M artist genre mapping
  artists_genres <- select(artist, artist.id, genres) %>%
    unnest_longer(genres)
  
  # Remove genre column from artist
  artist <- select(artist, !genres)
  
  # For user display names
  user <- 
    distinct(plays, username) %>%
    call_by_chunks(get_user_profile, access_token, 1) %>%
    select(id, display_name, any_of('images'))
  
}


# Save output and cleanup
save(
  plays, track, track_features, album, artist, artists_tracks, artists_genres,
  user, file = db_path
)

remove(
  t, access_token, db_path, force_refresh, retain_cols, call_by_chunks, 
  current_plays
)

message('Data Loaded.')


