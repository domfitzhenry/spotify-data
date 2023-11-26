library(tidyverse)

# This file reads the data, and potentially pulls a lot of data from Spotify

# Set this to true to call Spotify API
force_refresh <- FALSE

if (force_refresh) source('get_spotify_track_data.R')


load('data/spotify_play_history.RData')


# Generate datasets for the user that will be loaded and referenced in the qmd

# User ----
u <- filter(user, display_name == 'domotron') %>% pull(id)

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

# The same track might appear on multiple albums, in which case it will have
# different spotify track ids - the ISRC is the unique identifier for a track.

# For the purposes of top tracks, it doesn't really matter which track.id is
# linked, it is more important for the track to be unique in the list.

top_tracks <-
  filter(user_plays, track_played, !is.na(external_ids.isrc)) %>%
  group_by(external_ids.isrc) %>%
  summarise(
    track.id = first(track.id),
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
    track.id, current_rank, rank_diff, track_name, artist_names, first_listen, 
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
    artist.id, current_rank, rank_diff, artist_name, first_listen, 
    distinct_tracks, current_plays, all_plays, artist_popularity, followers, 
    artist_url
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
    album.id, current_rank, rank_diff, album_name, artist_names, first_listen, 
    distinct_tracks, current_plays, all_plays, album_popularity, release_year, 
    album_url
  )







## Genres ----

# Genres are associated with an artist, rather than a song or an album, so start
# with the top artist list and aggregate the existing summary numbers. 

top_genres <- 
  top_artists %>%
  select(artist.id, first_listen, distinct_tracks, current_plays, all_plays) %>%
  inner_join(artist, by = 'artist.id') %>%
  select(artist_name, genres, first_listen:all_plays) %>%
  unnest_longer(genres) %>%
  group_by(genres) %>%
  summarise(
    all_plays = sum(all_plays),
    current_plays = sum(current_plays),
    old_plays = all_plays - current_plays,
    first_listen = min(first_listen),
    distinct_artists = n_distinct(artist_name),
    .groups = 'drop'
  ) %>%
  # This genre filter is because I disagree with the associations in my personal 
  # listening history, and I have the power to make it disappear.
  filter(all_plays >= 5, genres != 'nu metal') %>%
  mutate(
    old_rank = min_rank(desc(old_plays)),
    current_rank = min_rank(desc(current_plays)),
    rank_diff = if_else(old_plays > 0, old_rank - current_rank, NA)
  ) %>%
  arrange(current_rank) %>%
  select(
    current_rank, rank_diff, genres, first_listen, distinct_artists, 
    current_plays, all_plays
  )





### Genre Hierarchy ----

# Assume that a broader genre will be a shorter string contained within a broad
# genre - e.g. rock -> alternative rock -> soft alternative rock. 

# Find the number of words in a genre, start with single words, and then search
# for broader genres. Treat anything non-alpha as a break.


genre_breaks <- top_genres %>%
  select(genres) %>%
  mutate(n_breaks = str_count(genres, '[^[:alpha:]]')) %>%
  arrange(n_breaks)


# For each genre that has n breaks, match it with all genres containing it as a
# substring that have n + 1 breaks. The problem with this approach is there 
# might be narrow genres that map to more than one broad genre - e.g. should 
# "funk rock" map to "funk" or "rock"?

# To avoid this, only match at the end of the narrow genre. This is good as it
# removes partial word matches which we don't want in most cases, and we assume
# earlier words are more like an adjective.

# Also note we are losing any genres with no parents or descendants in this step

genre_map <- genre_breaks %>%
  mutate(next_break = n_breaks + 1L) %>%
  inner_join(
    genre_breaks, by = c("next_break" = "n_breaks"), 
    suffix = c('_broad', '_narrow'), relationship = "many-to-many"
  ) %>%
  filter(str_ends(genres_narrow, fixed(genres_broad)))



# And there might be some narrow genres that don't map back to a genre 
# containing n-1 breaks - e.g. "hip hop" doesn't have a corresponding "hop"
# It seems reasonable to bring these up to the top level.

genre_map <- genre_map %>%
  mutate(
    n_breaks = if_else(
      genres_broad %in% genre_map$genres_narrow,
      n_breaks,
      0
    )
  )



# Add narrow genres for our top level genres that will link to a root node.
# At this point we can also reduce the dataset to the values we will use in the
# visualisation.

genre_map <- genre_map %>%
  ungroup() %>%
  filter(n_breaks == 0) %>%
  distinct(genres_broad) %>%
  rename(
    genres_narrow = genres_broad
  ) %>%
  mutate(
    genres_broad = 'genres',
    n_breaks = -1L,
    next_break = 0L,
    .before = genres_narrow
  ) %>%
  rbind(genre_map) %>%
  mutate(
    genre = genres_narrow,
    label = str_to_title(genres_narrow),
    parent = genres_broad,
    .keep = 'none'
  )



# Values will determine the size of each genre, and the sunburst viz will use
# the default 'remainder' branchvalues. This means we need a count of artists
# for each genre where the artist is *not* also associated with a subgenre.

# For each artist, count how many times each genre also has subgenres.
# Keeping only the ones with no subgenres, summarise the counts and top artists
 
 
artist_genre <- top_artists %>%
  inner_join(artist, by = c('artist.id', 'artist_name')) %>%
  select(artist_name, all_plays, genres) %>%
  unnest_longer(genres) %>%
  group_by(artist_name, all_plays)


genre_counts <- artist_genre %>%
  inner_join(
    artist_genre, by = c('artist_name', 'all_plays'), 
    suffix = c('_broad', '_narrow'), relationship = 'many-to-many') %>%
  mutate(
    ends = (str_ends(genres_narrow, fixed(genres_broad)) & 
              genres_narrow != genres_broad)
    ) %>%
  group_by(artist_name, all_plays, genres_broad) %>%
  filter(sum(ends) == 0) %>%
  group_by(genres_broad) %>%
  summarise(
    value = n(),
    top_artist = paste(
      'e.g.',str_flatten_comma(head(unique(artist_name), n =3L))
    )
  )
  

# Now add this to our genre map along with a root node

genre_map <- genre_map %>%
  left_join(genre_counts, by = c('genre' = 'genres_broad')) %>%
  replace_na(list('value' = 0L, 'top_artist' = '-')) %>%
  rbind(
    data.frame(
      genre = 'genres', 
      label = '', 
      parent = '', 
      value = 0L,
      top_artist = ''
    )
  )


save(
  top_tracks, top_artists, top_albums, top_genres, genre_map,
  file = 'data/top_things.RData'
)








# Colours ----
# Temporarily adding here while playing with plots
bg_col <- '#060606'
faint_col <- '#161b22'

txt_col <- '#e6edf3'
txt_light_col <- '#adafae'

spotify_green <- '#1DB954'





## Track audio features ----


# Loudness is the average dB across the track in a typical range from -60 to 0
# so is best used to compare relatively and the actual value is less important.
# user_plays %>%
#   distinct(track.id, loudness) %>%
#   ggplot(aes(x = loudness)) +
#   geom_density()
# 
# # Tempo is BPM
# user_plays %>%
#   distinct(track.id, tempo) %>%
#   ggplot(aes(x = tempo)) +
#   geom_density()


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

