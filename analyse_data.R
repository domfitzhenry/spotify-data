library(tidyverse)

# This file reads the data, and potentially pulls a lot of data from Spotify

# Set this to true to call Spotify API
force_refresh <- FALSE

if (force_refresh) source('get_spotify_track_data.R')


load('data/spotify_play_history.RData')
load('data/spotify_data.RData')

# Generate datasets for the user that will be loaded and referenced in the qmd

# User ----
u <- head(user, n = 1) %>% pull(id)

username <- filter(user, id == u) %>% pull(display_name)

# This will hold a filtered and transformed plays dataset used for the remaining
# analysis. When counting the number of plays of a track, we will exclude any
# that didn't get played for very long - arbitrarily 10% of the track length.

# We'll also convert our categorical integers into factors.
# Key is in pitch class notation

pcn <- c('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B')


user_plays <- plays %>%
  filter(username == u) %>%
  inner_join(track, by = 'track.id') %>%
  inner_join(album, by = 'album.id', suffix = c('_track', '_album')) %>%
  left_join(track_features, by = 'track.id') %>%
  mutate(
    track_played = duration_played / duration >= 0.1,
    
    # Time signature is an estimate of the meter / 4, values range from 3-7
    # other values will be forced to NA.
    time_signature = 
      factor(str_c(time_signature, "/4"), levels = str_c(seq(3, 7), "/4")),
    
    key = factor(key, levels = seq(0, 11), labels = pcn),
    
    # Mode: 0 = minor, 1 = major
    mode = factor(mode, levels = c(0, 1), labels = c('minor', 'major'))
  )

last_date <- max(date(user_plays$ts))
first_date <- floor_date(last_date, 'year')

# Determine if the plays are in the current year or older
user_plays <- mutate(user_plays, is_current = first_date <= ts,)



# Group the plays into sessions and provide some summary

play_sessions <- user_plays %>%
  select(ts, duration_played, track_played, is_current) %>%
  arrange(ts) %>%
  mutate(
    i = interval(
      start = ts - duration_played,
      # Add extra time to allow for pauses between songs to still count as a
      # single session.
      end = ts + dminutes(5)
    ),
    is_session_start = coalesce(!int_overlaps(i, lag(i)), TRUE),
    session_id = cumsum(is_session_start)
  ) %>%
  group_by(session_id, is_current) %>%
  summarise(
    session_start = min(ts - duration_played),
    session_end = max(ts),
    session_length = as.duration(session_end - session_start),
    tracks_played = sum(track_played),
    .groups = 'drop'
  ) %>%
  filter(tracks_played >= 1)



# Summary numbers ----

play_summary <- list()

play_summary['total_streams'] <- 
  filter(user_plays, track_played) %>% 
  nrow() %>%
  format(big.mark = ',')

play_summary['current_streams'] <- 
  filter(user_plays, track_played, is_current) %>% 
  nrow() %>%
  format(big.mark = ',')


play_summary['total_tracks'] <- 
  filter(user_plays, track_played) %>% 
  distinct(external_ids.isrc) %>%
  nrow() %>%
  format(big.mark = ',')

play_summary['current_tracks'] <- 
  filter(user_plays, track_played, is_current) %>% 
  distinct(external_ids.isrc) %>%
  nrow() %>%
  format(big.mark = ',')


play_summary['total_artists'] <- 
  filter(user_plays, track_played) %>% 
  unnest_longer(artist.id) %>%
  distinct(artist.id) %>%
  nrow() %>%
  format(big.mark = ',')

play_summary['current_artists'] <- 
  filter(user_plays, track_played, is_current) %>% 
  unnest_longer(artist.id) %>%
  distinct(artist.id) %>%
  nrow() %>%
  format(big.mark = ',')


play_summary['total_albums'] <- 
  filter(user_plays, track_played) %>% 
  distinct(album.id) %>%
  nrow() %>%
  format(big.mark = ',')

play_summary['current_albums'] <- 
  filter(user_plays, track_played, is_current) %>% 
  distinct(album.id) %>%
  nrow() %>%
  format(big.mark = ',')


play_summary['total_sessions'] <- 
  nrow(play_sessions) %>%
  format(big.mark = ',')

play_summary['current_sessions'] <- 
  filter(play_sessions, is_current) %>%
  nrow() %>%
  format(big.mark = ',')

play_summary['total_session_track_avg'] <- 
  median(play_sessions$tracks_played)

play_summary['current_session_track_avg'] <- 
  filter(play_sessions, is_current) %>%
  pull(tracks_played) %>%
  median()


## Total Duration (don't need to format or filter by played)

play_summary['total_duration'] <- sum(user_plays$duration_played)

play_summary['current_duration'] <- 
  filter(user_plays, is_current) %>% 
  pull(duration_played) %>%
  sum()

play_summary['total_session_duration_avg'] <- 
  median(play_sessions$session_length)

play_summary['current_session_duration_avg'] <- 
  filter(play_sessions, is_current) %>%
  pull(session_length) %>%
  median()


session_max <- filter(play_sessions, is_current) %>%
  slice_max(order_by = session_length)

play_summary['current_session_max_date'] <- 
  format(session_max$session_start, format = '%d %B at %I:%M %p')

play_summary['current_session_max_tracks'] <- session_max$tracks_played

play_summary['current_session_max_duration'] <- session_max$session_length



-# Date transforms ----

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
  last_date, first_date, play_summary, plays_by_date, plays_by_hour, username,
  file = 'data/summary_date_data.RData'
)



# What we listened to ----


## Ordered Tables ----

### Tracks ----

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
    track_url = paste0('https://open.spotify.com/track/', track.id),
    ) %>%
  arrange(current_rank) %>%
  select(
    track.id, current_rank, rank_diff, track_name, artist_names, first_listen, 
    current_plays, all_plays, track_popularity, track_url
  )


### Artists ----
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
    artist_url = paste0('https://open.spotify.com/artist/', artist.id),
    ) %>%
  arrange(current_rank) %>%
  select(
    artist.id, current_rank, rank_diff, artist_name, first_listen, 
    distinct_tracks, current_plays, all_plays, artist_popularity, followers, 
    artist_url
  )


### Albums ----
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
    album_url = paste0('https://open.spotify.com/album/', album.id),
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

genre_list <- top_artists %>%
  slice_max(order_by = current_plays(desc), n = 200) %>%
  select(artist.id, distinct_tracks) %>%
  inner_join(artist, by = 'artist.id') %>%
  unnest_longer(genres) %>%
  group_by(genres) %>%
  summarise(
    distinct_artists = n_distinct(artist_name),
    .groups = 'drop'
  ) %>%
  # This genre filter is because I disagree with the associations in my personal 
  # listening history, and I have the power to make it disappear.
  filter(distinct_artists >= 3, genres != 'nu metal') %>%
  select(genres) %>%
  mutate(
    clean_genre = str_squish(str_replace_all(genres, '[\\s\\-]', ' '))
  )



### Genre Hierarchy ----

# Assume that a broader genre will be a shorter string contained within a broad
# genre - e.g. rock -> alternative rock -> soft alternative rock. 

# There are some nodes we don't want to treat as atomic, remove them so they
# get bundled with the next node - e.g. no such thing as "hop" in "hip hop"

stop_nodes <-
  c('fi', 'hop', 'revival', 'music', 'cover', 'trip', 'nova', 'bass', 'roll')


genre_paths <- genre_list %>%
  mutate(
    node = lapply(str_split(clean_genre, ' '), rev)
  ) %>%
  unnest_longer(node, indices_to = 'level') %>%
  group_by(genres) %>%
  mutate(
    path = accumulate(node, ~ paste(.y, .x)),
    inferred = (!path %in% genre_list$clean_genre)
  ) %>%
  filter(!(node %in% stop_nodes & inferred)) %>%
  # Now to determine if we keep inferred genres, the approach will be:
  #   If the earliest actual genre is the leaf, keep the inferred root.
  #   Discard all (other) inferred genres.
  mutate(
    only_leaf = min(if_else(inferred, 99, level)) == max(level)
  ) %>%
  filter(!inferred | (level == 1 & only_leaf)) %>%
  # NOTE! This does not guarantee distinct paths!
  # This will be handled when we build the genre map
  mutate(
    level = row_number()
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
  inner_join(genre_paths, by = 'genres', relationship = 'many-to-many') %>%
  select(artist_name, genres, clean_genre, level, path, all_plays) %>%
  group_by(artist_name, path) %>%
  filter(n() == 1, clean_genre == path) %>%
  group_by(path) %>%
  arrange(desc(all_plays)) %>%
  summarise(
    value = sum(all_plays),#n(),
    top_artist = paste(
      'e.g.',str_flatten_comma(head(artist_name, n =3L))
    )
  )


genre_map <- genre_paths %>%
  left_join(artist_genre, by = 'path') %>%
  mutate(
    genre = path,
    label = str_to_title(path),
    parent = coalesce(lag(path), 'root'),
    value = coalesce(value, 0L),
    top_artist = coalesce(top_artist, ''),
    .keep = 'none'
  ) %>%
  group_by(genre) %>%
  # Where a genre links to both the root node and a different parent, discard 
  # the link to the root.
  filter(!(n_distinct(parent) > 1 & parent == 'root')) %>%
  ungroup() %>%
  select(genre, label, parent, value, top_artist) %>%
  distinct() %>%
  bind_rows(
    data.frame(
      genre = 'root', 
      label = '', 
      parent = '', 
      value = 0L,
      top_artist = ''
    )
  )




## Track audio features ----

user_track_features <- user_plays %>%
  filter(is_current, track_played) %>%
  mutate(month_played = month(ts, label = TRUE)) %>%
  select(track.id, valence, danceability, energy, month_played)


## Suggested Artists ----
suggested_artists <- related_artist %>%
  filter(!related_artist_id %in% top_artists$artist.id) %>%
  inner_join(top_artists, by = 'artist.id') %>%
  filter(current_rank <= 100) %>%
  group_by(related_artist_name, related_artist_url) %>%
  summarise(
    n_artists = n(),
    score = median(all_plays) * max(log10(related_artist_followers)),
    related = paste(str_flatten_comma(artist_name)),
    .groups = 'drop'
  ) %>%
  filter(n_artists >= 3) %>%
  slice_max(order_by = score, n = 10L) %>%
  select(related_artist_name, related_artist_url, related)



save(
  top_tracks, top_artists, top_albums, genre_map, user_track_features,
  suggested_artists,
  file = 'data/top_things.RData'
)






# Colours ----
# Temporarily adding here while playing with plots
bg_col <- '#060606'
faint_col <- '#161b22'

txt_col <- '#e6edf3'
txt_light_col <- '#adafae'

spotify_green <- '#1DB954'





# Key is in pitch class notation, -1 indicates not detected.



# Mode: 0 = minor, 1 = major







# Suggested Artists ---





