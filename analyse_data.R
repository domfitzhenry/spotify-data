library(tidyverse)
library(quarto)

# Parameters

report_year <- 2023

# This file reads the data, and potentially pulls a lot of data from Spotify

# Set this to true to call Spotify API
force_refresh <- FALSE


if (force_refresh) {
  source('get_spotify_track_data.R')
} else {
  load('data/spotify_play_history.RData')
  load('data/spotify_data.RData')
}


# Modify plays to include flags for whether the track was played (arbitrarily 
# this is 10% of the song), and if it was played in the current_year

plays <- mutate(
  plays,
  is_played = duration_played / 
    track[match(track.id, track$track.id), 'duration']>= 0.1,
  is_current = year(track_start) == report_year
)

for(user_id in user$id) {
  
  current_user <- filter(user, id == user_id)


  user_plays <- plays %>%
    filter(username == user_id) %>%
    inner_join(track, by = 'track.id') %>%
    inner_join(album, by = 'album.id', suffix = c('_track', '_album')) %>%
    left_join(track_features, by = 'track.id')
  
  
  
  date_df <- 
    data.frame(
      date = seq.Date(
        ymd(paste0(report_year, '01', '01')), 
        ymd(paste0(report_year, '12', '31')), 
        by = 'day')
    ) %>%
    mutate(
      weekday = wday(date, label = TRUE),
      week_start = floor_date(date, "week")
    )
  
  
  
  
  # Group the plays into sessions and provide some summary
  
  play_sessions <- user_plays %>%
    arrange(track_start) %>%
    mutate(
      # Treat breaks of 5 mins or less as the same session
      is_session_start = coalesce(track_start > lag(track_end + dminutes(5)), 
                                  TRUE),
      session_id = cumsum(is_session_start)
    ) %>%
    group_by(session_id, is_current) %>%
    summarise(
      session_start = min(track_start),
      session_end = max(track_end),
      session_length = as.duration(session_end - session_start),
      tracks_played = sum(is_played),
      .groups = 'drop'
    ) %>%
    filter(tracks_played >= 1)
  
  
  
  # Summary numbers ----
  
  play_summary <- list()
  
  play_summary['total_streams'] <- 
    sum(user_plays$is_played) %>% 
    format(big.mark = ',')
  
  play_summary['current_streams'] <- 
    sum(user_plays$is_played * user_plays$is_current) %>%
    format(big.mark = ',')
  
  
  play_summary['total_tracks'] <- 
    filter(user_plays, is_played) %>% 
    distinct(isrc) %>%
    nrow() %>%
    format(big.mark = ',')
  
  play_summary['current_tracks'] <- 
    filter(user_plays, is_played, is_current) %>% 
    distinct(isrc) %>%
    nrow() %>%
    format(big.mark = ',')
  
  
  play_summary['total_artists'] <- 
    filter(user_plays, is_played) %>% 
    unnest_longer(artist.id) %>%
    distinct(artist.id) %>%
    nrow() %>%
    format(big.mark = ',')
  
  play_summary['current_artists'] <- 
    filter(user_plays, is_played, is_current) %>% 
    unnest_longer(artist.id) %>%
    distinct(artist.id) %>%
    nrow() %>%
    format(big.mark = ',')
  
  
  play_summary['total_albums'] <- 
    filter(user_plays, is_played) %>% 
    distinct(album.id) %>%
    nrow() %>%
    format(big.mark = ',')
  
  play_summary['current_albums'] <- 
    filter(user_plays, is_played, is_current) %>% 
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
    slice_max(order_by = session_length, with_ties = FALSE)
  
  play_summary['current_session_max_date'] <- 
    format(session_max$session_start, format = '%d %B at %I:%M %p')
  
  play_summary['current_session_max_tracks'] <- session_max$tracks_played
  
  play_summary['current_session_max_duration'] <- session_max$session_length
  
  
  
  # By Date ----
  plays_by_date <-
    filter(user_plays, is_current) %>% 
    mutate(date = date(track_start)) %>%
    right_join(date_df, by = 'date') %>%
    group_by(week_start, weekday) %>%
    summarise(
      hours_played = coalesce(round(sum(duration_played) / 3600, 2), 0.0),
      .groups = 'drop'
    )
  
  
  
  plays_by_hour <- 
    filter(user_plays, is_current) %>% 
    mutate(
      hour_start = map2(
        floor_date(track_start, unit = 'hours'),
        floor_date(track_end, unit = 'hours'),
        seq.POSIXt, by = 'hours'
        )
      ) %>%
    unnest(cols = c('hour_start')) %>%
    mutate(
      hour_end = hour_start + dhours(1),
      duration_in_hour = 
        pmin(hour_end, track_end) - pmax(hour_start, track_start),
      hour = as.factor(hour(hour_start))
      ) %>%
    group_by(hour) %>%
    summarise(
      duration = sum(duration_in_hour)
     ) %>%
    mutate(
      norm_duration = seconds(duration) / seconds(max(duration))
    )
  
  
  # Ordered Tables ----
  
  ### Tracks ----
  
  # The same track might appear on multiple albums, in which case it will have
  # different spotify track ids - the ISRC is the unique identifier for a track.
  
  # For the purposes of top tracks, it doesn't really matter which track.id is
  # linked, it is more important for the track to be unique in the list.
  
  top_tracks <-
    filter(user_plays, is_played, !is.na(isrc)) %>%
    group_by(isrc) %>%
    summarise(
      track.id = first(track.id),
      all_plays = n(),
      current_plays = sum(is_current),
      old_plays = all_plays - current_plays,
      first_listen = min(track_start)
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
      rank_diff = if_else(old_plays > 0, old_rank - current_rank, NA)
      ) %>%
    arrange(current_rank) %>%
    select(
      track.id, current_rank, rank_diff, track_name, artist_names, first_listen, 
      current_plays, all_plays, track_popularity, track_url
    )
  
  
  ### Artists ----
  top_artists <- 
    filter(user_plays, is_played) %>%
    # We'll credit the song only to the first listed artist, as it might cause
    # obscure collaborations to appear high in the list.
    mutate(artist.id = map_chr(artist.id, 1)) %>%
    group_by(artist.id) %>%
    summarise(
      all_plays = n(),
      current_plays = sum(is_current),
      old_plays = all_plays - current_plays,
      first_listen = min(track_start),
      distinct_tracks = n_distinct(track.id)
      ) %>%
    filter(all_plays >= 5) %>%
    inner_join(artist, by = 'artist.id') %>%
    mutate(
      old_rank = min_rank(desc(old_plays)),
      current_rank = min_rank(desc(current_plays)),
      rank_diff = if_else(old_plays > 0, old_rank - current_rank, NA),
      ) %>%
    arrange(current_rank) %>%
    select(
      artist.id, current_rank, rank_diff, artist_name, first_listen, 
      distinct_tracks, current_plays, all_plays, artist_popularity, followers, 
      artist_url, artist_image_url
    )
  
  
  ### Albums ----
  top_albums <- 
    filter(user_plays, is_played) %>%
    group_by(album.id) %>%
    summarise(
      all_plays = n(),
      current_plays = sum(is_current),
      old_plays = all_plays - current_plays,
      first_listen = min(track_start),
      distinct_tracks = n_distinct(track.id),
      .groups = 'drop'
      ) %>%
    filter(all_plays >= 5) %>%
    inner_join(album, by = 'album.id') %>%
    mutate(
      old_rank = min_rank(desc(old_plays)),
      current_rank = min_rank(desc(current_plays)),
      rank_diff = if_else(old_plays > 0, old_rank - current_rank, NA),
      release_year = year(release_date)
      ) %>%
    arrange(current_rank) %>%
    select(
      album.id, current_rank, rank_diff, album_name, artist_names, first_listen, 
      distinct_tracks, current_plays, all_plays, album_popularity, release_year, 
      album_url, album_image_url
    )
  
  
  ## Genres ----
  
  # Genres are associated with an artist, rather than a song or an album, so 
  # start with the top artist list and aggregate the existing summary numbers. 
  
  genre_list <- top_artists %>%
    slice_max(order_by = current_plays, n = 200) %>%
    select(artist.id, distinct_tracks) %>%
    inner_join(artist, by = 'artist.id') %>%
    unnest_longer(genres) %>%
    group_by(genres) %>%
    summarise(
      distinct_artists = n_distinct(artist_name),
      .groups = 'drop'
    ) %>%
    # This genre filter is because I disagree with the associations in my own
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
  # Keeping only the ones with no subgenres, summarise including top artists
  
  
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
    filter(is_current, is_played) %>%
    mutate(month_played = month(track_start, label = TRUE)) %>%
    select(track.id, valence, danceability, energy, month_played)
  
  
  # Songs to select from for the highest and lowest measure songs
  top_track_features <- track_features %>%
    inner_join(top_tracks, by = 'track.id') %>%
    filter(current_plays > 0) %>%
    mutate(
      artist_track = paste(artist_names, '-', track_name),
      hyperlink = track_url
      )
    
  
  
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
    current_user, play_summary, plays_by_date, plays_by_hour, top_tracks, 
    top_artists, top_albums, genre_map, user_track_features,
    top_track_features, suggested_artists,
    file = 'data/user_data.RData'
  )
  
  
  
  quarto_render(
    'spotify_user_report.qmd',
    output_file = paste0('spotify_report_', current_user$display_name, '.html')
  )

}