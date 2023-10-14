

date_df <- 
  data.frame(date = seq.Date(last_date - years(1), last_date, by = 'day')) %>%
  mutate(
    weekday = wday(date, label = TRUE),
    week_start = floor_date(date, "week")
  )


# Now add the actual play count and time
by_date <- plays %>%
  mutate(date = date(ts)) %>%
  right_join(date_df, by = 'date') %>%
  group_by(week_start, weekday) %>%
  summarise(
    tracks_played = sum(!is.na(track.id)),
    hours_played = coalesce(round(sum(ms_played) / 3600000, 2), 0.0),
    .groups = 'drop'
  ) 



skip_3 <- function(x) x[seq_along(x) %% 3 == 0]

plays_by_hour %>%
  ggplot(aes(x = as.factor(hour), y = avg_pct_played)) +
  geom_bar(
    stat = 'identity', width = 0.9,
    colour = bg_col, fill = '#1DB954'
  ) +
  coord_polar(start = -0.65) +
  scale_y_continuous(breaks = NULL, limits = c(-0.3, 1)) +
  scale_x_discrete(labels = str_pad(seq(0,23,3), 2, pad = '0'), breaks = skip_3) +
  theme(
    axis.text.x = element_text(colour = txt_col),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    
    plot.background = element_rect(colour = bg_col, fill = bg_col),
    
    panel.background = element_rect(colour = bg_col, fill = bg_col),
    panel.grid.major.x  = element_line(colour = faint_col),
    panel.grid.major.y = element_blank()
  )
