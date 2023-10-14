# This holds the last year of dates from the last date
date_df <- 
  data.frame(
      date = seq.Date(
        max(date(plays$ts)) - years(1), max(date(plays$ts)), by = 'day')
  ) %>%
  mutate(
    weekday = wday(date, label = TRUE),
    week_start = floor_date(date, "week")
  )


# Now add the actual play count and time by day
plays_by_date <- plays %>%
  mutate(date = date(ts)) %>%
  right_join(date_df, by = 'date') %>%
  group_by(week_start, weekday) %>%
  summarise(
    tracks_played = sum(!is.na(track.id)),
    hours_played = coalesce(round(sum(ms_played) / 3600000, 2), 0.0),
    .groups = 'drop'
  ) 

# For our calendar, we want 5 discrete colours in hours_played. The first will
# be for zero, the remaining 4 should be sensible 

tile_cols <- c('#161b22', '#0e4429', '#006d32', '#26a641', '#39d353')

bg_col <- '#0d1117'
tile_bin <- quantile(by_date$hours_played, c(0, 0.2, 0.5, 0.7, 0.95))

skip_2 <- function(x) x[seq_along(x) %% 2 == 0]

# Next lets construct the plot
git_calendar <- 
  ggplot(by_date, aes(x = week_start, y = fct_rev(weekday), fill = hours_played)) +
  geom_tile(colour = bg_col, linewidth = 1.5) +
  
  scale_x_date(
    expand = expansion(add = 2), 
    position = 'top', 
    date_breaks = "1 month",
    date_labels = '%b') +
  scale_y_discrete(breaks = skip_2) +
  
  scale_fill_stepsn(
    colours = tile_cols, 
    breaks = tile_bin,
    values = scales::rescale(tile_bin),
    guide = 'legend',
    right = FALSE
  ) +
  
  annotate(
    'text', 
    x = c(max(by_date$week_start)-weeks(9), max(by_date$week_start)-weeks(2)),
           label = c('Less', 'More'),
           y = -0.3,  colour = '#7d8590',
           hjust = 1, size = 3) +
  coord_cartesian(ylim = c(1, 7), clip = "off") +
  
  theme(
    axis.text = element_text(colour = '#e6edf3'),
    axis.text.y = element_text(hjust = 0, margin = margin()),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    
    legend.background = element_blank(),
    legend.box.spacing = unit(1, 'mm'),
    legend.direction = 'horizontal',
    legend.justification = "right",
    legend.key = element_blank(),
    legend.key.size = unit(5, 'mm'),
    legend.margin = margin(r = 16.75, unit = 'mm'),
    legend.position = 'bottom',
    legend.spacing.x = unit(-0.5, 'mm'),
    legend.text = element_blank(),
    legend.title = element_blank(),
    
    plot.background = element_rect(fill = bg_col),
    panel.background = element_blank(),
    panel.grid = element_blank()
  )


save(git_calendar, by_date, file = 'data/git_calendar.RData')
