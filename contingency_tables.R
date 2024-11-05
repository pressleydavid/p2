source("load_data.R")

# One-way table of play types
play_type_table <- table(nfl_raw$play_type)
prop.table(play_type_table)

# Two-way table of play type by down
play_down_table <- table(nfl_raw$play_type, nfl_raw$down)
prop.table(play_down_table, margin = 2) # proportions by down


# Group by play type and get EPA summaries

epa_by_play <- nfl_raw |>
  group_by(play_type) |>
  summarize(
    mean_epa = mean(epa, na.rm = TRUE),
    median_epa = median(epa, na.rm = TRUE),
    sd_epa = sd(epa, na.rm = TRUE),
    n = n()
  )

# Yards gained by down and play type
yards_summary <- nfl_raw |>
  group_by(down, play_type) |>
  summarize(
    mean_yards = mean(yards_gained, na.rm = TRUE),
    median_yards = median(yards_gained, na.rm = TRUE),
    sd_yards = sd(yards_gained, na.rm = TRUE)
  )
