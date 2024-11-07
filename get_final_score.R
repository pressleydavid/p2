library(tidyverse)
library(shiny)
library(fst)
library(hms)
library(bslib)
library(vroom)
library(ggplot2)

# Data loading code same as before
if (!exists("nfl_raw")) {
  if (file.exists("nfl_raw.fst")) {
    print("Reading from fst file")
    nfl_raw <- read_fst("nfl_raw.fst")
  } else {
    print("Reading from csv file with vroom")
      nfl_raw <- vroom("archive/NFL Play by Play 2009-2018 (v5).csv",
                 delim = ",",
                 col_types = cols(
                   play_id = col_integer(),
                   game_id = col_character(),
                   posteam = col_character(),
                   defteam = col_character(),
                   qtr = col_integer(),
                   quarter_seconds_remaining = col_integer(),
                   sp = col_logical(),
                   down = col_integer(),
                   time = col_time(format = "%M:%S"),
                   ydsnet = col_double(),
                   play_type = col_character(),
                   score_differential = col_double(),
                   yards_gained = col_double(),
                   home_team = col_character(),
                   away_team = col_character(),
                   field_goal_result = col_character(),
                   field_goal_attempt = col_logical(),
                   defteam_score_post = col_integer(),
                   posteam_score_post = col_integer(),
                   total_home_score = col_integer(),
                   total_away_score = col_integer()
                 )) |>
  select(play_id, game_id, posteam, defteam, home_team, away_team, qtr, quarter_seconds_remaining,
         sp, down, time, ydsnet, play_type, score_differential, yards_gained, field_goal_result,
         field_goal_attempt, defteam_score_post, posteam_score_post, total_home_score, total_away_score)
      write_fst(nfl_raw, "nfl_raw.fst")
  }
}

#DONE
final_score <- nfl_raw |>
  group_by(game_id) |>
  select(play_id,game_id, home_team, away_team, qtr, time, quarter_seconds_remaining,
         total_home_score, total_away_score) |>
  slice_max(play_id, n = 1, with_ties = FALSE) |>
  mutate(winner = case_when(
    total_home_score > total_away_score ~ home_team,
    total_away_score > total_home_score ~ away_team,
    TRUE ~ "Tie"
  )) |>
  mutate(homeaway_win = case_when(
    total_home_score > total_away_score ~ "Home",
    total_away_score > total_home_score ~ "Away",
    TRUE ~ "Tie"
  )) |>
  mutate(homeaway_win_num = case_when(
    total_home_score > total_away_score ~ 1,
    total_away_score > total_home_score ~ 2,
    TRUE ~ 0
  ))
  select(-qtr, -time, -quarter_seconds_remaining)

team_win_pct <- final_score |>
  group_by(winner) |>
  count(homeaway_win) |>
  group_by(winner) |>
  mutate(percentage = n/sum(n) * 100)

team_homeaway_wins <- final_score |>
  group_by(winner, homeaway_win) |>
  summarize(wins = n(), .groups = "drop") |>
  arrange(winner, desc(wins))

#Pie chart
ggplot(team_win_percentages, aes(x = "", y = percentage, fill = homeaway_win)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~winner) +
  scale_fill_manual(values = c("Home" = "lightblue", "Away" = "pink")) +
  labs(
    title = "Home vs Away Win Distribution by Team",
    fill = "Win Location"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 8),
    legend.position = "bottom"
  ) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5))

#Bar chart
ggplot(team_homeaway_wins, aes(x = winner, y = wins, fill = homeaway_win)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Home and Away Wins by Team",
    x = "Team",
    y = "Number of Wins",
    fill = "Win Location"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


no_ties <- team_homeaway_wins |>
  filter(winner != "Tie")

ggplot(team_homeaway_wins, aes(x = wins, fill = homeaway_win, color = homeaway_win)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Distribution of Home and Away Wins Across Teams",
    x = "Number of Wins",
    y = "Density",
    fill = "Win Location",
    color = "Win Location"
  ) +
  theme_minimal()

team_wins <- final_score %>%
  group_by(winner, homeaway_win) %>%
  summarise(wins = n(), .groups = 'drop') %>%
  pivot_wider(names_from = homeaway_win, values_from = wins, values_fill = 0) %>%
  rename(home_team_wins = Home, away_team_wins = Away)

# Scatterplot of team wins for home vs away
ggplot(team_wins, aes(x = home_team_wins, y = away_team_wins)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Scatterplot of Team Wins: Home vs Away",
    x = "Home Wins",
    y = "Away Wins"
  ) +
  theme_minimal()



final_score |>
  ggplot(aes())

#TESTING
dupes <- final_score |>
  group_by(game_id) |>
  filter(n() > 1 ) |>
  ungroup()
dupes

game_id_2009091300 <- nfl_raw |>
  select(play_id,game_id, home_team, away_team, qtr, time, total_home_score, total_away_score) |>
  filter(game_id == "2009091300")
