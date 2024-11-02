library(tidyverse)
library(shiny)
library(fst)
library(vroom)

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
                       away_team = col_character()
                     )) |>
      select(play_id, game_id, posteam, defteam, home_team, away_team, qtr, quarter_seconds_remaining,
             sp, down, time, ydsnet, play_type, score_differential, yards_gained)
    write_fst(nfl_raw, "nfl_raw.fst")
  }
}

