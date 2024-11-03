library(tidyverse)
library(shiny)
library(fst)
library(hms)
library(bslib)
library(vroom)

# Data loading code remains the same
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

ui <- fluidPage(
  titlePanel("NFL Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "home_team",
        label = "Select Home Team",
        choices = c("All Teams", sort(na.omit(unique(nfl_raw$home_team)))),
        selected = "All Teams"
      ),
      selectInput(
        inputId = "away_team",
        label = "Select Away Team",
        choices = c("All Teams", sort(na.omit(unique(nfl_raw$away_team)))),
        selected = "All Teams"
      ),
      actionButton("submit", "Subset Data")
    ),
    mainPanel(
      tableOutput("qtr_table")
    )
  )
)

server <- function(input, output, session) {

  # Store the filtered data in a reactive value
  v <- reactiveValues(
    data = nfl_raw |> head(10)  # Start with first 10 rows
  )

  # Update the data when the button is clicked
  observeEvent(input$submit, {
    filtered <- nfl_raw

    if (input$home_team != "All Teams") {
      filtered <- filtered |> filter(home_team == input$home_team)
    }
    if (input$away_team != "All Teams") {
      filtered <- filtered |> filter(away_team == input$away_team)
    }

    v$data <- filtered |> head(10)  # Just show first 10 rows of filtered data
  })

  # Render the table
  output$qtr_table <- renderTable({
    v$data
  })
}

shinyApp(ui, server)
