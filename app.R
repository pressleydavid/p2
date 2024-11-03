library(tidyverse)
library(shiny)
library(fst)
library(hms)
library(bslib)
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

# Set up a by-minute sequence in seconds
time_seq <- seq(0, 900, by = 60)

ui <- fluidPage(
  titlePanel("NFL Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      card(
        full_screen = TRUE,
        card_header(
          h2("Select Teams")
        ),
        card_body(
          height = "250px",
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
          div(style = "margin-bottom: 20px;")
        ),
        card_footer(
          actionButton("submit", "Subset Data",
                       class = "btn-primary")
        )
      ),

      h2("Select a quarter of play"),
      sliderInput("qtr_slider", "Select Quarter (5 = Overtime):",
                  min = 1,
                  max = 5,
                  value = c(1,5),
                  ticks = FALSE,
                  step = 1),
      h2("Select a time range for quarter"),
      sliderInput("time_slider", "Select Seconds in Quarter:",
                  min = min(time_seq),
                  max = max(time_seq),
                  value = min(time_seq),
                  step = 60),
      h3("Selected time: "),
      verbatimTextOutput("selected_time")
    ),
    mainPanel(
      tableOutput("qtr_table")
    )
  )
)

server <- function(input, output, session) {

  # Store the filtered data in a container
  init_filtered <- reactiveValues(
    filtered_teams = NULL
  )

  # Update filtered teams on submit
  observeEvent(input$submit, {
    # Start with all data
    filtered <- nfl_raw

    # Apply team filters
    if (input$home_team != "All Teams" && input$away_team != "All Teams") {
      filtered <- filtered |>
        filter(home_team == input$home_team, away_team == input$away_team)
    } else if (input$home_team != "All Teams") {
      filtered <- filtered |> filter(home_team == input$home_team)
    } else if (input$away_team != "All Teams") {
      filtered <- filtered |> filter(away_team == input$away_team)
    }

    init_filtered$filtered_teams <- filtered
  }, ignoreNULL = FALSE)

  # Time/quarter filtering
  filtered_data <- reactive({

    init_filtered$filtered_teams |>
      filter(qtr >= input$qtr_slider[1] & qtr <= input$qtr_slider[2]) |>
      filter(!is.na(time)) |>
      mutate(time_seconds = as.numeric(time)) |>
      filter(time_seconds >= input$time_slider) |>
      select(play_id, game_id, home_team, away_team, qtr, time, score_differential, yards_gained) |>
      head(100)
  })

  # Update away team based on home team
  observeEvent(input$home_team, {
    if (input$home_team == "All Teams") {
      away_choices <- c("All Teams", sort(na.omit(unique(nfl_raw$away_team))))
    } else {
      away_choices <- c("All Teams",
                        sort(na.omit(unique(nfl_raw$away_team[nfl_raw$home_team == input$home_team]))))
    }
    updateSelectInput(session, "away_team",
                      choices = away_choices,
                      selected = if(input$away_team %in% away_choices) input$away_team else "All Teams")
  })

  # Update home team based on away team
  observeEvent(input$away_team, {
    if (input$away_team == "All Teams") {
      home_choices <- c("All Teams", sort(na.omit(unique(nfl_raw$home_team))))
    } else {
      home_choices <- c("All Teams",
                        sort(na.omit(unique(nfl_raw$home_team[nfl_raw$away_team == input$away_team]))))
    }
    updateSelectInput(session, "home_team",
                      choices = home_choices,
                      selected = if(input$home_team %in% home_choices) input$home_team else "All Teams")
  })

  # Render time output underneath slider since slider values are seconds
  #TODO: update slider values to mm:ss
  output$selected_time <- renderPrint({
    as_hms(input$time_slider)
  })

  # Render table output. Require data to render
  output$qtr_table <- renderTable({
    req(filtered_data())

    filtered_data() |>
      mutate(time = format(as_hms(time), "%M:%S"))
  })
}

shinyApp(ui, server)
