# ST558 Project 3
# Purpose: Shiny app to explore NFL Play by Play data from 2009-2018
# Course: ST558 Data Science for Statisticians
# Date: 06-Nov-2024
# Author: David Pressley
# Data Source: Kaggle NFL Play by Play Dataset

source("load_data.R")

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
        )
      ),

      h2("Select a quarter of play"),
      sliderInput("qtr_slider", "Select Quarter (5 = Overtime):",
                  min = 1,
                  max = 5,
                  value = c(1,5),
                  ticks = FALSE,
                  step = 1),
      h2("Select downs to analyze"),
      sliderInput("downs_slider", "Select Downs:",
                  min = 1,
                  max = 4,
                  value = c(1,4),
                  ticks = FALSE,
                  step = 1),
      h2("Select time remaining in Quarter"),
      sliderInput("time_slider", "Select Seconds in Quarter:",
                  min = min(time_seq),
                  max = max(time_seq),
                  value = min(time_seq),
                  step = 60),
      h3("Selected time: "),
      verbatimTextOutput("selected_time"),
      br(),
      actionButton("update_data", "Subset Data",
                   class = "btn-primary btn-lg btn-block"),
      br(),
      p("Click Subset Data button and navigate to the Downloads or Data Exploration Tab"),
      br(),
      br()
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("About",
                 h3("Purpose"),
                 p("This app is used to explore NFL Data. The source of this data is from Kaggle",
                   a(href = "https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016/data",
                     "Detailed NFL Play-by-Play Data 2009-2018",
                     target = "_blank" ),
                   p(a(href = "https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016/download",
                       "Download ",
                       target = "_blank"),
                     "the dataset as a zip file and place in your R project's working directory"),
                   p("The sidebar allows you to select 2 teams, or all teams, as well as a range of Quarters and Time remaining in the selected quarter"),
                   a(href = "https://www.nfl.com",
                     img(src = "https://static.www.nfl.com/image/upload/v1554321393/league/nvfr7ogywskqrfaiu38m.svg",
                         height = 150,
                         alt = "NFL Logo"),
                     target = "_blank")
                 )
        ),
        tabPanel("Download",
                 h3("Download Filtered Data"),
                 p("This will download a CSV file containing the data filtered by:"),
                 tags$ul(
                   tags$li("Selected home and away teams"),
                   tags$li("Selected quarter range"),
                   tags$li("Selected time remaining in quarter")
                 ),
                 downloadButton('downloadData', 'Download CSV',
                                class = "btn-primary"),
                 hr(),
                 h3("Preview of Filtered Data"),
                 # Add spinner to the DT output
                 shinycssloaders::withSpinner(
                   DT::dataTableOutput("filtered_table"),
                   type = 8,
                   color = "#0275d8",
                   size = 1
                 )
        ),

        tabPanel("Data Exploration",
                 h3(textOutput("data_summary")),
                 tabsetPanel(
                   tabPanel("Visualization",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("analysis_type", "Visualization Type",
                                            choices = c("Yards by Down" = "yards_box",
                                                        "EPA Distribution" = "epa_density",
                                                        "Play Distribution" = "play_dist_heat",
                                                        "Success Rate" = "success_tile",
                                                        "Field Goal Success" = "fg_success",
                                                        "Run Direction Analysis" = "run_direction")),
                                selectInput("play_filter", "Filter by Play Type",
                                            choices = c("All", "run", "pass")),
                                selectInput("down_filter", "Filter by Down",
                                            choices = c("All", "1", "2", "3", "4")),
                                checkboxInput("show_facet", "Show Faceted Plot", FALSE),
                                selectInput("color_var", "Facet Variable",
                                            choices = c("None", "play_type", "down")),
                                hr(),
                                downloadButton("download_plot", "Download Plot")
                              ),
                              mainPanel(
                                shinycssloaders::withSpinner(
                                  plotOutput("analysis_plot"),
                                  type = 8,
                                  color = "#0275d8"
                                )
                              )
                            )
                   ),
                   tabPanel("Summaries",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("summary_type", "Summary Type",
                                            choices = c("Numeric" = "numeric",
                                                        "Categorical" = "categorical")),
                                conditionalPanel(
                                  condition = "input.summary_type == 'numeric'",
                                  selectInput("numeric_var", "Select Numeric Variable",
                                              choices = c("yards_gained", "epa", "ydstogo", "score_differential")),
                                  selectInput("group_var", "Group By",
                                              choices = c("None", "play_type", "down", "qtr"))
                                ),
                                conditionalPanel(
                                  condition = "input.summary_type == 'categorical'",
                                  selectInput("cat_var1", "First Variable",
                                              choices = c("play_type", "down", "qtr")),
                                  selectInput("cat_var2", "Second Variable (Optional)",
                                              choices = c("None", "play_type", "down", "qtr")),
                                  checkboxInput("show_proportions", "Show Proportions", FALSE)
                                ),
                                downloadButton("download_summary", "Download Summary")
                              ),
                              mainPanel(
                                conditionalPanel(
                                  condition = "input.summary_type == 'numeric'",
                                  h4("Numeric Summary"),
                                  shinycssloaders::withSpinner(
                                    tableOutput("numeric_summary"),
                                    type = 8,
                                    color = "#0275d8"
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.summary_type == 'categorical'",
                                  h4("Categorical Summary"),
                                  shinycssloaders::withSpinner(
                                    tableOutput("categorical_summary"),
                                    type = 8,
                                    color = "#0275d8"
                                  )
                                )
                              )
                            )
                   )
                 )
        )))
  )
)

server <- function(input, output, session) {
  # Create a reactive expression for filtered data
  filtered_data <- reactive({
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

    # Apply time/quarter/down filters
    filtered |>
      filter(qtr >= input$qtr_slider[1] & qtr <= input$qtr_slider[2]) |>
      filter(!is.na(time)) |>
      mutate(time_seconds = as.numeric(time)) |>
      filter(time_seconds >= input$time_slider) |>
      filter(down >= input$downs_slider[1] & down <= input$downs_slider[2]) |>
      select(play_id, game_id, home_team, away_team, qtr, down, time,
             score_differential, yards_gained, play_type,
             yardline_100, first_down_rush, first_down_pass,
             field_goal_attempt, field_goal_result, kick_distance,
             run_location, epa, ydstogo)
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

  # Render time output
  output$selected_time <- renderPrint({
    as_hms(input$time_slider)
  })

  # Numeric summary output
  output$numeric_summary <- renderTable({
    req(filtered_data(), input$numeric_var)
    data <- filtered_data()

    # Create descriptive title based on selections
    title_text <- if(input$group_var == "None") {
      sprintf("Summary of %s", tools::toTitleCase(gsub("_", " ", input$numeric_var)))
    } else {
      sprintf("Summary of %s by %s",
              tools::toTitleCase(gsub("_", " ", input$numeric_var)),
              tools::toTitleCase(gsub("_", " ", input$group_var)))
    }

    # Define summary stats function
    calc_summary_stats <- \(x) {
      c(Mean = mean(x, na.rm = TRUE),
        Median = median(x, na.rm = TRUE),
        SD = sd(x, na.rm = TRUE),
        Min = min(x, na.rm = TRUE),
        Max = max(x, na.rm = TRUE))
    }

    # Calculate summary
    if(input$group_var == "None") {
      result <- data.frame(t(calc_summary_stats(data[[input$numeric_var]])))
    } else {
      groups <- split(data[[input$numeric_var]], data[[input$group_var]])
      summary_df <- do.call(rbind, lapply(groups, calc_summary_stats))
      result <- as.data.frame(summary_df)
      result$Group <- rownames(result)
      result <- result[, c("Group", "Mean", "Median", "SD", "Min", "Max")]
    }

    # Add title as attribute
    attr(result, "title") <- title_text
    result
  }, caption = function(x) attr(x, "title"))

  # Categorical summary output
  output$categorical_summary <- renderTable({
    req(filtered_data(), input$cat_var1)

    data <- filtered_data()

    if(input$cat_var2 == "None") {
      tbl <- table(data[[input$cat_var1]])
      if(input$show_proportions) {
        as.data.frame(prop.table(tbl) * 100)
      } else {
        as.data.frame(tbl)
      }
    } else {
      tbl <- table(data[[input$cat_var1]], data[[input$cat_var2]])
      if(input$show_proportions) {
        as.data.frame(prop.table(tbl) * 100)
      } else {
        as.data.frame(tbl)
      }
    }
  })

  # Analysis plot output
  output$analysis_plot <- renderPlot({
  req(filtered_data(), input$analysis_type)
  data <- filtered_data()

  lots_of_plots <- switch(input$analysis_type,
                          "yards_box" = {
                            data_subset <- if(input$play_filter != "All") {
                              filter(data, play_type == input$play_filter)
                            } else data

                            ggplot(data_subset,
                                   aes(x = factor(down), y = yards_gained, fill = play_type)) +
                              geom_boxplot() +
                              labs(title = "Yards Gained by Down and Play Type",
                                   x = "Down", y = "Yards Gained") +
                              scale_fill_viridis_d() +
                              theme_minimal()
                          },
                          "epa_density" = {
                            data_subset <- if(input$down_filter != "All") {
                              filter(data, down == input$down_filter)
                            } else data

                            ggplot(data_subset, aes(x = epa, y = factor(down), fill = factor(down))) +
                              geom_density_ridges() +
                              facet_wrap(~play_type) +
                              labs(title = "EPA Distribution by Down and Play Type",
                                   x = "EPA", y = "Down") +
                              theme_minimal()
                          },
                          "play_dist_heat" = {
                            data |>
                              filter(play_type %in% c("run", "pass"),
                                     !is.na(down),
                                     !is.na(yardline_100)) |>
                              mutate(field_position_bin = cut(yardline_100,
                                                              breaks = seq(0, 100, length.out = input$field_position_bins + 1))) |>
                              group_by(play_type, field_position_bin, down) |>
                              summarise(count = n(), .groups = 'drop') |>
                              group_by(field_position_bin, down) |>
                              mutate(percentage = count/sum(count) * 100) |>
                              ggplot(aes(x = field_position_bin, y = factor(down), fill = percentage)) +
                              geom_tile() +
                              facet_wrap(~play_type) +
                              scale_fill_gradient(low = "lightblue", high = "darkblue") +
                              labs(title = "Play Type Distribution",
                                   x = "Field Position",
                                   y = "Down",
                                   fill = "Percentage") +
                              theme_minimal() +
                              theme(axis.text.x = element_text(angle = 45))
                          },
                          "success_tile" = {
                            data |>
                              filter(!is.na(ydstogo),
                                     !is.na(down),
                                     !is.na(first_down_rush),
                                     !is.na(first_down_pass)) |>
                              mutate(yards_to_go_bin = cut(ydstogo,
                                                           breaks = c(0,2,5,7,10,15,20,Inf))) |>
                              group_by(down, yards_to_go_bin) |>
                              summarise(success_rate = mean(first_down_rush | first_down_pass, na.rm = TRUE)) |>
                              ggplot(aes(x = yards_to_go_bin, y = factor(down), fill = success_rate)) +
                              geom_tile() +
                              scale_fill_gradient(low = "lightblue", high = "darkblue") +
                              labs(title = "Success Rate by Down and Distance",
                                   x = "Yards to Go",
                                   y = "Down",
                                   fill = "Success Rate") +
                              theme_minimal()
                          },
                          "fg_success" = {
                            data |>
                              filter(field_goal_attempt == 1,
                                     !is.na(field_goal_result),
                                     !is.na(kick_distance)) |>
                              mutate(success = as.numeric(field_goal_result == "made")) |>
                              ggplot(aes(x = kick_distance, y = success)) +
                              geom_smooth(method = "loess", se = TRUE, color = "steelblue") +
                              labs(title = "Field Goal Success Probability by Distance",
                                   x = "Field Goal Distance (yards)",
                                   y = "Success Probability") +
                              theme_minimal() +
                              scale_y_continuous(labels = scales::percent) +
                              coord_cartesian(ylim = c(0, 1))
                          },
                          "run_direction" = {
                            data |>
                              filter(play_type == "run",
                                     !is.na(run_location),
                                     !is.na(yards_gained)) |>
                              ggplot(aes(x = run_location, y = yards_gained)) +
                              geom_violin(fill = "lightgreen", alpha = 0.7) +
                              geom_boxplot(width=0.2, fill = "white", alpha = 0.7) +
                              labs(title = "Distribution of Yards Gained by Run Direction",
                                   x = "Run Direction",
                                   y = "Yards Gained") +
                              theme_minimal() +
                              coord_cartesian(ylim = c(-10, 20))
                          }
  )

  # Add faceting if selected
  if (input$show_facet && !is.null(input$color_var) && input$color_var != "None") {
    lots_of_plots <- lots_of_plots + facet_wrap(as.formula(paste("~", input$color_var)))
  }

  lots_of_plots
})

  # Dynamic text summary
  output$data_summary <- renderText({
    req(filtered_data())
    data <- filtered_data()

    teams_text <- if (input$home_team == "All Teams" && input$away_team == "All Teams") {
      "all teams"
    } else if (input$home_team == "All Teams") {
      paste("all home teams vs", input$away_team)
    } else if (input$away_team == "All Teams") {
      paste(input$home_team, "vs all away teams")
    } else {
      paste(input$home_team, "vs", input$away_team)
    }

    sprintf("Analysis of %d plays for %s. Summary includes %d runs and %d passes.",
            nrow(data),
            teams_text,
            sum(data$play_type == "run", na.rm = TRUE),
            sum(data$play_type == "pass", na.rm = TRUE))
  })


  # Render filtered table
  output$filtered_table <- renderDataTable({
    filtered_data() |>
      select(play_id, game_id, home_team, away_team, qtr, down, time,
             score_differential, yards_gained) |>
      head(100)
  })

  # Download handler for filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("nfl_filtered_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )

  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("nfl_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 10, height = 7, dpi = 300)
    }
  )

  # Download handler for summaries
  output$download_summary <- downloadHandler(
    filename = function() {
      paste0("nfl_", input$summary_type, "_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      # Get the data
      data <- filtered_data()

      #If numeric, else, categorical
      if (input$summary_type == "numeric") {
        # Anonymous function to calculate summary stats for numerics
        calc_summary_stats <- \(x) {
          c(Mean = mean(x, na.rm = TRUE),
            Median = median(x, na.rm = TRUE),
            SD = sd(x, na.rm = TRUE),
            Min = min(x, na.rm = TRUE),
            Max = max(x, na.rm = TRUE))
        }

        #When group_var is None, Numeric summary is an aggregate, otherwise
        #numeric summary is by group selected in Summaries Tab Panel UI
        if(input$group_var == "None") {
          # Single group summary
          result <- data.frame(t(calc_summary_stats(data[[input$numeric_var]])))
        } else {
          # Create empty list to store results for each group
          summary_results <- list()

          # Get unique groups
          group_levels <- unique(data[[input$group_var]])

          # Loop through each group and calculate statistics
          for(group in group_levels) {
            # Subset data for this group
            group_data <- data[[input$numeric_var]][data[[input$group_var]] == group]

            # Calculate statistics for this group
            # Use calc_summary_stats anonymous function
            group_stats <- calc_summary_stats(group_data)

            # Store in list with group name
            summary_results[[group]] <- group_stats
          }

          # Convert list to data frame
          result <- as.data.frame(do.call(rbind, summary_results))

          # Add group names as a column
          result$Group <- rownames(result)

          # Reorder columns to put Group first
          result <- result[, c("Group", "Mean", "Median", "SD", "Min", "Max")]
        }
      } else {
        # Categorical summaries
        if(input$cat_var2 == "None") {
          # Single variable analysis
          # Get the vector of categories
          categories <- data[[input$cat_var1]]

          # Calculate frequencies
          unique_cats <- unique(categories)
          frequencies <- numeric(length(unique_cats))

          # Count occurrences of each category
          for(i in seq_along(unique_cats)) {
            frequencies[i] <- sum(categories == unique_cats[i], na.rm = TRUE)
          }

          # Create base result
          result <- data.frame(
            Category = unique_cats,
            Frequency = frequencies
          )

          # Add proportions if requested
          if(input$show_proportions) {
            total_count <- sum(frequencies)
            result$Proportion <- (result$Frequency / total_count) * 100
            result <- result[, c("Category", "Proportion")]
          }

        } else {
          # Two-variable analysis
          # Get vectors for both variables
          var1 <- data[[input$cat_var1]]
          var2 <- data[[input$cat_var2]]

          # Get unique values for both variables
          unique_var1 <- unique(var1)
          unique_var2 <- unique(var2)

          # Create count matrix using sapply
          count_matrix <- sapply(
            unique_var2,
            function(y) sapply(
              unique_var1,
              function(x) sum(var1 == x & var2 == y, na.rm = TRUE)
            )
          )

          # Convert to data frame and add row/column names
          result <- as.data.frame(count_matrix)
          names(result) <- unique_var2
          result$Category <- unique_var1

          # Calculate proportions if requested
          if(input$show_proportions) {
            total_count <- sum(count_matrix)
            # Convert counts to proportions
            for(col in names(result)[-ncol(result)]) {  # exclude Category column
              result[[col]] <- (result[[col]] / total_count) * 100
            }
          }

          # Reorder columns to put Category first
          result <- result[, c("Category", setdiff(names(result), "Category"))]
        }
      }

      # Write the results
      write.csv(result, file, row.names = FALSE)
    }
  )
}

shinyApp(ui,server)
