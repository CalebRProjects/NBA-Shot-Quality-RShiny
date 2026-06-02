# NBA Shot Quality + Possession Quality Shiny App
# Step 1: stable backend + simple app shell
#
# Run:
#   shiny::runApp()
#
# First build cache:
#   source("R/07_cache_builder.R")
#   build_sq_cache(player_ids = c("203999", "1629029"), season = "2024-25", season_type = "Playoffs")

source("R/00_config.R")
source("R/01_helpers.R")
source("R/02_player_lookup.R")
source("R/03_data_pull_game_logs.R")
source("R/04_data_pull_pbp.R")
source("R/05_shot_quality_rules.R")
source("R/06_possession_quality.R")
source("R/08_player_summaries.R")
source("R/09_leaderboards.R")
source("R/07_cache_builder.R")

required_pkgs <- c(
  "shiny", "tidyverse", "janitor", "lubridate", "stringr",
  "readr", "scales", "glue", "DT", "plotly"
)

invisible(lapply(required_pkgs, require, character.only = TRUE))

cache <- load_app_cache()

ui <- navbarPage(
  title = APP_TITLE,

  tabPanel(
    "Home",
    fluidPage(
      br(),
      h2("NBA Shot Quality + Overall Possession Quality"),
      p("A public-data proxy model for evaluating player shot process, shot-making, and broader possession value."),
      tags$div(
        class = "alert alert-warning",
        strong("Important: "),
        "This is not the final film model. It uses NBA.com public data through hoopR, cached locally, and applies transparent placeholder rules where richer tracking or manual tagging is not available."
      ),
      h4("Current pipeline"),
      tags$ol(
        tags$li("NBA.com data via hoopR"),
        tags$li("Player game logs and play-by-play shot events"),
        tags$li("Shot quality bucket rules"),
        tags$li("Game-level shot quality and possession quality summaries"),
        tags$li("Player search and leaderboards")
      ),
      h4("Cache status"),
      verbatimTextOutput("home_cache_status")
    )
  ),

  tabPanel(
    "Methodology",
    fluidPage(
      br(),
      h2("Methodology / Definitions"),
      h3("Shot Quality Score"),
      p("Shot Quality Score is a process score for the shots a player takes. It is not the same as shot-making."),
      tags$ul(
        tags$li(strong("9:"), " highest-value attempts such as rim attempts, dunks, layups, and some very strong late-clock looks."),
        tags$li(strong("7:"), " strong-quality looks."),
        tags$li(strong("5:"), " neutral looks."),
        tags$li(strong("3:"), " difficult looks."),
        tags$li(strong("1:"), " very poor attempts."),
        tags$li(strong("Prayer:"), " end-clock heaves or bailout attempts tracked separately.")
      ),
      h3("Shot Making Layer"),
      p("Shot making is separated from shot quality. The starter version uses simple placeholder expected values by bucket. A future version should replace these with league-average baselines."),
      h3("Possession Quality Score"),
      p("Possession Quality Score extends beyond shot attempts by adding available box-score events such as assists, turnovers, steals, blocks, offensive rebounds, fouls, and fouls drawn where available."),
      h3("Automation status"),
      DT::dataTableOutput("methodology_fields")
    )
  ),

  tabPanel(
    "Player Search",
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          "player_id",
          "Player",
          choices = build_player_choices(cache$player_summary),
          selected = default_player_id(cache$player_summary),
          options = list(placeholder = "Search for a player")
        ),
        sliderInput("last_n", "Last X games", min = 1, max = 25, value = 10, step = 1),
        helpText("Uses cached data by default. Rebuild cache from R scripts before deploying updated data.")
      ),
      mainPanel(
        h3(textOutput("player_title")),
        fluidRow(
          column(3, strong("Games"), br(), textOutput("player_games")),
          column(3, strong("Avg SQ"), br(), textOutput("player_avg_sq")),
          column(3, strong("Avg PQ"), br(), textOutput("player_avg_pq")),
          column(3, strong("FGA"), br(), textOutput("player_fga"))
        ),
        hr(),
        h4("Shot Quality Over Time"),
        plotly::plotlyOutput("player_sq_plot"),
        h4("Shot Bucket Distribution"),
        plotly::plotlyOutput("player_bucket_plot"),
        h4("Game Log"),
        DT::dataTableOutput("player_game_log")
      )
    )
  ),

  tabPanel(
    "Leaderboards",
    sidebarLayout(
      sidebarPanel(
        numericInput("min_games", "Minimum games", value = 2, min = 1, step = 1),
        numericInput("min_fga", "Minimum FGA", value = 10, min = 0, step = 1),
        selectInput(
          "leaderboard_metric",
          "Metric",
          choices = c(
            "Shot Quality Score" = "avg_sq_score",
            "Possession Quality Score" = "avg_pq_score",
            "Shot Making Score" = "avg_shot_making_score"
          )
        )
      ),
      mainPanel(
        h3("Leaderboard"),
        p("Default leaderboard reflects the currently loaded cache."),
        DT::dataTableOutput("leaderboard_table")
      )
    )
  ),

  tabPanel(
    "Data / Pipeline Status",
    fluidPage(
      br(),
      h2("Data / Pipeline Status"),
      verbatimTextOutput("pipeline_status"),
      h3("Missing or Estimated Fields"),
      DT::dataTableOutput("missing_fields_table")
    )
  )
)

server <- function(input, output, session) {

  output$home_cache_status <- renderPrint({
    cache_status(cache)
  })

  output$methodology_fields <- DT::renderDataTable({
    methodology_field_table()
  }, options = list(pageLength = 20))

  selected_player_id <- reactive({
    req(input$player_id)
    as.character(input$player_id)
  })
  
  selected_games <- reactive({
    req(selected_player_id(), input$last_n)
    
    df <- get_player_games(
      game_summary = cache$game_summary,
      selected_player_id = selected_player_id(),
      last_n = input$last_n
    )
    
    validate(
      need(nrow(df) > 0, "No cached games found for this player.")
    )
    
    df
  })
  
  selected_game_ids <- reactive({
    selected_games() |>
      pull(game_id) |>
      unique()
  })
  
  selected_player_shots <- reactive({
    pid <- selected_player_id()
    gids <- selected_game_ids()
    
    df <- cache$shot_events |>
      filter(
        .data$player_id == pid,
        .data$game_id %in% gids
      )
    
    validate(
      need(nrow(df) > 0, "No cached shot events found for this player/game selection.")
    )
    
    df
  })

  output$player_title <- renderText({
    selected_games()$player_name[1]
  })

  output$player_games <- renderText({
    nrow(selected_games())
  })
  
  output$player_avg_sq <- renderText({
    scales::number(mean(selected_games()$avg_sq_score, na.rm = TRUE), accuracy = 0.01)
  })
  
  output$player_avg_pq <- renderText({
    scales::number(mean(selected_games()$pq_score, na.rm = TRUE), accuracy = 0.01)
  })
  
  output$player_fga <- renderText({
    sum(selected_games()$fga, na.rm = TRUE)
  })

  output$player_sq_plot <- plotly::renderPlotly({
    df <- selected_games() |>
      mutate(
        game_label = glue::glue(
          "{matchup}<br>{game_date}<br>Avg SQ: {round(avg_sq_score, 2)}<br>FGA: {fga}<br>PQ: {round(pq_score, 2)}"
        )
      )
    
    validate(
      need(nrow(df) > 0, "No games available for this player.")
    )
    
    p <- ggplot(df, aes(x = game_date, y = avg_sq_score, text = game_label)) +
      geom_hline(
        yintercept = 5,
        linetype = "dashed",
        linewidth = 0.4,
        alpha = 0.6
      ) +
      geom_point(size = 3) +
      scale_y_continuous(
        limits = c(1, 9),
        breaks = seq(1, 9, by = 1)
      ) +
      labs(
        x = NULL,
        y = "Average Shot Quality",
        title = glue::glue("{df$player_name[1]}: Shot Quality by Game"),
        subtitle = "1–9 public-data proxy scale; prayer shots excluded from average"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    plotly::ggplotly(p, tooltip = "text")
  })

  output$player_bucket_plot <- plotly::renderPlotly({
    df <- selected_player_shots() |>
      count(sq_bucket, name = "attempts") |>
      mutate(
        sq_bucket = factor(
          sq_bucket,
          levels = c("9", "7", "5", "3", "1", "Prayer")
        )
      )
    
    p <- ggplot(df, aes(x = sq_bucket, y = attempts)) +
      geom_col() +
      labs(
        x = "Shot Quality Bucket",
        y = "Attempts",
        title = "Shot Bucket Distribution",
        subtitle = glue::glue("Filtered to selected player and last {input$last_n} games")
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    plotly::ggplotly(p)
  })

  output$player_game_log <- DT::renderDataTable({
    selected_games() |>
      select(
        game_date, matchup, wl, min, fga, pts, ast, tov, stl, blk, oreb, pfd,
        avg_sq_score, shot_making_score, pq_score
      ) |>
      arrange(desc(game_date))
  }, options = list(pageLength = 10, scrollX = TRUE))

  output$leaderboard_table <- DT::renderDataTable({
    build_leaderboard(
      player_summary = cache$player_summary,
      metric = input$leaderboard_metric,
      min_games = input$min_games,
      min_fga = input$min_fga,
      n = 10
    )
  }, options = list(pageLength = 10, scrollX = TRUE))

  output$pipeline_status <- renderPrint({
    cache_status(cache)
  })

  output$missing_fields_table <- DT::renderDataTable({
    missing_or_estimated_fields()
  }, options = list(pageLength = 20))
  
  observe({
    message(glue::glue(
      "Selected player_id={input$player_id}; last_n={input$last_n}"
    ))
  })
}

shinyApp(ui, server)
