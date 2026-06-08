# NBA Attempt Quality + Possession Quality Shiny App
# Cache-first Shiny app using NBA.com data through hoopR.
#
# Run:
#   shiny::runApp()
#
# Build cache first:
#   source("R/07_cache_builder.R")
#   source("R/10_test_players.R")
#   build_sq_cache(
#     player_ids = test_players_default,
#     season = "2024-25",
#     season_type = "Playoffs",
#     force_refresh = TRUE
#   )

required_pkgs <- c(
  "shiny", "tidyverse", "janitor", "lubridate", "stringr",
  "readr", "scales", "glue", "DT", "plotly"
)

invisible(lapply(required_pkgs, require, character.only = TRUE))

source("R/00_config.R")
source("R/01_helpers.R")
source("R/02_player_lookup.R")
source("R/03_data_pull_game_logs.R")
source("R/04_data_pull_pbp.R")
source("R/05_shot_quality_rules.R")
source("R/06_possession_quality.R")
source("R/08_player_summaries.R")
source("R/09_leaderboards.R")
source("R/12_shot_profile.R")
source("R/07_cache_builder.R")

cache <- load_app_cache()

ui <- navbarPage(
  title = APP_TITLE,
  header = tags$head(
    includeCSS("www/styles.css")
  ),

  tabPanel(
    "Home",
    fluidPage(
      br(),
      h2("NBA Attempt Quality + Overall Possession Quality"),
      p("A public-data proxy model for evaluating player shot diet, shot-making, and broader possession value."),
      tags$div(
        class = "alert alert-warning",
        strong("Important: "),
        "This is not the final film model. It uses NBA.com public data through hoopR, cached locally, and applies transparent placeholder rules where richer tracking or manual tagging is not available."
      ),
      h4("Current pipeline"),
      tags$ol(
        tags$li("NBA.com data via hoopR"),
        tags$li("Player game logs and play-by-play shot events"),
        tags$li("Attempt quality bucket rules"),
        tags$li("Game-level attempt quality and possession quality summaries"),
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
      
      tags$div(
        class = "alert alert-info",
        strong("Current model status: "),
        "This is a public-data proxy model. It uses NBA.com game logs and play-by-play data through hoopR. It does not fully capture contest quality, creation burden, defensive context, or film-based possession details yet."
      ),
      
      h3("Attempt Quality Score"),
      p(
        "Attempt Quality Score measures the quality of the shots in a player's shot diet. ",
        "It is not the same as shot-making, and it does not fully capture how difficult those shots were to create."
      ),
      p(
        "This means play-finishing bigs or rim-heavy players may score highly because they take a large share of high-value attempts. ",
        "Primary creators may score lower because their role often requires pull-ups, late-clock shots, and self-created jumpers."
      ),
      
      h4("Current attempt buckets"),
      tags$ul(
        tags$li(strong("9:"), " Highest-quality attempts, such as dunks, layups, and very strong rim attempts."),
        tags$li(strong("7:"), " Strong-quality looks."),
        tags$li(strong("5:"), " Neutral looks."),
        tags$li(strong("3:"), " Difficult looks."),
        tags$li(strong("1:"), " Very poor attempts."),
        tags$li(strong("Grenade:"), " Late-clock bailout or heave attempts tracked separately so they do not overly punish attempt quality.")
      ),
      
      h4("Attempt Quality formula"),
      tags$pre(
        "Attempt Quality = mean(bucket score for all non-grenade attempts)

where:

9 bucket = 9
7 bucket = 7
5 bucket = 5
3 bucket = 3
1 bucket = 1
Grenade attempts = excluded from Attempt Quality average"
      ),
      
      h4("Current bucket assignment logic"),
      tags$pre(
        "Current public-data proxy inputs:
- shot_value
- shot_distance
- action_type
- sub_type
- description
- seconds_remaining

General rule structure:
- Close rim attempts and dunk/layup language are pushed toward 9s.
- Strong rim/paint actions and some high-value looks are pushed toward 7s.
- Neutral attempts are pushed toward 5s.
- Difficult attempts are pushed toward 3s or 1s.
- Very late, very deep attempts are classified as Grenades.
- Late-clock non-grenade attempts can be bumped up to avoid over-penalizing bailout shots."
      ),
      
      h3("Shot-Making Layer"),
      p(
        "Shot making is separated from attempt quality. The current version compares actual points to placeholder expected values by attempt bucket. ",
        "A future version should replace these placeholders with league-average points per shot by bucket."
      ),
      
      h4("Shot Making formula"),
      tags$pre(
        "Shot Making = Actual Points - Expected Points

At the shot level:
Actual Points = shot_value if made, otherwise 0
Expected Points = placeholder expected value for the assigned attempt bucket

At the game/player level:
Shot Making = sum(Actual Points - Expected Points)"
      ),
      
      h4("Current placeholder expected points"),
      tags$pre(
        "9 bucket = 1.35 expected points
7 bucket = 1.15 expected points
5 bucket = 1.00 expected points
3 bucket = 0.80 expected points
1 bucket = 0.55 expected points
Grenade = 0.20 expected points"
      ),
      
      h3("Possession Quality Score"),
      p(
        "Possession Quality Score is a broader placeholder value score that combines available box-score events, attempt quality, and shot-making. ",
        "The current weights are not final and should be treated as transparent working assumptions."
      ),
      
      h4("Possession Quality formula"),
      tags$pre(
        "Possession Quality =
  Box Score Component
  + Shot Process Component
  + Shot Making Component

Box Score Component =
  (AST * ast_weight)
  + (TOV * tov_weight)
  + (STL * stl_weight)
  + (BLK * blk_weight)
  + (OREB * oreb_weight)
  + (PF * pf_weight)
  + (PFD * fouls_drawn_weight)

Shot Process Component =
  Attempt Quality * FGA / 10

Shot Making Component =
  sum(Actual Points - Expected Points by Attempt Bucket)"
      ),
      
      h4("Current placeholder possession weights"),
      tags$pre(
        "AST = +1.00
TOV = -2.00
STL = +1.25
BLK = +1.00
OREB = +1.10
PF = -0.40
PFD = +0.60

Potential AST = +0.40 placeholder, not active unless available
Deflections = +0.35 placeholder, not active unless available
Charges Drawn = +1.25 placeholder, not active unless available"
      ),
      
      h3("Shot Profile Context"),
      p(
        "The app also reports shot-profile fields that help explain Attempt Quality. ",
        "These are calculated from play-by-play shot events and should be interpreted as public-data proxies."
      ),
      
      h4("Shot profile formulas"),
      tags$pre(
        "Rim ≤5 ft Rate = attempts with shot_distance <= 5 / total FGA from play-by-play

3PA Rate = attempts with shot_value == 3 / total FGA from play-by-play

Midrange Rate =
  two-point attempts with shot_distance > 5 and shot_distance < 23
  / total FGA from play-by-play

Average Shot Distance =
  mean(shot_distance)

Grenade Rate =
  grenade attempts / total FGA from play-by-play"
      ),
      
      h3("Current included possession events"),
      tags$ul(
        tags$li("Assists"),
        tags$li("Turnovers"),
        tags$li("Steals"),
        tags$li("Blocks"),
        tags$li("Offensive rebounds"),
        tags$li("Personal fouls"),
        tags$li("Personal fouls drawn"),
        tags$li("Attempt quality process component"),
        tags$li("Shot-making component")
      ),
      
      h3("Automation Status"),
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
          column(
            3,
            tags$div(
              class = "metric-card",
              tags$div(class = "metric-label", "Games"),
              tags$div(class = "metric-value", textOutput("player_games"))
            )
          ),
          column(
            3,
            tags$div(
              class = "metric-card",
              tags$div(class = "metric-label", "FGA"),
              tags$div(class = "metric-value", textOutput("player_fga"))
            )
          ),
          column(
            3,
            tags$div(
              class = "metric-card",
              tags$div(class = "metric-label", "Attempt Quality"),
              tags$div(class = "metric-value", textOutput("player_avg_sq"))
            )
          ),
          column(
            3,
            tags$div(
              class = "metric-card",
              tags$div(class = "metric-label", "Possession Quality"),
              tags$div(class = "metric-value", textOutput("player_avg_pq"))
            )
          )
        ),
        fluidRow(
          column(
            3,
            tags$div(
              class = "metric-card",
              tags$div(class = "metric-label", "Rim ≤5 ft Rate"),
              tags$div(class = "metric-value", textOutput("player_rim_rate"))
            )
          ),
          column(
            3,
            tags$div(
              class = "metric-card",
              tags$div(class = "metric-label", "3PA Rate"),
              tags$div(class = "metric-value", textOutput("player_three_rate"))
            )
          ),
          column(
            3,
            tags$div(
              class = "metric-card",
              tags$div(class = "metric-label", "Avg Shot Distance"),
              tags$div(class = "metric-value", textOutput("player_avg_dist"))
            )
          ),
          column(
            3,
            tags$div(
              class = "metric-card",
              tags$div(class = "metric-label", "Grenade Rate"),
              tags$div(class = "metric-value", textOutput("player_grenade_rate"))
            )
          )
        ),
        hr(),
        plotly::plotlyOutput("player_sq_plot"),
        br(),
        plotly::plotlyOutput("player_bucket_plot"),
        h4("Game Log"),
        tags$div(
          class = "model-note",
          "Table color bars are scaled within the selected player's visible games."
        ),
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
            "Attempt Quality Score" = "avg_sq_score",
            "Possession Quality Score" = "avg_pq_score",
            "Shot Making Score" = "avg_shot_making_score"
          )
        )
      ),
      mainPanel(
        h3("Leaderboard"),
        tags$div(
          class = "model-note",
          p("Default leaderboard reflects the currently loaded cache."),
          p("Attempt Quality reflects shot diet quality. It does not fully capture creation burden, contest quality, or defensive context."),
          p("Color bars use fixed reference ranges: Attempt Quality 1–9, Shot Making -5 to +5, and Possession Quality 0–30.")
        ),
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
  
  selected_player_summary <- reactive({
    req(selected_player_id())
    
    df <- cache$player_summary |>
      filter(.data$player_id == selected_player_id())
    
    validate(
      need(nrow(df) > 0, "No player summary found for this player.")
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
  
  output$player_rim_rate <- renderText({
    scales::percent(selected_player_summary()$rim_rate, accuracy = 0.1)
  })
  
  output$player_three_rate <- renderText({
    scales::percent(selected_player_summary()$three_rate, accuracy = 0.1)
  })
  
  output$player_avg_dist <- renderText({
    paste0(
      scales::number(selected_player_summary()$avg_shot_distance, accuracy = 0.1),
      " ft"
    )
  })
  
  output$player_grenade_rate <- renderText({
    scales::percent(selected_player_summary()$grenade_rate, accuracy = 0.1)
  })

  output$player_sq_plot <- plotly::renderPlotly({
    df <- selected_games() |>
      mutate(
        game_label_axis = glue::glue("{format(game_date, '%b %d')}<br>{matchup}"),
        game_label_axis = factor(game_label_axis, levels = game_label_axis),
        hover_label = glue::glue(
          "{matchup}<br>{game_date}<br>Attempt Quality: {round(avg_sq_score, 2)}<br>FGA: {fga}<br>Shot Making: {round(shot_making_score, 2)}<br>Possession Quality: {round(pq_score, 2)}"
        )
      )
    
    validate(
      need(nrow(df) > 0, "No games available for this player.")
    )
    
    p <- ggplot(df, aes(x = game_label_axis, y = avg_sq_score, text = hover_label)) +
      geom_col(alpha = 0.75) +
      geom_hline(
        yintercept = 5,
        linetype = "dashed",
        linewidth = 0.4,
        alpha = 0.7
      ) +
      scale_y_continuous(
        limits = c(0, 9),
        breaks = seq(0, 9, by = 1)
      ) +
      labs(
        x = NULL,
        y = "Attempt Quality",
        title = "Attempt Quality by Game",
        subtitle = glue::glue("{df$player_name[1]} | Grenades excluded from average")
      ) + 
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "#66615a"),
        axis.text.x = element_text(size = 8),
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
          levels = c("9", "7", "5", "3", "1", "Grenade")
        )
      )
    
    p <- ggplot(df, aes(x = sq_bucket, y = attempts, fill = sq_bucket)) +
      geom_col(alpha = 0.75, show.legend = FALSE) +
      labs(
        x = "Attempt Quality Bucket",
        y = "Attempts",
        title = "Attempt Bucket Distribution",
        subtitle = glue::glue("{selected_games()$player_name[1]} | Last {input$last_n} games")
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "#66615a"),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )
    
    plotly::ggplotly(p)
  })

  output$player_game_log <- DT::renderDataTable({
    game_log_df <- selected_games() |>
      select(
        Date = game_date,
        Matchup = matchup,
        Result = wl,
        MIN = min,
        FGA = fga,
        PTS = pts,
        AST = ast,
        TOV = tov,
        STL = stl,
        BLK = blk,
        OREB = oreb,
        `Fouls Drawn` = pfd,
        `Non-Grenade FGA` = non_grenade_fga,
        Grenades = grenade_attempts,
        `Attempt Quality` = avg_sq_score,
        `Shot Making` = shot_making_score,
        `Possession Quality` = pq_score
      ) |>
      mutate(
        MIN = round(MIN, 1),
        `Attempt Quality` = round(`Attempt Quality`, 2),
        `Shot Making` = round(`Shot Making`, 2),
        `Possession Quality` = round(`Possession Quality`, 2)
      ) |>
      arrange(desc(Date))
    
    DT::datatable(
      game_log_df,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "tip"
      )
    ) |>
      DT::formatStyle(
        "Attempt Quality",
        background = DT::styleColorBar(
          range(game_log_df$`Attempt Quality`, na.rm = TRUE),
          "lightblue"
        ),
        backgroundSize = "98% 70%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) |>
      DT::formatStyle(
        "Shot Making",
        background = DT::styleColorBar(
          range(game_log_df$`Shot Making`, na.rm = TRUE),
          "lightgreen"
        ),
        backgroundSize = "98% 70%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) |>
      DT::formatStyle(
        "Possession Quality",
        background = DT::styleColorBar(
          range(game_log_df$`Possession Quality`, na.rm = TRUE),
          "khaki"
        ),
        backgroundSize = "98% 70%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  output$leaderboard_table <- DT::renderDataTable({
    leaderboard_df <- build_leaderboard(
      player_summary = cache$player_summary,
      metric = input$leaderboard_metric,
      min_games = input$min_games,
      min_fga = input$min_fga,
      n = 10
    )
    
    DT::datatable(
      leaderboard_df,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "tip"
      )
    ) |>
      DT::formatStyle(
        "Attempt Quality",
        background = DT::styleColorBar(
          c(1, 9),
          "lightblue"
        ),
        backgroundSize = "98% 70%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) |>
      DT::formatStyle(
        "Shot Making",
        background = DT::styleColorBar(
          c(-5, 5),
          "lightgreen"
        ),
        backgroundSize = "98% 70%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) |>
      DT::formatStyle(
        "Possession Quality",
        background = DT::styleColorBar(
          c(0, 30),
          "khaki"
        ),
        backgroundSize = "98% 70%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  output$pipeline_status <- renderPrint({
    cache_status(cache)
  })
  
  output$missing_fields_table <- DT::renderDataTable({
    missing_or_estimated_fields()
  }, options = list(pageLength = 20))
  
}

shinyApp(ui, server)
