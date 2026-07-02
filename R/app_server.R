# R/app_server.R
# Main Shiny server logic for the Attempt Quality + Possession Quality app.

required_pkgs <- c("shiny", "tidyverse", "DT", "scales", "glue")
invisible(lapply(required_pkgs, require, character.only = TRUE))


app_server <- function(input, output, session) {
  cache_bundle <- load_app_caches()
  
  observe({
    season_choices <- get_available_seasons(cache_bundle)
    
    shiny::updateSelectInput(
      session,
      "player_season",
      choices = season_choices,
      selected = season_choices[[1]]
    )
    
    shiny::updateSelectInput(
      session,
      "leaderboard_season",
      choices = season_choices,
      selected = season_choices[[1]]
    )
  })
  
  # Home ----------------------------------------------------------------------
  
  home_cache <- reactive({
    latest_season <- cache_bundle$index |>
      dplyr::filter(.data$season_type_key == "regular") |>
      dplyr::arrange(dplyr::desc(.data$data_year)) |>
      dplyr::slice(1) |>
      dplyr::pull(.data$season)
    
    get_cache_by_selection(
      cache_bundle = cache_bundle,
      season = latest_season,
      season_type_key = "regular"
    )
  })
  
  output$home_season <- renderText({
    home_cache()$metadata$season
  })
  
  output$home_players <- renderText({
    fmt_int(home_cache()$metadata$players)
  })
  
  output$home_games <- renderText({
    fmt_int(home_cache()$metadata$games)
  })
  
  output$home_fga <- renderText({
    fmt_int(home_cache()$metadata$total_fga)
  })
  
  output$home_top_combined <- DT::renderDT({
    home_cache() |>
      get_leaderboard("combined_value_total", n = 15) |>
      format_leaderboard_table() |>
      make_dt(page_length = 15)
  })
  
  
  # Methodology ---------------------------------------------------------------
  
  method_cache <- home_cache
  
  output$method_expected_points <- DT::renderDT({
    method_cache()$expected_points_by_bucket |>
      dplyr::mutate(
        dplyr::across(where(is.numeric), ~ round(.x, 3))
      ) |>
      clean_table_names() |>
      make_dt(page_length = 10)
  })
  
  
  # Player Search -------------------------------------------------------------
  
  player_cache <- reactive({
    req(input$player_season, input$player_season_type)
    
    get_cache_by_selection(
      cache_bundle = cache_bundle,
      season = input$player_season,
      season_type_key = input$player_season_type
    )
  })
  
  output$player_selector_ui <- renderUI({
    choices <- get_player_choices(player_cache())
    
    shiny::selectizeInput(
      "selected_player",
      "Player",
      choices = choices,
      selected = if ("1628983__OKC" %in% choices) "1628983__OKC" else choices[[1]],
      options = list(
        placeholder = "Search player...",
        maxOptions = 1000
      )
    )
  })
  
  selected_player_detail <- reactive({
    req(input$selected_player)
    
    get_player_detail(
      cache = player_cache(),
      player_choice = input$selected_player
    )
  })
  
  selected_player_detail_with_percentiles <- reactive({
    req(input$selected_player)
    
    selected <- split_player_choice(input$selected_player)
    
    player_cache()$player_detail |>
      add_player_percentiles() |>
      dplyr::filter(
        player_id == selected$player_id,
        team_tricode == selected$team_tricode
      )
  })
  
  output$player_hero_ui <- renderUI({
    detail <- selected_player_detail_with_percentiles()
    
    if (nrow(detail) == 0) {
      return(
        shiny::tags$div(
          class = "player-hero",
          shiny::tags$div(class = "player-name", "Select a player")
        )
      )
    }
    
    row <- detail[1, ]
    
    shiny::tags$div(
      class = "player-hero",
      
      shiny::tags$div(
        class = "player-hero-top",
        
        shiny::tags$div(
          shiny::tags$div(
            class = "player-name",
            paste0(row$player_name, " - ", row$team_tricode)
          ),
          shiny::tags$div(
            class = "player-context",
            season_type_label(input$player_season_type)
          )
        ),
        
        shiny::tags$div(
          class = "player-volume",
          paste0(
            row$games, " games",
            " · ", scales::comma(row$fga), " FGA",
            " · ", scales::comma(row$assists), " AST",
            " · ", scales::comma(row$turnovers), " TOV"
          )
        )
      ),
      
      shiny::tags$div(
        class = "stat-rail",
        
        stat_unit_ui(
          label = "Attempt Quality",
          value = round(row$avg_aq_score, 2),
          percentile = row$aq_percentile
        ),
        
        stat_unit_ui(
          label = "Expected Pts / FGA",
          value = round(row$expected_points_per_attempt, 3),
          percentile = row$expected_points_percentile
        ),
        
        stat_unit_ui(
          label = "Shot Making / FGA",
          value = format_signed_number(row$shot_making_per_attempt, 3),
          percentile = row$shot_making_percentile
        ),
        
        stat_unit_ui(
          label = "Possession Quality",
          value = round(row$avg_possession_quality, 3),
          percentile = row$pq_percentile
        )
      ),
      
      shiny::tags$div(
        class = "stat-note",
        "Percentiles compare the player to all players in the selected season type."
      )
    )
  })
  
  selected_player_game_log <- reactive({
    req(input$selected_player)
    
    get_player_game_log(
      cache = player_cache(),
      player_choice = input$selected_player,
      last_n_games = input$last_n_games
    )
  })
  
  selected_player_shots <- reactive({
    req(input$selected_player)
    
    get_player_shots(
      cache = player_cache(),
      player_choice = input$selected_player,
      last_n_games = input$last_n_games
    )
  })
  
  output$player_header <- renderText({
    detail <- selected_player_detail()
    
    if (nrow(detail) == 0) {
      return("Player Summary")
    }
    
    paste0(
      detail$player_name[1],
      " - ",
      detail$team_tricode[1],
      " | ",
      season_type_label(input$player_season_type)
    )
  })
  
  output$player_headline <- DT::renderDT({
    selected_player_detail() |>
      player_headline_stats() |>
      make_dt(page_length = 1)
  })
  
  output$player_game_log <- DT::renderDT({
    table <- selected_player_game_log() |>
      format_game_log_table()
    
    DT::datatable(
      table,
      rownames = FALSE,
      class = "compact stripe hover nowrap",
      options = list(
        pageLength = input$last_n_games,
        scrollX = TRUE,
        autoWidth = FALSE,
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      )
    ) |>
      DT::formatStyle(
        "Attempt Quality",
        backgroundColor = DT::styleInterval(
          c(5.5, 6.25, 7.0, 7.75),
          c("#f8d7da", "#fde9b9", "#fff8d6", "#d8f0dc", "#bfe7c5")
        )
      ) |>
      DT::formatStyle(
        "Expected Pts / FGA",
        backgroundColor = DT::styleInterval(
          c(0.85, 0.95, 1.05, 1.15),
          c("#f8d7da", "#fde9b9", "#fff8d6", "#d8f0dc", "#bfe7c5")
        )
      ) |>
      DT::formatStyle(
        "Shot Making / FGA",
        backgroundColor = DT::styleInterval(
          c(-0.10, -0.025, 0.025, 0.10),
          c("#f4cccc", "#fce5cd", "#ffffff", "#d9ead3", "#b6d7a8")
        )
      ) |>
      DT::formatStyle(
        "Possession Quality",
        backgroundColor = DT::styleInterval(
          c(0.45, 0.60, 0.75, 0.90),
          c("#f8d7da", "#fde9b9", "#fff8d6", "#d8f0dc", "#bfe7c5")
        )
      ) |>
      DT::formatStyle(
        "Total Game Value",
        backgroundColor = DT::styleInterval(
          c(10, 20, 30, 40),
          c("#f8d7da", "#fde9b9", "#fff8d6", "#d8f0dc", "#bfe7c5")
        )
      )
  })
  
  output$player_shots <- DT::renderDT({
    selected_player_shots() |>
      dplyr::select(
        game_date,
        game_id,
        htm,
        vtm,
        period,
        clock,
        action_type,
        sub_type,
        shot_zone_basic,
        shot_zone_area,
        shot_zone_range,
        shot_distance,
        shot_value,
        shot_made_flag,
        attempt_quality_bucket,
        attempt_quality_score,
        expected_points,
        actual_points,
        shot_making_value
      ) |>
      dplyr::mutate(
        Game = paste0(vtm, " @ ", htm),
        game_date = as.character(game_date),
        dplyr::across(where(is.numeric), ~ round(.x, 3))
      ) |>
      dplyr::select(
        game_date,
        Game,
        period,
        clock,
        action_type,
        sub_type,
        shot_zone_basic,
        shot_zone_area,
        shot_zone_range,
        shot_distance,
        shot_value,
        shot_made_flag,
        attempt_quality_bucket,
        attempt_quality_score,
        expected_points,
        actual_points,
        shot_making_value
      ) |>
      clean_table_names() |>
      make_dt(page_length = 15)
  })
  
  
  # Leaderboards --------------------------------------------------------------
  
  leaderboard_cache <- reactive({
    req(input$leaderboard_season, input$leaderboard_season_type)
    
    get_cache_by_selection(
      cache_bundle = cache_bundle,
      season = input$leaderboard_season,
      season_type_key = input$leaderboard_season_type
    )
  })
  
  output$leaderboard_table <- DT::renderDT({
    table <- leaderboard_cache() |>
      get_leaderboard(
        leaderboard_key = input$leaderboard_key,
        n = input$leaderboard_n
      ) |>
      format_leaderboard_table()
    
    dt <- DT::datatable(
      table,
      rownames = FALSE,
      class = "compact stripe hover nowrap",
      options = list(
        pageLength = input$leaderboard_n,
        scrollX = TRUE,
        autoWidth = FALSE,
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      )
    )
    
    if ("Avg AQ Score" %in% names(table)) {
      dt <- dt |>
        DT::formatStyle(
          "Avg AQ Score",
          backgroundColor = DT::styleInterval(
            c(5.5, 6.25, 7.0, 7.75),
            c("#f8d7da", "#fde9b9", "#fff8d6", "#d8f0dc", "#bfe7c5")
          )
        )
    }
    
    if ("Expected Points Per Attempt" %in% names(table)) {
      dt <- dt |>
        DT::formatStyle(
          "Expected Points Per Attempt",
          backgroundColor = DT::styleInterval(
            c(0.85, 0.95, 1.05, 1.15),
            c("#f8d7da", "#fde9b9", "#fff8d6", "#d8f0dc", "#bfe7c5")
          )
        )
    }
    
    if ("Shot Making Per Attempt" %in% names(table)) {
      dt <- dt |>
        DT::formatStyle(
          "Shot Making Per Attempt",
          backgroundColor = DT::styleInterval(
            c(-0.10, -0.025, 0.025, 0.10),
            c("#f4cccc", "#fce5cd", "#ffffff", "#d9ead3", "#b6d7a8")
          )
        )
    }
    
    if ("Avg Possession Quality" %in% names(table)) {
      dt <- dt |>
        DT::formatStyle(
          "Avg Possession Quality",
          backgroundColor = DT::styleInterval(
            c(0.45, 0.60, 0.75, 0.90),
            c("#f8d7da", "#fde9b9", "#fff8d6", "#d8f0dc", "#bfe7c5")
          )
        )
    }
    
    if ("PQ Per Game" %in% names(table)) {
      dt <- dt |>
        DT::formatStyle(
          "PQ Per Game",
          backgroundColor = DT::styleInterval(
            c(10, 20, 30, 40),
            c("#f8d7da", "#fde9b9", "#fff8d6", "#d8f0dc", "#bfe7c5")
          )
        )
    }
    
    if ("AQ Plus PQ Per Game" %in% names(table)) {
      dt <- dt |>
        DT::formatStyle(
          "AQ Plus PQ Per Game",
          backgroundColor = DT::styleInterval(
            c(10, 20, 30, 40),
            c("#f8d7da", "#fde9b9", "#fff8d6", "#d8f0dc", "#bfe7c5")
          )
        )
    }
    
    dt
  })
  
  
  # Data Status ---------------------------------------------------------------
  
  output$regular_metadata <- DT::renderDT({
    caches$regular$metadata |>
      dplyr::mutate(
        built_at = as.character(built_at)
      ) |>
      clean_table_names() |>
      make_dt(page_length = 1)
  })
  
  output$playoff_metadata <- DT::renderDT({
    caches$playoffs$metadata |>
      dplyr::mutate(
        built_at = as.character(built_at)
      ) |>
      clean_table_names() |>
      make_dt(page_length = 1)
  })
}