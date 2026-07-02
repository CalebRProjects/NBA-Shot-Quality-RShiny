# R/app_helpers.R
# Helper functions for loading app cache, searching players, formatting tables,
# and pulling leaderboard data.

required_pkgs <- c(
  "shiny",
  "tidyverse",
  "readr",
  "stringr",
  "DT",
  "scales",
  "glue"
)

invisible(lapply(required_pkgs, require, character.only = TRUE))


# Cache loading ---------------------------------------------------------------

CACHE_DIR <- "data/cache"

parse_cache_file <- function(path) {
  file_name <- basename(path)
  
  parsed <- stringr::str_match(
    file_name,
    "^app_cache_([0-9]{4})_(regular|playoffs)\\.rds$"
  )
  
  tibble::tibble(
    path = path,
    data_year = as.integer(parsed[, 2]),
    season_type_key = parsed[, 3],
    season = paste0(parsed[, 2], "-", stringr::str_sub(as.character(as.integer(parsed[, 2]) + 1), 3, 4)),
    season_label = paste0(parsed[, 2], "-", stringr::str_sub(as.character(as.integer(parsed[, 2]) + 1), 3, 4)),
    cache_key = paste0(season, "__", season_type_key)
  )
}

discover_app_caches <- function() {
  cache_files <- list.files(
    CACHE_DIR,
    pattern = "^app_cache_[0-9]{4}_(regular|playoffs)\\.rds$",
    full.names = TRUE
  )
  
  if (length(cache_files) == 0) {
    stop("No app cache files found in: ", CACHE_DIR)
  }
  
  purrr::map_dfr(cache_files, parse_cache_file) |>
    dplyr::filter(!is.na(.data$data_year)) |>
    dplyr::arrange(dplyr::desc(.data$data_year), .data$season_type_key)
}

load_app_caches <- function() {
  cache_index <- discover_app_caches()
  
  caches <- cache_index |>
    dplyr::mutate(cache = purrr::map(.data$path, readr::read_rds)) |>
    dplyr::select(cache_key, cache)
  
  cache_list <- stats::setNames(caches$cache, caches$cache_key)
  
  list(
    index = cache_index,
    caches = cache_list
  )
}

get_cache_by_selection <- function(cache_bundle, season, season_type_key) {
  cache_key <- paste0(season, "__", season_type_key)
  
  if (!cache_key %in% names(cache_bundle$caches)) {
    stop("Unknown cache selection: ", cache_key)
  }
  
  cache_bundle$caches[[cache_key]]
}

get_available_seasons <- function(cache_bundle) {
  cache_bundle$index |>
    dplyr::distinct(.data$season_label, .data$season) |>
    dplyr::arrange(dplyr::desc(.data$season)) |>
    dplyr::pull(.data$season, .data$season_label)
}


# Player search ---------------------------------------------------------------

get_player_choices <- function(cache) {
  choices_df <- cache$player_lookup |>
    dplyr::mutate(
      value = paste(player_id, team_tricode, sep = "__")
    ) |>
    dplyr::arrange(player_name, team_tricode)
  
  stats::setNames(
    choices_df$value,
    choices_df$search_label
  )
}


split_player_choice <- function(player_choice) {
  parts <- stringr::str_split_fixed(player_choice, "__", 2)
  
  tibble::tibble(
    player_id = parts[, 1],
    team_tricode = parts[, 2]
  )
}


get_player_detail <- function(cache, player_choice) {
  selected <- split_player_choice(player_choice)
  
  cache$player_detail |>
    dplyr::filter(
      player_id == selected$player_id,
      team_tricode == selected$team_tricode
    )
}


get_player_game_log <- function(cache, player_choice, last_n_games = 10) {
  selected <- split_player_choice(player_choice)
  
  cache$player_game_log |>
    dplyr::filter(
      player_id == selected$player_id,
      team_tricode == selected$team_tricode
    ) |>
    dplyr::arrange(dplyr::desc(game_date)) |>
    dplyr::slice_head(n = last_n_games)
}


get_player_shots <- function(cache, player_choice, last_n_games = 10) {
  selected <- split_player_choice(player_choice)
  
  player_games <- cache$player_game_log |>
    dplyr::filter(
      player_id == selected$player_id,
      team_tricode == selected$team_tricode
    ) |>
    dplyr::arrange(dplyr::desc(game_date)) |>
    dplyr::slice_head(n = last_n_games) |>
    dplyr::pull(game_id)
  
  cache$scored_shots |>
    dplyr::filter(
      player_id == selected$player_id,
      team_tricode == selected$team_tricode,
      game_id %in% player_games
    ) |>
    dplyr::arrange(dplyr::desc(game_date), period, game_event_id)
}


# Leaderboards ----------------------------------------------------------------

leaderboard_labels <- c(
  avg_attempt_quality = "Attempt Quality",
  possession_quality_per_event = "Possession Quality / Event",
  shot_making_per_attempt = "Shot Making / FGA",
  combined_value_per_game = "Combined Value / Game",
  expected_points_per_attempt = "Expected Pts / FGA",
  possession_quality_total = "Total Possession Quality",
  shot_making_total = "Total Shot Making",
  combined_value_total = "Total Combined Value",
  assist_creation = "Assist Creation",
  rim_rate = "Rim Rate",
  three_rate = "Three-Point Rate"
)


get_leaderboard <- function(cache, leaderboard_key, n = 25) {
  if (!leaderboard_key %in% names(cache$leaderboards)) {
    stop("Unknown leaderboard: ", leaderboard_key)
  }
  
  cache$leaderboards[[leaderboard_key]] |>
    dplyr::slice_head(n = n)
}


# Formatting ------------------------------------------------------------------

fmt_number <- function(x, digits = 1) {
  scales::number(x, accuracy = 10^-digits)
}


fmt_percent <- function(x, digits = 1) {
  scales::percent(x, accuracy = 10^-digits)
}


fmt_int <- function(x) {
  scales::comma(x, accuracy = 1)
}


clean_table_names <- function(data) {
  data |>
    dplyr::rename_with(
      ~ .x |>
        stringr::str_replace_all("_", " ") |>
        stringr::str_to_title() |>
        stringr::str_replace_all("\\bAq\\b", "AQ") |>
        stringr::str_replace_all("\\bPq\\b", "PQ") |>
        stringr::str_replace_all("\\bFga\\b", "FGA") |>
        stringr::str_replace_all("\\bFgm\\b", "FGM") |>
        stringr::str_replace_all("\\bFg\\b", "FG") |>
        stringr::str_replace_all("\\bEfg\\b", "eFG") |>
        stringr::str_replace_all("\\bPts\\b", "PTS") |>
        stringr::str_replace_all("\\bId\\b", "ID") |>
        stringr::str_replace("^Avg AQ Score$", "Attempt Quality") |>
        stringr::str_replace("^Expected Points Per Attempt$", "Expected Pts / FGA") |>
        stringr::str_replace("^Shot Making Per Attempt$", "Shot Making / FGA") |>
        stringr::str_replace("^Avg Possession Quality$", "Possession Quality / Event") |>
        stringr::str_replace("^Possession Quality Value$", "Total PQ") |>
        stringr::str_replace("^PQ Per Game$", "PQ / Game") |>
        stringr::str_replace("^AQ Plus PQ Value$", "Total Value") |>
        stringr::str_replace("^AQ Plus PQ Per Game$", "Value / Game") |>
        stringr::str_replace("^Shot Making Value$", "Total Shot Making")
    )
}


format_player_summary_table <- function(data) {
  data |>
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ round(.x, 3)
      )
    ) |>
    clean_table_names()
}


format_game_log_table <- function(data) {
  data |>
    dplyr::mutate(
      Game = paste0(vtm, " @ ", htm),
      game_date = as.character(game_date),
      expected_points_per_attempt_game = dplyr::if_else(
        fga > 0,
        expected_points / fga,
        NA_real_
      ),
      shot_making_per_attempt_game = dplyr::if_else(
        fga > 0,
        shot_making_value / fga,
        NA_real_
      )
    ) |>
    dplyr::select(
      game_date,
      Game,
      fga,
      pts_from_shots,
      avg_aq_score,
      expected_points_per_attempt_game,
      shot_making_per_attempt_game,
      avg_possession_quality,
      aq_plus_pq_value,
      assists,
      turnovers,
      offensive_rebounds,
      defensive_rebounds,
      steals,
      blocks
    ) |>
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ round(.x, 3))
    ) |>
    dplyr::rename(
      `Game Date` = game_date,
      FGA = fga,
      PTS = pts_from_shots,
      `Attempt Quality` = avg_aq_score,
      `Expected Pts / FGA` = expected_points_per_attempt_game,
      `Shot Making / FGA` = shot_making_per_attempt_game,
      `Possession Quality` = avg_possession_quality,
      `Total Game Value` = aq_plus_pq_value,
      AST = assists,
      TOV = turnovers,
      OREB = offensive_rebounds,
      DREB = defensive_rebounds,
      STL = steals,
      BLK = blocks
    )
}


format_leaderboard_table <- function(data) {
  data |>
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ round(.x, 3)
      )
    ) |>
    clean_table_names()
}


# DT wrappers -----------------------------------------------------------------

make_dt <- function(data, page_length = 10) {
  DT::datatable(
    data,
    rownames = FALSE,
    class = "compact stripe hover nowrap",
    options = list(
      pageLength = page_length,
      scrollX = TRUE,
      autoWidth = FALSE,
      dom = "lfrtip",
      columnDefs = list(
        list(className = "dt-center", targets = "_all")
      )
    )
  )
}


# Small UI value helpers -------------------------------------------------------

value_box_text <- function(value, digits = 1) {
  if (length(value) == 0 || is.na(value)) {
    return("—")
  }
  
  fmt_number(value, digits = digits)
}


percent_box_text <- function(value, digits = 1) {
  if (length(value) == 0 || is.na(value)) {
    return("—")
  }
  
  fmt_percent(value, digits = digits)
}


player_headline_stats <- function(player_detail) {
  if (nrow(player_detail) == 0) {
    return(tibble::tibble())
  }
  
  player_detail |>
    dplyr::transmute(
      Player = paste0(player_name, " - ", team_tricode),
      Games = games,
      FGA = fga,
      `Avg AQ` = round(avg_aq_score, 2),
      `Shot Making` = round(shot_making_value, 1),
      `PQ Value` = round(possession_quality_value, 1),
      `Combined Value` = round(aq_plus_pq_value, 1),
      Assists = assists,
      Turnovers = turnovers,
      Steals = steals,
      Blocks = blocks
    )
}

safe_percent_rank <- function(x) {
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  
  dplyr::percent_rank(x)
}

add_player_percentiles <- function(data) {
  qualified <- data |>
    dplyr::mutate(
      shot_making_per_attempt = dplyr::if_else(
        fga > 0,
        shot_making_value / fga,
        NA_real_
      )
    ) |>
    dplyr::filter(
      fga >= dplyr::if_else(season_type == "Regular Season", 300, 50)
    ) |>
    dplyr::mutate(
      aq_percentile = safe_percent_rank(avg_aq_score),
      expected_points_percentile = safe_percent_rank(expected_points_per_attempt),
      shot_making_percentile = safe_percent_rank(shot_making_per_attempt),
      pq_percentile = safe_percent_rank(avg_possession_quality)
    ) |>
    dplyr::select(
      player_id,
      team_tricode,
      aq_percentile,
      expected_points_percentile,
      shot_making_percentile,
      pq_percentile
    )
  
  data |>
    dplyr::mutate(
      shot_making_per_attempt = dplyr::if_else(
        fga > 0,
        shot_making_value / fga,
        NA_real_
      )
    ) |>
    dplyr::left_join(
      qualified,
      by = c("player_id", "team_tricode")
    )
}

format_percentile_label <- function(x) {
  if (length(x) == 0 || is.na(x)) {
    return("—")
  }
  
  paste0(round(x * 100), "th pct")
}

format_signed_number <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x)) {
    return("—")
  }
  
  rounded <- round(x, digits)
  
  if (rounded > 0) {
    paste0("+", rounded)
  } else {
    as.character(rounded)
  }
}

stat_unit_ui <- function(label, value, percentile) {
  pct_width <- ifelse(is.na(percentile), 0, round(percentile * 100, 1))
  
  shiny::tags$div(
    class = "stat-unit",
    shiny::tags$div(class = "stat-label", label),
    shiny::tags$div(
      class = "stat-line",
      shiny::tags$span(class = "stat-value", value),
      shiny::tags$span(class = "stat-rank", format_percentile_label(percentile))
    ),
    shiny::tags$div(
      class = "stat-note",
      "Percentiles compare the player to qualified players in the selected season type."
    ),
    shiny::tags$div(
      class = "thin-bar",
      shiny::tags$div(
        class = "thin-bar-fill",
        style = paste0("width:", pct_width, "%;")
      )
    )
  )
}