# Cache validation checks ------------------------------------------------------
# Use this after building a cache to confirm the app has the expected data shape.

source("R/00_config.R")
source("R/01_helpers.R")
source("R/07_cache_builder.R")

check_cache <- function(cache = load_app_cache()) {
  
  checks <- tibble::tibble(
    check = c(
      "metadata exists",
      "player lookup has rows",
      "game logs have rows",
      "shot events have rows",
      "game summary has rows",
      "player summary has rows",
      "game logs include player_id",
      "game logs include player_name",
      "game logs include game_id",
      "shot events include player_id",
      "shot events include game_id",
      "shot events include sq_bucket",
      "shot events include sq_score",
      "game summary includes avg_sq_score",
      "game summary includes pq_score",
      "player summary includes avg_sq_score",
      "player summary includes avg_pq_score"
    ),
    passed = c(
      length(cache$metadata) > 0,
      nrow(cache$player_lookup) > 0,
      nrow(cache$game_logs) > 0,
      nrow(cache$shot_events) > 0,
      nrow(cache$game_summary) > 0,
      nrow(cache$player_summary) > 0,
      "player_id" %in% names(cache$game_logs),
      "player_name" %in% names(cache$game_logs),
      "game_id" %in% names(cache$game_logs),
      "player_id" %in% names(cache$shot_events),
      "game_id" %in% names(cache$shot_events),
      "sq_bucket" %in% names(cache$shot_events),
      "sq_score" %in% names(cache$shot_events),
      "avg_sq_score" %in% names(cache$game_summary),
      "pq_score" %in% names(cache$game_summary),
      "avg_sq_score" %in% names(cache$player_summary),
      "avg_pq_score" %in% names(cache$player_summary)
    )
  )
  
  checks
}

summarize_cache <- function(cache = load_app_cache()) {
  
  list(
    metadata = cache$metadata,
    
    player_summary = cache$player_summary |>
      dplyr::select(
        player_id, player_name, games, fga, pts, ast, tov,
        avg_sq_score, avg_shot_making_score, avg_pq_score
      ) |>
      dplyr::arrange(dplyr::desc(avg_pq_score)),
    
    games_by_player = cache$game_summary |>
      dplyr::count(player_name, sort = TRUE),
    
    selected_player_shot_events = cache$shot_events |>
      dplyr::semi_join(cache$player_lookup, by = "player_id") |>
      dplyr::count(player_name, sort = TRUE)
  )
}