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

audit_shot_profile_accuracy <- function(cache = load_app_cache()) {
  
  player_ids <- cache$player_lookup$player_id
  
  box_totals <- cache$game_logs |>
    dplyr::filter(.data$player_id %in% player_ids) |>
    dplyr::group_by(player_id, player_name) |>
    dplyr::summarise(
      box_games = dplyr::n_distinct(game_id),
      box_fga = sum(fga, na.rm = TRUE),
      box_3pa = sum(fg3a, na.rm = TRUE),
      box_pts = sum(pts, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(full_player_name = player_name)
  
  pbp_totals <- cache$shot_events |>
    dplyr::filter(.data$player_id %in% player_ids) |>
    dplyr::group_by(player_id) |>
    dplyr::summarise(
      pbp_name = dplyr::first(player_name),
      pbp_fga = dplyr::n(),
      pbp_3pa = sum(shot_value == 3, na.rm = TRUE),
      pbp_rim_5ft_attempts = sum(shot_distance <= 5, na.rm = TRUE),
      pbp_avg_shot_distance = mean(shot_distance, na.rm = TRUE),
      .groups = "drop"
    )
  
  box_totals |>
    dplyr::left_join(
      pbp_totals,
      by = "player_id"
    ) |>
    dplyr::mutate(
      fga_diff = pbp_fga - box_fga,
      three_pa_diff = pbp_3pa - box_3pa,
      
      box_three_rate = box_3pa / box_fga,
      pbp_three_rate = pbp_3pa / pbp_fga,
      three_rate_diff = pbp_three_rate - box_three_rate,
      
      pbp_rim_5ft_rate = pbp_rim_5ft_attempts / pbp_fga
    ) |>
    dplyr::select(
      player_id,
      player_name = full_player_name,
      pbp_name,
      box_games,
      box_fga,
      pbp_fga,
      fga_diff,
      box_3pa,
      pbp_3pa,
      three_pa_diff,
      box_three_rate,
      pbp_three_rate,
      three_rate_diff,
      pbp_rim_5ft_rate,
      pbp_avg_shot_distance
    ) |>
    dplyr::arrange(
      dplyr::desc(abs(fga_diff)),
      dplyr::desc(abs(three_pa_diff))
    )
}

check_cache_status <- function(cache = load_app_cache()) {
  tibble::tibble(
    object = names(cache),
    class = purrr::map_chr(cache, ~ class(.x)[1]),
    rows = purrr::map_int(cache, ~ {
      if (is.data.frame(.x)) nrow(.x) else NA_integer_
    }),
    columns = purrr::map_int(cache, ~ {
      if (is.data.frame(.x)) ncol(.x) else NA_integer_
    })
  )
}
