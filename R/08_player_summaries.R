# Player summaries -------------------------------------------------------------

build_shot_game_summary <- function(scored_shots) {
  if (nrow(scored_shots) == 0) return(tibble::tibble())
  
  scored_shots |>
    dplyr::mutate(
      sq_bucket = as.character(.data$sq_bucket),
      is_non_grenade = .data$sq_bucket != "Grenade"
    ) |>
    dplyr::group_by(.data$player_id, .data$game_id) |>
    dplyr::summarise(
      fga_from_pbp = dplyr::n(),
      non_grenade_fga = sum(.data$is_non_grenade, na.rm = TRUE),
      grenade_attempts = sum(.data$sq_bucket == "Grenade", na.rm = TRUE),
      
      avg_sq_score = dplyr::if_else(
        sum(.data$is_non_grenade, na.rm = TRUE) > 0,
        mean(.data$sq_score[.data$is_non_grenade], na.rm = TRUE),
        NA_real_
      ),
      
      shot_making_score = sum(.data$shot_making_value, na.rm = TRUE),
      actual_points_from_shots = sum(.data$actual_points, na.rm = TRUE),
      expected_points_from_shots = sum(.data$expected_points, na.rm = TRUE),
      
      .groups = "drop"
    )
}

build_player_summary <- function(game_summary) {
  if (nrow(game_summary) == 0) return(tibble::tibble())
  
  game_summary |>
    dplyr::mutate(
      fga_weight = dplyr::if_else(is.na(.data$fga) | .data$fga <= 0, 1, .data$fga),
      valid_sq = !is.na(.data$avg_sq_score)
    ) |>
    dplyr::group_by(.data$player_id, .data$player_name) |>
    dplyr::summarise(
      games = dplyr::n_distinct(.data$game_id),
      fga = sum(.data$fga, na.rm = TRUE),
      pts = sum(.data$pts, na.rm = TRUE),
      ast = sum(.data$ast, na.rm = TRUE),
      tov = sum(.data$tov, na.rm = TRUE),
      
      avg_sq_score = dplyr::if_else(
        sum(.data$valid_sq, na.rm = TRUE) > 0,
        weighted.mean(
          .data$avg_sq_score[.data$valid_sq],
          w = .data$fga_weight[.data$valid_sq],
          na.rm = TRUE
        ),
        NA_real_
      ),
      
      avg_shot_making_score = mean(.data$shot_making_score, na.rm = TRUE),
      total_shot_making_score = sum(.data$shot_making_score, na.rm = TRUE),
      avg_pq_score = mean(.data$pq_score, na.rm = TRUE),
      total_pq_score = sum(.data$pq_score, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$avg_pq_score))
}

get_player_games <- function(game_summary, selected_player_id, last_n = 10) {
  if (nrow(game_summary) == 0 || is.null(selected_player_id) || selected_player_id == "") {
    return(tibble::tibble())
  }
  
  game_summary |>
    dplyr::filter(.data$player_id == as.character(.env$selected_player_id)) |>
    dplyr::arrange(dplyr::desc(.data$game_date)) |>
    dplyr::slice_head(n = last_n) |>
    dplyr::arrange(.data$game_date)
}