# Player summaries -------------------------------------------------------------

build_player_summary <- function(game_summary) {
  if (nrow(game_summary) == 0) return(tibble::tibble())
  
  game_summary |>
    mutate(
      fga_weight = dplyr::if_else(is.na(fga) | fga <= 0, 1, fga),
      valid_sq = !is.na(avg_sq_score)
    ) |>
    group_by(player_id, player_name) |>
    summarise(
      games = n_distinct(game_id),
      fga = sum(fga, na.rm = TRUE),
      pts = sum(pts, na.rm = TRUE),
      ast = sum(ast, na.rm = TRUE),
      tov = sum(tov, na.rm = TRUE),
      
      avg_sq_score = dplyr::if_else(
        sum(valid_sq, na.rm = TRUE) > 0,
        weighted.mean(
          avg_sq_score[valid_sq],
          w = fga_weight[valid_sq],
          na.rm = TRUE
        ),
        NA_real_
      ),
      
      avg_shot_making_score = mean(shot_making_score, na.rm = TRUE),
      avg_pq_score = mean(pq_score, na.rm = TRUE),
      total_pq_score = sum(pq_score, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(avg_pq_score))
}

get_player_games <- function(game_summary, player_id, last_n = 10) {
  if (nrow(game_summary) == 0 || is.null(player_id) || player_id == "") {
    return(tibble::tibble())
  }

  game_summary |>
    filter(.data$player_id == as.character(player_id)) |>
    arrange(desc(game_date)) |>
    slice_head(n = last_n) |>
    arrange(game_date)
}
