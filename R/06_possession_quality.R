# Possession Quality -----------------------------------------------------------
# Starter version: transparent placeholder score using available box-score data.
# Future version: add tracking stats, potential assists, fouls drawn, deflections,
# charges drawn, and calibrated league-average value.

calculate_possession_quality <- function(game_logs, shot_game_summary) {
  if (nrow(game_logs) == 0) return(tibble::tibble())
  
  # Avoid duplicate player_name columns after join.
  # game_logs is the preferred source for player_name.
  shot_game_summary_clean <- shot_game_summary |>
    select(-any_of("player_name"))
  
  game_logs |>
    left_join(
      shot_game_summary_clean,
      by = c("player_id", "game_id")
    ) |>
    mutate(
      avg_sq_score = coalesce(avg_sq_score, NA_real_),
      shot_making_score = coalesce(shot_making_score, 0),
      
      pq_box_score_component =
        PQ_WEIGHTS$ast * coalesce(ast, 0) +
        PQ_WEIGHTS$tov * coalesce(tov, 0) +
        PQ_WEIGHTS$stl * coalesce(stl, 0) +
        PQ_WEIGHTS$blk * coalesce(blk, 0) +
        PQ_WEIGHTS$oreb * coalesce(oreb, 0) +
        PQ_WEIGHTS$pf * coalesce(pf, 0) +
        PQ_WEIGHTS$fouls_drawn * coalesce(pfd, 0),
      
      pq_shot_process_component = coalesce(avg_sq_score, 0) * coalesce(fga, 0) / 10,
      
      pq_score = pq_box_score_component + pq_shot_process_component + shot_making_score
    )
}

build_shot_game_summary <- function(scored_shots) {
  if (nrow(scored_shots) == 0) return(tibble::tibble())

  scored_shots |>
    group_by(player_id, player_name, game_id) |>
    summarise(
      fga_from_pbp = n(),
      non_prayer_fga = sum(!is_prayer, na.rm = TRUE),
      prayer_attempts = sum(is_prayer, na.rm = TRUE),
      avg_sq_score = mean(sq_score[!is_prayer], na.rm = TRUE),
      shot_making_score = sum(shot_making_score, na.rm = TRUE),
      .groups = "drop"
    )
}
