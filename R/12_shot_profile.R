# Shot profile summaries -------------------------------------------------------
# Adds context to Attempt Quality / Shot Quality scores.
# These columns explain whether a player's score is driven by rim attempts,
# threes, midrange shots, shot distance, or grenade attempts.

build_shot_profile_summary <- function(scored_shots) {
  
  if (nrow(scored_shots) == 0) {
    return(tibble::tibble())
  }
  
  scored_shots |>
    dplyr::group_by(player_id, player_name) |>
    dplyr::summarise(
      fga_from_pbp = dplyr::n(),
      
      grenade_attempts = sum(is_grenade, na.rm = TRUE),
      non_grenade_fga = sum(!is_grenade, na.rm = TRUE),
      
      rim_attempts = sum(shot_distance <= 5, na.rm = TRUE),
      midrange_attempts = sum(
        shot_value == 2 & shot_distance > 5 & shot_distance < 23,
        na.rm = TRUE
      ),
      three_attempts = sum(shot_value == 3, na.rm = TRUE),
      
      rim_rate = rim_attempts / fga_from_pbp,
      midrange_rate = midrange_attempts / fga_from_pbp,
      three_rate = three_attempts / fga_from_pbp,
      grenade_rate = grenade_attempts / fga_from_pbp,
      
      avg_shot_distance = mean(shot_distance, na.rm = TRUE),
      
      bucket_9_rate = mean(as.character(sq_bucket) == "9", na.rm = TRUE),
      bucket_7_rate = mean(as.character(sq_bucket) == "7", na.rm = TRUE),
      bucket_5_rate = mean(as.character(sq_bucket) == "5", na.rm = TRUE),
      bucket_3_rate = mean(as.character(sq_bucket) == "3", na.rm = TRUE),
      bucket_1_rate = mean(as.character(sq_bucket) == "1", na.rm = TRUE),
      
      .groups = "drop"
    )
}