# R/07_build_player_outputs.R
# Builds player-game and player-level summaries from scored Attempt Quality shots.

required_pkgs <- c("tidyverse", "readr", "stringr", "glue")
invisible(lapply(required_pkgs, require, character.only = TRUE))


# Paths ------------------------------------------------------------------------

PROCESSED_MODEL_DIR <- "data/processed/model_outputs"
PROCESSED_PLAYER_DIR <- "data/processed/players"

dir.create(PROCESSED_PLAYER_DIR, recursive = TRUE, showWarnings = FALSE)


# Helpers ----------------------------------------------------------------------

season_to_data_year <- function(season) {
  as.integer(stringr::str_sub(season, 1, 4))
}

season_type_file_label <- function(season_type) {
  if (season_type == "Regular Season") {
    "regular"
  } else if (season_type == "Playoffs") {
    "playoffs"
  } else {
    stop("Unsupported season_type: ", season_type)
  }
}

build_scored_shots_path <- function(season, season_type) {
  data_year <- season_to_data_year(season)
  type_label <- season_type_file_label(season_type)
  
  file.path(
    PROCESSED_MODEL_DIR,
    paste0("scored_shots_", data_year, "_", type_label, ".rds")
  )
}

build_player_output_path <- function(prefix, season, season_type) {
  data_year <- season_to_data_year(season)
  type_label <- season_type_file_label(season_type)
  
  file.path(
    PROCESSED_PLAYER_DIR,
    paste0(prefix, "_", data_year, "_", type_label, ".rds")
  )
}


# Builders ---------------------------------------------------------------------

build_player_game_summary <- function(scored_shots) {
  scored_shots |>
    dplyr::mutate(
      is_non_grenade = .data$aq_bucket != "Grenade",
      is_made = .data$shot_result == "Made"
    ) |>
    dplyr::group_by(
      .data$season,
      .data$season_type,
      .data$game_id,
      .data$game_date,
      .data$htm,
      .data$vtm,
      .data$team_id,
      .data$team_tricode,
      .data$player_id,
      .data$player_name
    ) |>
    dplyr::summarise(
      fga = dplyr::n(),
      fgm = sum(.data$is_made, na.rm = TRUE),
      three_pa = sum(.data$is_three, na.rm = TRUE),
      three_pm = sum(.data$is_three & .data$is_made, na.rm = TRUE),
      pts_from_shots = sum(.data$actual_points, na.rm = TRUE),
      
      non_grenade_fga = sum(.data$is_non_grenade, na.rm = TRUE),
      grenade_attempts = sum(.data$aq_bucket == "Grenade", na.rm = TRUE),
      aq_score_sum = sum(.data$aq_score[.data$is_non_grenade], na.rm = TRUE),
      
      avg_aq_score = dplyr::if_else(
        sum(.data$is_non_grenade, na.rm = TRUE) > 0,
        sum(.data$aq_score[.data$is_non_grenade], na.rm = TRUE) /
          sum(.data$is_non_grenade, na.rm = TRUE),
        NA_real_
      ),
      
      expected_points = sum(.data$expected_points, na.rm = TRUE),
      shot_making_value = sum(.data$shot_making_value, na.rm = TRUE),
      
      rim_attempts = sum(.data$is_restricted_area, na.rm = TRUE),
      paint_non_ra_attempts = sum(.data$is_paint_non_ra, na.rm = TRUE),
      midrange_attempts = sum(.data$is_midrange, na.rm = TRUE),
      corner_three_attempts = sum(.data$is_corner_three, na.rm = TRUE),
      above_break_three_attempts = sum(.data$is_above_break_three, na.rm = TRUE),
      
      bucket_9_attempts = sum(.data$aq_bucket == "9", na.rm = TRUE),
      bucket_7_attempts = sum(.data$aq_bucket == "7", na.rm = TRUE),
      bucket_5_attempts = sum(.data$aq_bucket == "5", na.rm = TRUE),
      bucket_3_attempts = sum(.data$aq_bucket == "3", na.rm = TRUE),
      bucket_1_attempts = sum(.data$aq_bucket == "1", na.rm = TRUE),
      
      avg_shot_distance = mean(.data$final_shot_distance, na.rm = TRUE),
      
      .groups = "drop"
    ) |>
    dplyr::mutate(
      fg_pct = dplyr::if_else(.data$fga > 0, .data$fgm / .data$fga, NA_real_),
      three_pct = dplyr::if_else(.data$three_pa > 0, .data$three_pm / .data$three_pa, NA_real_),
      efg_pct = dplyr::if_else(
        .data$fga > 0,
        (.data$fgm + 0.5 * .data$three_pm) / .data$fga,
        NA_real_
      ),
      
      points_per_attempt = dplyr::if_else(
        .data$fga > 0,
        .data$pts_from_shots / .data$fga,
        NA_real_
      ),
      
      expected_points_per_attempt = dplyr::if_else(
        .data$fga > 0,
        .data$expected_points / .data$fga,
        NA_real_
      ),
      
      shot_making_per_attempt = dplyr::if_else(
        .data$fga > 0,
        .data$shot_making_value / .data$fga,
        NA_real_
      ),
      
      rim_rate = dplyr::if_else(.data$fga > 0, .data$rim_attempts / .data$fga, NA_real_),
      paint_non_ra_rate = dplyr::if_else(.data$fga > 0, .data$paint_non_ra_attempts / .data$fga, NA_real_),
      midrange_rate = dplyr::if_else(.data$fga > 0, .data$midrange_attempts / .data$fga, NA_real_),
      corner_three_rate = dplyr::if_else(.data$fga > 0, .data$corner_three_attempts / .data$fga, NA_real_),
      above_break_three_rate = dplyr::if_else(.data$fga > 0, .data$above_break_three_attempts / .data$fga, NA_real_),
      three_rate = dplyr::if_else(.data$fga > 0, .data$three_pa / .data$fga, NA_real_),
      grenade_rate = dplyr::if_else(.data$fga > 0, .data$grenade_attempts / .data$fga, NA_real_)
    ) |>
    dplyr::arrange(.data$game_date, .data$game_id, .data$team_tricode, .data$player_name)
}


build_player_summary <- function(player_game_summary) {
  player_game_summary |>
    dplyr::group_by(
      .data$season,
      .data$season_type,
      .data$player_id,
      .data$player_name,
      .data$team_id,
      .data$team_tricode
    ) |>
    dplyr::summarise(
      games = dplyr::n_distinct(.data$game_id),
      fga = sum(.data$fga, na.rm = TRUE),
      fgm = sum(.data$fgm, na.rm = TRUE),
      three_pa = sum(.data$three_pa, na.rm = TRUE),
      three_pm = sum(.data$three_pm, na.rm = TRUE),
      pts_from_shots = sum(.data$pts_from_shots, na.rm = TRUE),
      
      non_grenade_fga = sum(.data$non_grenade_fga, na.rm = TRUE),
      grenade_attempts = sum(.data$grenade_attempts, na.rm = TRUE),
      
      aq_score_sum = sum(.data$aq_score_sum, na.rm = TRUE),
      
      shot_distance_weighted_sum = sum(
        .data$avg_shot_distance * .data$fga,
        na.rm = TRUE
      ),
      
      expected_points = sum(.data$expected_points, na.rm = TRUE),
      shot_making_value = sum(.data$shot_making_value, na.rm = TRUE),
      
      rim_attempts = sum(.data$rim_attempts, na.rm = TRUE),
      paint_non_ra_attempts = sum(.data$paint_non_ra_attempts, na.rm = TRUE),
      midrange_attempts = sum(.data$midrange_attempts, na.rm = TRUE),
      corner_three_attempts = sum(.data$corner_three_attempts, na.rm = TRUE),
      above_break_three_attempts = sum(.data$above_break_three_attempts, na.rm = TRUE),
      
      bucket_9_attempts = sum(.data$bucket_9_attempts, na.rm = TRUE),
      bucket_7_attempts = sum(.data$bucket_7_attempts, na.rm = TRUE),
      bucket_5_attempts = sum(.data$bucket_5_attempts, na.rm = TRUE),
      bucket_3_attempts = sum(.data$bucket_3_attempts, na.rm = TRUE),
      bucket_1_attempts = sum(.data$bucket_1_attempts, na.rm = TRUE),
      
      .groups = "drop"
    ) |>
    dplyr::mutate(
      avg_aq_score = dplyr::if_else(
        .data$non_grenade_fga > 0,
        .data$aq_score_sum / .data$non_grenade_fga,
        NA_real_
      ),
      
      avg_shot_distance = dplyr::if_else(
        .data$fga > 0,
        .data$shot_distance_weighted_sum / .data$fga,
        NA_real_
      ),
      
      fg_pct = dplyr::if_else(.data$fga > 0, .data$fgm / .data$fga, NA_real_),
      three_pct = dplyr::if_else(.data$three_pa > 0, .data$three_pm / .data$three_pa, NA_real_),
      efg_pct = dplyr::if_else(
        .data$fga > 0,
        (.data$fgm + 0.5 * .data$three_pm) / .data$fga,
        NA_real_
      ),
      
      points_per_attempt = dplyr::if_else(
        .data$fga > 0,
        .data$pts_from_shots / .data$fga,
        NA_real_
      ),
      
      expected_points_per_attempt = dplyr::if_else(
        .data$fga > 0,
        .data$expected_points / .data$fga,
        NA_real_
      ),
      
      shot_making_per_attempt = dplyr::if_else(
        .data$fga > 0,
        .data$shot_making_value / .data$fga,
        NA_real_
      ),
      
      rim_rate = dplyr::if_else(.data$fga > 0, .data$rim_attempts / .data$fga, NA_real_),
      paint_non_ra_rate = dplyr::if_else(.data$fga > 0, .data$paint_non_ra_attempts / .data$fga, NA_real_),
      midrange_rate = dplyr::if_else(.data$fga > 0, .data$midrange_attempts / .data$fga, NA_real_),
      corner_three_rate = dplyr::if_else(.data$fga > 0, .data$corner_three_attempts / .data$fga, NA_real_),
      above_break_three_rate = dplyr::if_else(.data$fga > 0, .data$above_break_three_attempts / .data$fga, NA_real_),
      three_rate = dplyr::if_else(.data$fga > 0, .data$three_pa / .data$fga, NA_real_),
      grenade_rate = dplyr::if_else(.data$fga > 0, .data$grenade_attempts / .data$fga, NA_real_),
      
      bucket_9_rate = dplyr::if_else(.data$fga > 0, .data$bucket_9_attempts / .data$fga, NA_real_),
      bucket_7_rate = dplyr::if_else(.data$fga > 0, .data$bucket_7_attempts / .data$fga, NA_real_),
      bucket_5_rate = dplyr::if_else(.data$fga > 0, .data$bucket_5_attempts / .data$fga, NA_real_),
      bucket_3_rate = dplyr::if_else(.data$fga > 0, .data$bucket_3_attempts / .data$fga, NA_real_),
      bucket_1_rate = dplyr::if_else(.data$fga > 0, .data$bucket_1_attempts / .data$fga, NA_real_)
    ) |>
    dplyr::select(
      -aq_score_sum,
      -shot_distance_weighted_sum
    ) |>
    dplyr::arrange(dplyr::desc(.data$fga))
}


build_player_outputs <- function(
    season = "2024-25",
    season_type = "Regular Season",
    write_csv = TRUE,
    force_rebuild = TRUE
) {
  scored_path <- build_scored_shots_path(season, season_type)
  
  if (!file.exists(scored_path)) {
    stop("Missing scored shots file: ", scored_path)
  }
  
  player_game_out <- build_player_output_path("player_game_summary", season, season_type)
  player_summary_out <- build_player_output_path("player_summary", season, season_type)
  
  if (!force_rebuild && file.exists(player_game_out) && file.exists(player_summary_out)) {
    message("Already built player outputs for ", season, " / ", season_type)
    return(
      list(
        player_game_summary = readr::read_rds(player_game_out),
        player_summary = readr::read_rds(player_summary_out)
      )
    )
  }
  
  message("\nBuilding player outputs")
  message("- Season: ", season)
  message("- Season type: ", season_type)
  
  scored_shots <- readr::read_rds(scored_path)
  
  player_game_summary <- build_player_game_summary(scored_shots)
  player_summary <- build_player_summary(player_game_summary)

  qa_summary <- list(
    player_games = player_game_summary |>
      dplyr::summarise(
        rows = dplyr::n(),
        games = dplyr::n_distinct(.data$game_id),
        players = dplyr::n_distinct(.data$player_id),
        total_fga = sum(.data$fga, na.rm = TRUE),
        total_expected_points = sum(.data$expected_points, na.rm = TRUE),
        total_shot_making_value = sum(.data$shot_making_value, na.rm = TRUE)
      ),
    
    players = player_summary |>
      dplyr::summarise(
        rows = dplyr::n(),
        players = dplyr::n_distinct(.data$player_id),
        total_fga = sum(.data$fga, na.rm = TRUE),
        avg_aq_score_weighted = sum(
          .data$avg_aq_score * .data$non_grenade_fga,
          na.rm = TRUE
        ) / sum(.data$non_grenade_fga, na.rm = TRUE)
      )
  )
  
  message("\nPlayer-game QA:")
  print(qa_summary$player_games)
  
  message("\nPlayer summary QA:")
  print(qa_summary$players)
  
  message("\nTop FGA players:")
  print(
    player_summary |>
      dplyr::select(
        player_name,
        team_tricode,
        games,
        fga,
        avg_aq_score,
        expected_points_per_attempt,
        points_per_attempt,
        shot_making_value,
        rim_rate,
        three_rate,
        grenade_rate
      ) |>
      dplyr::slice_head(n = 20),
    n = 20
  )
  
  readr::write_rds(player_game_summary, player_game_out)
  readr::write_rds(player_summary, player_summary_out)
  
  if (write_csv) {
    readr::write_csv(
      player_game_summary,
      stringr::str_replace(player_game_out, "\\.rds$", ".csv")
    )
    
    readr::write_csv(
      player_summary,
      stringr::str_replace(player_summary_out, "\\.rds$", ".csv")
    )
  }
  
  message("\nSaved player outputs:")
  message("- ", player_game_out)
  message("- ", player_summary_out)
  
  invisible(
    list(
      player_game_summary = player_game_summary,
      player_summary = player_summary,
      qa_summary = qa_summary
    )
  )
}