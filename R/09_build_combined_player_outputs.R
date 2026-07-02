# R/09_build_combined_player_outputs.R
# Combines Attempt Quality / Shot Making outputs with Possession Quality outputs.

required_pkgs <- c("tidyverse", "readr", "stringr", "glue", "janitor")
invisible(lapply(required_pkgs, require, character.only = TRUE))


# Paths ------------------------------------------------------------------------

PROCESSED_PLAYER_DIR <- "data/processed/players"
PROCESSED_MODEL_DIR <- "data/processed/model_outputs"
PROCESSED_COMBINED_DIR <- "data/processed/combined"

dir.create(PROCESSED_COMBINED_DIR, recursive = TRUE, showWarnings = FALSE)


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

build_input_path <- function(base_dir, prefix, season, season_type) {
  data_year <- season_to_data_year(season)
  type_label <- season_type_file_label(season_type)
  
  file.path(
    base_dir,
    paste0(prefix, "_", data_year, "_", type_label, ".rds")
  )
}

build_combined_output_path <- function(prefix, season, season_type) {
  data_year <- season_to_data_year(season)
  type_label <- season_type_file_label(season_type)
  
  file.path(
    PROCESSED_COMBINED_DIR,
    paste0(prefix, "_", data_year, "_", type_label, ".rds")
  )
}

safe_divide <- function(num, denom) {
  dplyr::if_else(
    !is.na(denom) & denom > 0,
    num / denom,
    NA_real_
  )
}


# Builders ---------------------------------------------------------------------

build_combined_player_game <- function(player_game_summary, player_game_pq) {
  player_game_summary |>
    dplyr::left_join(
      player_game_pq |>
        dplyr::select(
          season,
          season_type,
          game_id,
          team_id,
          team_tricode,
          player_id,
          player_name,
          pq_events,
          possession_quality_value,
          avg_possession_quality,
          made_shots,
          missed_shots,
          assists,
          turnovers,
          offensive_rebounds,
          defensive_rebounds,
          unknown_rebounds,
          steals,
          blocks,
          offensive_fouls,
          made_free_throws,
          missed_free_throws
        ),
      by = c(
        "season",
        "season_type",
        "game_id",
        "team_id",
        "team_tricode",
        "player_id",
        "player_name"
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        c(
          pq_events,
          possession_quality_value,
          made_shots,
          missed_shots,
          assists,
          turnovers,
          offensive_rebounds,
          defensive_rebounds,
          unknown_rebounds,
          steals,
          blocks,
          offensive_fouls,
          made_free_throws,
          missed_free_throws
        ),
        ~ tidyr::replace_na(.x, 0)
      ),
      
      avg_possession_quality = safe_divide(
        .data$possession_quality_value,
        .data$pq_events
      ),
      
      aq_plus_pq_value = .data$shot_making_value + .data$possession_quality_value,
      
      possession_quality_per_fga = safe_divide(
        .data$possession_quality_value,
        .data$fga
      ),
      
      assists_per_turnover = dplyr::if_else(
        .data$turnovers > 0,
        .data$assists / .data$turnovers,
        NA_real_
      )
    ) |>
    dplyr::arrange(.data$game_date, .data$game_id, .data$team_tricode, .data$player_name)
}


build_combined_player_summary <- function(player_summary, player_pq_summary) {
  player_summary |>
    dplyr::left_join(
      player_pq_summary |>
        dplyr::select(
          season,
          season_type,
          player_id,
          player_name,
          team_id,
          team_tricode,
          pq_events,
          possession_quality_value,
          avg_possession_quality,
          pq_per_game,
          made_shots,
          missed_shots,
          assists,
          turnovers,
          offensive_rebounds,
          defensive_rebounds,
          unknown_rebounds,
          steals,
          blocks,
          offensive_fouls,
          made_free_throws,
          missed_free_throws,
          turnover_rate_pq_events,
          assist_to_turnover
        ),
      by = c(
        "season",
        "season_type",
        "player_id",
        "player_name",
        "team_id",
        "team_tricode"
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        c(
          pq_events,
          possession_quality_value,
          made_shots,
          missed_shots,
          assists,
          turnovers,
          offensive_rebounds,
          defensive_rebounds,
          unknown_rebounds,
          steals,
          blocks,
          offensive_fouls,
          made_free_throws,
          missed_free_throws
        ),
        ~ tidyr::replace_na(.x, 0)
      ),
      
      avg_possession_quality = safe_divide(
        .data$possession_quality_value,
        .data$pq_events
      ),
      
      pq_per_game = safe_divide(
        .data$possession_quality_value,
        .data$games
      ),
      
      possession_quality_per_fga = safe_divide(
        .data$possession_quality_value,
        .data$fga
      ),
      
      aq_plus_pq_value = .data$shot_making_value + .data$possession_quality_value,
      
      aq_plus_pq_per_game = safe_divide(
        .data$aq_plus_pq_value,
        .data$games
      ),
      
      assist_to_turnover = dplyr::if_else(
        .data$turnovers > 0,
        .data$assists / .data$turnovers,
        NA_real_
      ),
      
      turnover_rate_pq_events = safe_divide(
        .data$turnovers,
        .data$pq_events
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$aq_plus_pq_value))
}


# Main builder -----------------------------------------------------------------

build_combined_player_outputs <- function(
    season = "2024-25",
    season_type = "Regular Season",
    write_csv = TRUE,
    force_rebuild = TRUE
) {
  player_summary_path <- build_input_path(
    PROCESSED_PLAYER_DIR,
    "player_summary",
    season,
    season_type
  )
  
  player_game_summary_path <- build_input_path(
    PROCESSED_PLAYER_DIR,
    "player_game_summary",
    season,
    season_type
  )
  
  player_pq_summary_path <- build_input_path(
    PROCESSED_MODEL_DIR,
    "player_pq_summary",
    season,
    season_type
  )
  
  player_game_pq_path <- build_input_path(
    PROCESSED_MODEL_DIR,
    "player_game_pq",
    season,
    season_type
  )
  
  required_files <- c(
    player_summary_path,
    player_game_summary_path,
    player_pq_summary_path,
    player_game_pq_path
  )
  
  missing_files <- required_files[!file.exists(required_files)]
  
  if (length(missing_files) > 0) {
    stop(
      "Missing required input files:\n",
      paste(missing_files, collapse = "\n")
    )
  }
  
  combined_player_summary_out <- build_combined_output_path(
    "combined_player_summary",
    season,
    season_type
  )
  
  combined_player_game_out <- build_combined_output_path(
    "combined_player_game_summary",
    season,
    season_type
  )
  
  if (
    !force_rebuild &&
    file.exists(combined_player_summary_out) &&
    file.exists(combined_player_game_out)
  ) {
    message("Already built combined outputs for ", season, " / ", season_type)
    
    return(
      list(
        combined_player_summary = readr::read_rds(combined_player_summary_out),
        combined_player_game_summary = readr::read_rds(combined_player_game_out)
      )
    )
  }
  
  message("\nBuilding combined player outputs")
  message("- Season: ", season)
  message("- Season type: ", season_type)
  
  player_summary <- readr::read_rds(player_summary_path)
  player_game_summary <- readr::read_rds(player_game_summary_path)
  player_pq_summary <- readr::read_rds(player_pq_summary_path)
  player_game_pq <- readr::read_rds(player_game_pq_path)
  
  combined_player_summary <- build_combined_player_summary(
    player_summary = player_summary,
    player_pq_summary = player_pq_summary
  )
  
  combined_player_game_summary <- build_combined_player_game(
    player_game_summary = player_game_summary,
    player_game_pq = player_game_pq
  )
  
  qa_summary <- list(
    combined_player_summary = combined_player_summary |>
      dplyr::summarise(
        rows = dplyr::n(),
        players = dplyr::n_distinct(.data$player_id),
        total_fga = sum(.data$fga, na.rm = TRUE),
        total_shot_making_value = sum(.data$shot_making_value, na.rm = TRUE),
        total_pq_events = sum(.data$pq_events, na.rm = TRUE),
        total_possession_quality_value = sum(.data$possession_quality_value, na.rm = TRUE),
        avg_aq_score_weighted = sum(
          .data$avg_aq_score * .data$non_grenade_fga,
          na.rm = TRUE
        ) / sum(.data$non_grenade_fga, na.rm = TRUE),
        avg_pq_weighted = sum(
          .data$avg_possession_quality * .data$pq_events,
          na.rm = TRUE
        ) / sum(.data$pq_events, na.rm = TRUE)
      ),
    
    combined_player_game_summary = combined_player_game_summary |>
      dplyr::summarise(
        rows = dplyr::n(),
        games = dplyr::n_distinct(.data$game_id),
        players = dplyr::n_distinct(.data$player_id),
        total_fga = sum(.data$fga, na.rm = TRUE),
        total_pq_events = sum(.data$pq_events, na.rm = TRUE),
        total_shot_making_value = sum(.data$shot_making_value, na.rm = TRUE),
        total_possession_quality_value = sum(.data$possession_quality_value, na.rm = TRUE)
      )
  )
  
  message("\nCombined player summary QA:")
  print(qa_summary$combined_player_summary)
  
  message("\nCombined player-game summary QA:")
  print(qa_summary$combined_player_game_summary)
  
  message("\nTop combined players:")
  print(
    combined_player_summary |>
      dplyr::select(
        player_name,
        team_tricode,
        games,
        fga,
        avg_aq_score,
        shot_making_value,
        possession_quality_value,
        aq_plus_pq_value,
        aq_plus_pq_per_game,
        assists,
        turnovers,
        steals,
        blocks
      ) |>
      dplyr::slice_head(n = 20),
    n = 20
  )
  
  readr::write_rds(combined_player_summary, combined_player_summary_out)
  readr::write_rds(combined_player_game_summary, combined_player_game_out)
  
  if (write_csv) {
    readr::write_csv(
      combined_player_summary,
      stringr::str_replace(combined_player_summary_out, "\\.rds$", ".csv")
    )
    
    readr::write_csv(
      combined_player_game_summary,
      stringr::str_replace(combined_player_game_out, "\\.rds$", ".csv")
    )
  }
  
  message("\nSaved combined outputs:")
  message("- ", combined_player_summary_out)
  message("- ", combined_player_game_out)
  
  invisible(
    list(
      combined_player_summary = combined_player_summary,
      combined_player_game_summary = combined_player_game_summary,
      qa_summary = qa_summary
    )
  )
}