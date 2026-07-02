# R/05_build_matchups.R
# Builds clean matchup-context datasets from local nba_data matchups files.
#
# Inputs:
# - data/raw_local/nba_data/compressed/matchups_<year>.tar.xz
# - data/raw_local/nba_data/compressed/matchups_po_<year>.tar.xz
#
# Outputs:
# - data/processed/matchups/clean_matchups_<year>_regular.rds
# - data/processed/matchups/clean_matchups_<year>_regular.csv
# - data/processed/matchups/clean_matchups_<year>_playoffs.rds
# - data/processed/matchups/clean_matchups_<year>_playoffs.csv

required_pkgs <- c("tidyverse", "janitor", "stringr", "readr", "lubridate", "glue")
invisible(lapply(required_pkgs, require, character.only = TRUE))


# Paths ------------------------------------------------------------------------

RAW_COMPRESSED_DIR <- "data/raw_local/nba_data/compressed"
RAW_EXTRACTED_DIR <- "data/raw_local/nba_data/extracted"
PROCESSED_MATCHUPS_DIR <- "data/processed/matchups"


# Helpers ----------------------------------------------------------------------

normalize_game_id <- function(game_id) {
  stringr::str_pad(as.character(game_id), width = 10, side = "left", pad = "0")
}

season_to_data_year <- function(season) {
  as.integer(stringr::str_sub(season, 1, 4))
}

season_type_suffix <- function(season_type) {
  if (season_type == "Regular Season") {
    ""
  } else if (season_type == "Playoffs") {
    "_po"
  } else {
    stop("Unsupported season_type: ", season_type)
  }
}

build_source_file_name <- function(data_type, season, season_type) {
  data_year <- season_to_data_year(season)
  suffix <- season_type_suffix(season_type)
  
  if (suffix == "") {
    paste0(data_type, "_", data_year, ".tar.xz")
  } else {
    paste0(data_type, suffix, "_", data_year, ".tar.xz")
  }
}

build_csv_file_name <- function(data_type, season, season_type) {
  data_year <- season_to_data_year(season)
  suffix <- season_type_suffix(season_type)
  
  if (suffix == "") {
    paste0(data_type, "_", data_year, ".csv")
  } else {
    paste0(data_type, suffix, "_", data_year, ".csv")
  }
}

build_output_file_stub <- function(season, season_type) {
  data_year <- season_to_data_year(season)
  type_label <- if (season_type == "Regular Season") "regular" else "playoffs"
  
  paste0("clean_matchups_", data_year, "_", type_label)
}

extract_if_needed <- function(tar_file, csv_file, exdir) {
  if (file.exists(csv_file)) {
    message("Already extracted: ", csv_file)
    return(invisible(csv_file))
  }
  
  if (!file.exists(tar_file)) {
    stop("Missing source file: ", tar_file)
  }
  
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  
  message("Extracting: ", basename(tar_file))
  utils::untar(tarfile = tar_file, exdir = exdir)
  
  if (!file.exists(csv_file)) {
    stop("Expected CSV not found after extraction: ", csv_file)
  }
  
  invisible(csv_file)
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}


# Main builder -----------------------------------------------------------------

build_clean_matchups <- function(
    season = "2024-25",
    season_type = "Regular Season",
    write_csv = TRUE,
    force_rebuild = TRUE
) {
  dir.create(RAW_EXTRACTED_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(PROCESSED_MATCHUPS_DIR, recursive = TRUE, showWarnings = FALSE)
  
  matchup_tar <- file.path(
    RAW_COMPRESSED_DIR,
    build_source_file_name("matchups", season, season_type)
  )
  
  matchup_csv <- file.path(
    RAW_EXTRACTED_DIR,
    build_csv_file_name("matchups", season, season_type)
  )
  
  output_stub <- build_output_file_stub(season, season_type)
  
  out_rds <- file.path(PROCESSED_MATCHUPS_DIR, paste0(output_stub, ".rds"))
  out_csv <- file.path(PROCESSED_MATCHUPS_DIR, paste0(output_stub, ".csv"))
  
  if (!force_rebuild && file.exists(out_rds)) {
    message("Already built: ", out_rds)
    return(readr::read_rds(out_rds))
  }
  
  message("\nBuilding clean matchups")
  message("- Season: ", season)
  message("- Season type: ", season_type)
  
  extract_if_needed(matchup_tar, matchup_csv, RAW_EXTRACTED_DIR)
  
  matchups_raw <- readr::read_csv(matchup_csv, show_col_types = FALSE) |>
    janitor::clean_names()
  
  clean_matchups <- matchups_raw |>
    dplyr::mutate(
      season = season,
      season_type = season_type,
      game_id = normalize_game_id(.data$game_id),
      
      away_team_id = as.character(.data$away_team_id),
      home_team_id = as.character(.data$home_team_id),
      team_id = as.character(.data$team_id),
      
      player_id = as.character(.data$person_id),
      matchup_player_id = as.character(.data$matchups_person_id),
      
      player_name = stringr::str_squish(paste(.data$first_name, .data$family_name)),
      matchup_player_name = stringr::str_squish(
        paste(.data$matchups_first_name, .data$matchups_family_name)
      ),
      
      matchup_seconds = safe_numeric(.data$matchup_minutes_sort),
      matchup_minutes_decimal = .data$matchup_seconds / 60,
      partial_possessions = safe_numeric(.data$partial_possessions),
      
      percentage_defender_total_time = safe_numeric(.data$percentage_defender_total_time),
      percentage_offensive_total_time = safe_numeric(.data$percentage_offensive_total_time),
      percentage_total_time_both_on = safe_numeric(.data$percentage_total_time_both_on),
      
      switches_on = safe_numeric(.data$switches_on),
      
      player_points = safe_numeric(.data$player_points),
      team_points = safe_numeric(.data$team_points),
      
      matchup_assists = safe_numeric(.data$matchup_assists),
      matchup_potential_assists = safe_numeric(.data$matchup_potential_assists),
      matchup_turnovers = safe_numeric(.data$matchup_turnovers),
      matchup_blocks = safe_numeric(.data$matchup_blocks),
      
      matchup_field_goals_made = safe_numeric(.data$matchup_field_goals_made),
      matchup_field_goals_attempted = safe_numeric(.data$matchup_field_goals_attempted),
      matchup_field_goals_percentage = safe_numeric(.data$matchup_field_goals_percentage),
      
      matchup_three_pointers_made = safe_numeric(.data$matchup_three_pointers_made),
      matchup_three_pointers_attempted = safe_numeric(.data$matchup_three_pointers_attempted),
      matchup_three_pointers_percentage = safe_numeric(.data$matchup_three_pointers_percentage),
      
      help_blocks = safe_numeric(.data$help_blocks),
      help_field_goals_made = safe_numeric(.data$help_field_goals_made),
      help_field_goals_attempted = safe_numeric(.data$help_field_goals_attempted),
      help_field_goals_percentage = safe_numeric(.data$help_field_goals_percentage),
      
      matchup_free_throws_made = safe_numeric(.data$matchup_free_throws_made),
      matchup_free_throws_attempted = safe_numeric(.data$matchup_free_throws_attempted),
      shooting_fouls = safe_numeric(.data$shooting_fouls),
      
      matchup_points_per_fga = dplyr::if_else(
        .data$matchup_field_goals_attempted > 0,
        .data$player_points / .data$matchup_field_goals_attempted,
        NA_real_
      ),
      
      matchup_three_attempt_rate = dplyr::if_else(
        .data$matchup_field_goals_attempted > 0,
        .data$matchup_three_pointers_attempted / .data$matchup_field_goals_attempted,
        NA_real_
      ),
      
      matchup_free_throw_rate = dplyr::if_else(
        .data$matchup_field_goals_attempted > 0,
        .data$matchup_free_throws_attempted / .data$matchup_field_goals_attempted,
        NA_real_
      )
    ) |>
    dplyr::transmute(
      season,
      season_type,
      game_id,
      
      away_team_id,
      home_team_id,
      
      team_id,
      team_name,
      team_city,
      team_tricode,
      team_slug,
      
      player_id,
      player_name,
      name_i,
      player_slug,
      position,
      jersey_num,
      
      matchup_player_id,
      matchup_player_name,
      matchups_name_i,
      matchups_player_slug,
      matchups_jersey_num,
      
      matchup_minutes,
      matchup_seconds,
      matchup_minutes_decimal,
      partial_possessions,
      
      percentage_defender_total_time,
      percentage_offensive_total_time,
      percentage_total_time_both_on,
      
      switches_on,
      
      player_points,
      team_points,
      
      matchup_assists,
      matchup_potential_assists,
      matchup_turnovers,
      matchup_blocks,
      
      matchup_field_goals_made,
      matchup_field_goals_attempted,
      matchup_field_goals_percentage,
      
      matchup_three_pointers_made,
      matchup_three_pointers_attempted,
      matchup_three_pointers_percentage,
      
      matchup_free_throws_made,
      matchup_free_throws_attempted,
      
      shooting_fouls,
      
      help_blocks,
      help_field_goals_made,
      help_field_goals_attempted,
      help_field_goals_percentage,
      
      matchup_points_per_fga,
      matchup_three_attempt_rate,
      matchup_free_throw_rate
    ) |>
    dplyr::arrange(.data$game_id, .data$team_tricode, .data$player_name, dplyr::desc(.data$matchup_minutes_decimal))
  
  qa_summary <- clean_matchups |>
    dplyr::summarise(
      rows = dplyr::n(),
      games = dplyr::n_distinct(.data$game_id),
      offensive_players = dplyr::n_distinct(.data$player_id),
      matchup_players = dplyr::n_distinct(.data$matchup_player_id),
      total_matchup_minutes = sum(.data$matchup_minutes_decimal, na.rm = TRUE),
      total_partial_possessions = sum(.data$partial_possessions, na.rm = TRUE),
      rows_with_fga = sum(.data$matchup_field_goals_attempted > 0, na.rm = TRUE),
      rows_with_possessions = sum(.data$partial_possessions > 0, na.rm = TRUE)
    )
  
  player_matchup_summary <- clean_matchups |>
    dplyr::group_by(.data$player_id, .data$player_name, .data$team_tricode) |>
    dplyr::summarise(
      games = dplyr::n_distinct(.data$game_id),
      matchup_rows = dplyr::n(),
      matchup_minutes = sum(.data$matchup_minutes_decimal, na.rm = TRUE),
      partial_possessions = sum(.data$partial_possessions, na.rm = TRUE),
      player_points = sum(.data$player_points, na.rm = TRUE),
      fga = sum(.data$matchup_field_goals_attempted, na.rm = TRUE),
      fgm = sum(.data$matchup_field_goals_made, na.rm = TRUE),
      three_pa = sum(.data$matchup_three_pointers_attempted, na.rm = TRUE),
      shooting_fouls = sum(.data$shooting_fouls, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      fg_pct = dplyr::if_else(.data$fga > 0, .data$fgm / .data$fga, NA_real_),
      three_rate = dplyr::if_else(.data$fga > 0, .data$three_pa / .data$fga, NA_real_),
      points_per_fga = dplyr::if_else(.data$fga > 0, .data$player_points / .data$fga, NA_real_)
    ) |>
    dplyr::arrange(dplyr::desc(.data$matchup_minutes))
  
  duplicate_keys <- clean_matchups |>
    dplyr::count(.data$game_id, .data$player_id, .data$matchup_player_id) |>
    dplyr::filter(.data$n > 1)
  
  message("\nMatchup QA summary:")
  print(qa_summary)
  
  message("\nTop player matchup-minute summary:")
  print(player_matchup_summary |> dplyr::slice_head(n = 20), n = 20)
  
  message("\nDuplicate matchup keys:")
  print(duplicate_keys, n = 25)
  
  if (nrow(duplicate_keys) > 0) {
    warning("Duplicate matchup keys found. This may be normal if rows split by stint, but inspect before using as unique keys.")
  }
  
  readr::write_rds(clean_matchups, out_rds)
  
  if (write_csv) {
    readr::write_csv(clean_matchups, out_csv)
  }
  
  message("\nSaved clean matchups:")
  message("- ", out_rds)
  
  if (write_csv) {
    message("- ", out_csv)
  }
  
  invisible(clean_matchups)
}