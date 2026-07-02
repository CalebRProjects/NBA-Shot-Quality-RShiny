# R/04_build_enriched_shots.R
# Builds enriched shot-level datasets from local nba_data files.
#
# Primary inputs:
# - data/raw_local/nba_data/compressed/nbastatsv3_<year>.tar.xz
# - data/raw_local/nba_data/compressed/shotdetail_<year>.tar.xz
#
# Playoff inputs:
# - data/raw_local/nba_data/compressed/nbastatsv3_po_<year>.tar.xz
# - data/raw_local/nba_data/compressed/shotdetail_po_<year>.tar.xz
#
# Output:
# - data/processed/shots/enriched_shots_<year>_<season_type>.rds
# - data/processed/shots/enriched_shots_<year>_<season_type>.csv

required_pkgs <- c("tidyverse", "janitor", "stringr", "readr", "lubridate", "glue")
invisible(lapply(required_pkgs, require, character.only = TRUE))


# Paths ------------------------------------------------------------------------

RAW_COMPRESSED_DIR <- "data/raw_local/nba_data/compressed"
RAW_EXTRACTED_DIR <- "data/raw_local/nba_data/extracted"
PROCESSED_SHOTS_DIR <- "data/processed/shots"


# Helpers ----------------------------------------------------------------------

normalize_game_id <- function(game_id) {
  stringr::str_pad(as.character(game_id), width = 10, side = "left", pad = "0")
}

parse_iso_clock_seconds <- function(clock_chr) {
  mins <- stringr::str_match(clock_chr, "PT([0-9]+)M")[, 2]
  secs <- stringr::str_match(clock_chr, "M([0-9.]+)S")[, 2]
  
  mins <- suppressWarnings(as.numeric(mins))
  secs <- suppressWarnings(as.numeric(secs))
  
  dplyr::coalesce(mins, 0) * 60 + dplyr::coalesce(secs, 0)
}

season_to_data_year <- function(season) {
  as.integer(stringr::str_sub(season, 1, 4))
}

clean_season_type_label <- function(season_type) {
  season_type |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")
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
  
  paste0("enriched_shots_", data_year, "_", type_label)
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

add_pbp_shot_value <- function(data) {
  if ("shot_value" %in% names(data)) {
    data |>
      dplyr::mutate(
        pbp_shot_value = suppressWarnings(as.numeric(.data$shot_value))
      )
  } else {
    data |>
      dplyr::mutate(
        pbp_shot_value = dplyr::case_when(
          stringr::str_detect(
            stringr::str_to_lower(dplyr::coalesce(.data$description, "")),
            "3pt"
          ) ~ 3,
          TRUE ~ 2
        )
      )
  }
}

add_pbp_is_field_goal <- function(data) {
  if ("is_field_goal" %in% names(data)) {
    data |>
      dplyr::mutate(
        pbp_is_field_goal = suppressWarnings(as.numeric(.data$is_field_goal))
      )
  } else {
    data |>
      dplyr::mutate(
        pbp_is_field_goal = dplyr::if_else(
          stringr::str_to_lower(dplyr::coalesce(.data$action_type, "")) %in%
            c("made shot", "missed shot"),
          1,
          0
        )
      )
  }
}

# Main builder -----------------------------------------------------------------

build_enriched_shots <- function(
    season = "2024-25",
    season_type = "Regular Season",
    write_csv = TRUE,
    force_rebuild = TRUE
) {
  dir.create(RAW_EXTRACTED_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(PROCESSED_SHOTS_DIR, recursive = TRUE, showWarnings = FALSE)
  
  pbp_tar <- file.path(
    RAW_COMPRESSED_DIR,
    build_source_file_name("nbastatsv3", season, season_type)
  )
  
  shotdetail_tar <- file.path(
    RAW_COMPRESSED_DIR,
    build_source_file_name("shotdetail", season, season_type)
  )
  
  pbp_csv <- file.path(
    RAW_EXTRACTED_DIR,
    build_csv_file_name("nbastatsv3", season, season_type)
  )
  
  shotdetail_csv <- file.path(
    RAW_EXTRACTED_DIR,
    build_csv_file_name("shotdetail", season, season_type)
  )
  
  output_stub <- build_output_file_stub(season, season_type)
  
  out_rds <- file.path(PROCESSED_SHOTS_DIR, paste0(output_stub, ".rds"))
  out_csv <- file.path(PROCESSED_SHOTS_DIR, paste0(output_stub, ".csv"))
  
  if (!force_rebuild && file.exists(out_rds)) {
    message("Already built: ", out_rds)
    return(readr::read_rds(out_rds))
  }
  
  message("\nBuilding enriched shots")
  message("- Season: ", season)
  message("- Season type: ", season_type)
  
  extract_if_needed(pbp_tar, pbp_csv, RAW_EXTRACTED_DIR)
  extract_if_needed(shotdetail_tar, shotdetail_csv, RAW_EXTRACTED_DIR)
  
  pbp_raw <- readr::read_csv(pbp_csv, show_col_types = FALSE) |>
    janitor::clean_names()
  
  shotdetail_raw <- readr::read_csv(shotdetail_csv, show_col_types = FALSE) |>
    janitor::clean_names()
  
  pbp_shots <- pbp_raw |>
    dplyr::mutate(
      season = season,
      season_type = season_type,
      game_id = normalize_game_id(.data$game_id),
      game_event_id = as.integer(.data$action_number),
      player_id = as.character(.data$person_id),
      team_id = as.character(.data$team_id),
      period = as.integer(.data$period),
      seconds_remaining = parse_iso_clock_seconds(.data$clock),
      pbp_shot_distance = suppressWarnings(as.numeric(.data$shot_distance))
    ) |>
    add_pbp_shot_value() |>
    add_pbp_is_field_goal() |>
    dplyr::filter(
      .data$pbp_is_field_goal == 1,
      !is.na(.data$player_id),
      .data$player_id != "0",
      !stringr::str_detect(.data$player_id, "^161061")
    ) |>
    dplyr::transmute(
      season,
      season_type,
      game_id,
      game_event_id,
      action_number,
      action_id,
      period,
      clock,
      seconds_remaining,
      
      team_id,
      team_tricode,
      
      player_id,
      player_name,
      player_name_i,
      
      x_legacy = suppressWarnings(as.numeric(.data$x_legacy)),
      y_legacy = suppressWarnings(as.numeric(.data$y_legacy)),
      
      pbp_shot_distance,
      pbp_shot_value,
      shot_result,
      
      score_home = suppressWarnings(as.numeric(.data$score_home)),
      score_away = suppressWarnings(as.numeric(.data$score_away)),
      points_total = suppressWarnings(as.numeric(.data$points_total)),
      
      location,
      description,
      pbp_action_type = action_type,
      pbp_sub_type = sub_type,
      video_available
    )
  
  shotdetail_clean <- shotdetail_raw |>
    dplyr::mutate(
      game_id = normalize_game_id(.data$game_id),
      game_event_id = as.integer(.data$game_event_id),
      player_id = as.character(.data$player_id),
      team_id = as.character(.data$team_id),
      period = as.integer(.data$period),
      shotdetail_total_seconds_remaining = .data$minutes_remaining * 60 + .data$seconds_remaining,
      shot_distance = suppressWarnings(as.numeric(.data$shot_distance)),
      shot_attempted_flag = suppressWarnings(as.numeric(.data$shot_attempted_flag)),
      shot_made_flag = suppressWarnings(as.numeric(.data$shot_made_flag)),
      game_date = as.Date(as.character(.data$game_date), format = "%Y%m%d")
    ) |>
    dplyr::transmute(
      game_id,
      game_event_id,
      player_id,
      
      shotdetail_player_name = player_name,
      shotdetail_team_id = team_id,
      team_name,
      
      shotdetail_period = period,
      minutes_remaining,
      shotdetail_seconds_remaining = seconds_remaining,
      shotdetail_total_seconds_remaining,
      
      event_type,
      shotdetail_action_type = action_type,
      shot_type,
      shot_zone_basic,
      shot_zone_area,
      shot_zone_range,
      
      shot_distance,
      loc_x,
      loc_y,
      
      shot_attempted_flag,
      shot_made_flag,
      game_date,
      htm,
      vtm
    )
  
  enriched_shots <- pbp_shots |>
    dplyr::left_join(
      shotdetail_clean,
      by = c("game_id", "game_event_id", "player_id")
    ) |>
    dplyr::mutate(
      shot_value = dplyr::case_when(
        stringr::str_detect(.data$shot_type, "3PT") ~ 3,
        stringr::str_detect(.data$description, "3PT") ~ 3,
        .data$pbp_shot_value %in% c(2, 3) ~ .data$pbp_shot_value,
        TRUE ~ 2
      ),
      
      shot_result = dplyr::case_when(
        .data$shot_made_flag == 1 ~ "Made",
        .data$shot_made_flag == 0 ~ "Missed",
        .data$shot_result %in% c("Made", "Missed") ~ .data$shot_result,
        TRUE ~ .data$shot_result
      ),
      
      actual_points = dplyr::if_else(
        .data$shot_result == "Made",
        as.numeric(.data$shot_value),
        0
      ),
      
      final_shot_distance = dplyr::coalesce(.data$shot_distance, .data$pbp_shot_distance),
      
      is_three = .data$shot_value == 3,
      is_two = .data$shot_value == 2,
      
      is_restricted_area = .data$shot_zone_basic == "Restricted Area",
      is_paint_non_ra = .data$shot_zone_basic == "In The Paint (Non-RA)",
      is_midrange = .data$shot_zone_basic == "Mid-Range",
      
      is_corner_three = .data$shot_zone_basic %in% c("Left Corner 3", "Right Corner 3"),
      is_above_break_three = .data$shot_zone_basic == "Above the Break 3",
      
      is_backcourt = .data$shot_zone_basic == "Backcourt",
      
      is_late_clock = .data$seconds_remaining <= 4,
      is_heave_distance = .data$final_shot_distance >= 35,
      is_grenade = .data$seconds_remaining <= 2 & .data$final_shot_distance >= 35,
      
      shot_zone_group = dplyr::case_when(
        .data$is_restricted_area ~ "Restricted Area",
        .data$is_paint_non_ra ~ "Paint Non-RA",
        .data$is_midrange ~ "Mid-Range",
        .data$is_corner_three ~ "Corner 3",
        .data$is_above_break_three ~ "Above Break 3",
        .data$is_backcourt ~ "Backcourt",
        
        is.na(.data$shot_zone_basic) & .data$shot_value == 3 ~ "Unknown 3",
        is.na(.data$shot_zone_basic) & .data$final_shot_distance <= 5 ~ "Unknown Rim",
        is.na(.data$shot_zone_basic) & .data$final_shot_distance <= 10 ~ "Unknown Paint",
        is.na(.data$shot_zone_basic) & .data$shot_value == 2 ~ "Unknown 2",
        
        TRUE ~ "Other"
      )
    ) |>
    dplyr::select(
      season,
      season_type,
      game_id,
      game_date,
      htm,
      vtm,
      
      game_event_id,
      action_number,
      action_id,
      period,
      clock,
      seconds_remaining,
      
      team_id,
      team_tricode,
      team_name,
      
      player_id,
      player_name,
      player_name_i,
      
      shot_result,
      shot_value,
      actual_points,
      final_shot_distance,
      
      shot_type,
      shot_zone_basic,
      shot_zone_area,
      shot_zone_range,
      shot_zone_group,
      
      loc_x,
      loc_y,
      x_legacy,
      y_legacy,
      
      event_type,
      description,
      pbp_action_type,
      pbp_sub_type,
      shotdetail_action_type,
      
      is_three,
      is_two,
      is_restricted_area,
      is_paint_non_ra,
      is_midrange,
      is_corner_three,
      is_above_break_three,
      is_backcourt,
      is_late_clock,
      is_heave_distance,
      is_grenade,
      
      shot_attempted_flag,
      shot_made_flag,
      video_available,
      
      score_home,
      score_away,
      points_total
    ) |>
    dplyr::arrange(.data$game_id, .data$period, dplyr::desc(.data$seconds_remaining), .data$game_event_id)
  
  qa_summary <- enriched_shots |>
    dplyr::summarise(
      rows = dplyr::n(),
      games = dplyr::n_distinct(.data$game_id),
      players = dplyr::n_distinct(.data$player_id),
      matched_shotdetail = sum(!is.na(.data$shot_zone_basic)),
      unmatched_shotdetail = sum(is.na(.data$shot_zone_basic)),
      match_rate = .data$matched_shotdetail / .data$rows,
      made_shots = sum(.data$shot_result == "Made", na.rm = TRUE),
      three_attempts = sum(.data$is_three, na.rm = TRUE),
      restricted_area_attempts = sum(.data$is_restricted_area, na.rm = TRUE),
      paint_non_ra_attempts = sum(.data$is_paint_non_ra, na.rm = TRUE),
      midrange_attempts = sum(.data$is_midrange, na.rm = TRUE),
      corner_three_attempts = sum(.data$is_corner_three, na.rm = TRUE),
      above_break_three_attempts = sum(.data$is_above_break_three, na.rm = TRUE),
      backcourt_attempts = sum(.data$is_backcourt, na.rm = TRUE),
      grenade_attempts = sum(.data$is_grenade, na.rm = TRUE)
    )
  
  zone_summary <- enriched_shots |>
    dplyr::count(.data$shot_zone_group, sort = TRUE)
  
  duplicate_keys <- enriched_shots |>
    dplyr::count(.data$game_id, .data$game_event_id, .data$player_id) |>
    dplyr::filter(.data$n > 1)
  
  message("\nQA summary:")
  print(qa_summary)
  
  message("\nShot zone summary:")
  print(zone_summary)
  
  message("\nDuplicate key rows:")
  print(duplicate_keys, n = 25)
  
  if (qa_summary$match_rate < 0.99) {
    warning("Shotdetail match rate below 99%: ", round(qa_summary$match_rate, 4))
  }
  
  if (nrow(duplicate_keys) > 0) {
    warning("Duplicate shot keys found. Inspect duplicate_keys before modeling.")
  }
  
  readr::write_rds(enriched_shots, out_rds)
  
  if (write_csv) {
    readr::write_csv(enriched_shots, out_csv)
  }
  
  message("\nSaved enriched shots:")
  message("- ", out_rds)
  
  if (write_csv) {
    message("- ", out_csv)
  }
  
  invisible(enriched_shots)
}