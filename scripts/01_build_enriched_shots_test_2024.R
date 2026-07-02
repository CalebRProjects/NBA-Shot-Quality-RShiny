# scripts/01_build_enriched_shots_test_2024.R
# Proof-of-concept build for 2024-25 Regular Season enriched shots.
#
# Input:
# data/raw_local/nba_data/compressed/nbastatsv3_2024.tar.xz
# data/raw_local/nba_data/compressed/shotdetail_2024.tar.xz
#
# Output:
# data/processed/shots/enriched_shots_2024_regular.rds
# data/processed/shots/enriched_shots_2024_regular.csv

library(tidyverse)
library(janitor)
library(stringr)
library(readr)
library(lubridate)

# Paths ------------------------------------------------------------------------

RAW_COMPRESSED_DIR <- "data/raw_local/nba_data/compressed"
RAW_EXTRACTED_DIR <- "data/raw_local/nba_data/extracted"
PROCESSED_SHOTS_DIR <- "data/processed/shots"

dir.create(RAW_EXTRACTED_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PROCESSED_SHOTS_DIR, recursive = TRUE, showWarnings = FALSE)

SEASON <- "2024-25"
SEASON_TYPE <- "Regular Season"

PBP_TAR <- file.path(RAW_COMPRESSED_DIR, "nbastatsv3_2024.tar.xz")
SHOTDETAIL_TAR <- file.path(RAW_COMPRESSED_DIR, "shotdetail_2024.tar.xz")

PBP_CSV <- file.path(RAW_EXTRACTED_DIR, "nbastatsv3_2024.csv")
SHOTDETAIL_CSV <- file.path(RAW_EXTRACTED_DIR, "shotdetail_2024.csv")

OUT_RDS <- file.path(PROCESSED_SHOTS_DIR, "enriched_shots_2024_regular.rds")
OUT_CSV <- file.path(PROCESSED_SHOTS_DIR, "enriched_shots_2024_regular.csv")


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

extract_if_needed <- function(tar_file, csv_file, exdir) {
  if (file.exists(csv_file)) {
    message("Already extracted: ", csv_file)
    return(invisible(csv_file))
  }
  
  if (!file.exists(tar_file)) {
    stop("Missing source file: ", tar_file)
  }
  
  message("Extracting: ", basename(tar_file))
  utils::untar(tarfile = tar_file, exdir = exdir)
  
  if (!file.exists(csv_file)) {
    stop("Expected CSV not found after extraction: ", csv_file)
  }
  
  invisible(csv_file)
}


# Extract/load -----------------------------------------------------------------

extract_if_needed(PBP_TAR, PBP_CSV, RAW_EXTRACTED_DIR)
extract_if_needed(SHOTDETAIL_TAR, SHOTDETAIL_CSV, RAW_EXTRACTED_DIR)

pbp_raw <- readr::read_csv(PBP_CSV, show_col_types = FALSE) |>
  janitor::clean_names()

shotdetail_raw <- readr::read_csv(SHOTDETAIL_CSV, show_col_types = FALSE) |>
  janitor::clean_names()


# Build clean PBP shot layer ---------------------------------------------------

pbp_shots <- pbp_raw |>
  mutate(
    season = SEASON,
    season_type = SEASON_TYPE,
    game_id = normalize_game_id(game_id),
    game_event_id = as.integer(action_number),
    player_id = as.character(person_id),
    team_id = as.character(team_id),
    period = as.integer(period),
    seconds_remaining = parse_iso_clock_seconds(clock),
    pbp_shot_distance = suppressWarnings(as.numeric(shot_distance)),
    pbp_shot_value = suppressWarnings(as.numeric(shot_value)),
    is_field_goal = suppressWarnings(as.numeric(is_field_goal))
  ) |>
  filter(
    is_field_goal == 1,
    !is.na(player_id),
    player_id != "0"
  ) |>
  transmute(
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
    
    x_legacy = suppressWarnings(as.numeric(x_legacy)),
    y_legacy = suppressWarnings(as.numeric(y_legacy)),
    
    pbp_shot_distance,
    pbp_shot_value,
    shot_result,
    
    score_home = suppressWarnings(as.numeric(score_home)),
    score_away = suppressWarnings(as.numeric(score_away)),
    points_total = suppressWarnings(as.numeric(points_total)),
    
    location,
    description,
    pbp_action_type = action_type,
    pbp_sub_type = sub_type,
    video_available
  )


# Build clean shotdetail layer -------------------------------------------------

shotdetail_clean <- shotdetail_raw |>
  mutate(
    game_id = normalize_game_id(game_id),
    game_event_id = as.integer(game_event_id),
    player_id = as.character(player_id),
    team_id = as.character(team_id),
    period = as.integer(period),
    shotdetail_total_seconds_remaining = minutes_remaining * 60 + seconds_remaining,
    shot_distance = suppressWarnings(as.numeric(shot_distance)),
    shot_attempted_flag = suppressWarnings(as.numeric(shot_attempted_flag)),
    shot_made_flag = suppressWarnings(as.numeric(shot_made_flag)),
    game_date = as.Date(as.character(game_date), format = "%Y%m%d")
  ) |>
  transmute(
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


# Join and enrich --------------------------------------------------------------

enriched_shots <- pbp_shots |>
  left_join(
    shotdetail_clean,
    by = c("game_id", "game_event_id", "player_id")
  ) |>
  mutate(
    shot_value = case_when(
      str_detect(shot_type, "3PT") ~ 3,
      str_detect(description, "3PT") ~ 3,
      pbp_shot_value %in% c(2, 3) ~ pbp_shot_value,
      TRUE ~ 2
    ),
    
    shot_result = case_when(
      shot_made_flag == 1 ~ "Made",
      shot_made_flag == 0 ~ "Missed",
      shot_result %in% c("Made", "Missed") ~ shot_result,
      TRUE ~ shot_result
    ),
    
    actual_points = if_else(shot_result == "Made", as.numeric(shot_value), 0),
    
    final_shot_distance = coalesce(shot_distance, pbp_shot_distance),
    
    is_three = shot_value == 3,
    is_two = shot_value == 2,
    
    is_restricted_area = shot_zone_basic == "Restricted Area",
    is_paint_non_ra = shot_zone_basic == "In The Paint (Non-RA)",
    is_midrange = shot_zone_basic == "Mid-Range",
    
    is_corner_three = shot_zone_basic %in% c("Left Corner 3", "Right Corner 3"),
    is_above_break_three = shot_zone_basic == "Above the Break 3",
    
    is_backcourt = shot_zone_basic == "Backcourt",
    
    is_late_clock = seconds_remaining <= 4,
    is_heave_distance = final_shot_distance >= 35,
    is_grenade = seconds_remaining <= 2 & final_shot_distance >= 35,
    
    shot_zone_group = case_when(
      is_restricted_area ~ "Restricted Area",
      is_paint_non_ra ~ "Paint Non-RA",
      is_midrange ~ "Mid-Range",
      is_corner_three ~ "Corner 3",
      is_above_break_three ~ "Above Break 3",
      is_backcourt ~ "Backcourt",
      
      is.na(shot_zone_basic) & shot_value == 3 ~ "Unknown 3",
      is.na(shot_zone_basic) & final_shot_distance <= 5 ~ "Unknown Rim",
      is.na(shot_zone_basic) & final_shot_distance <= 10 ~ "Unknown Paint",
      is.na(shot_zone_basic) & shot_value == 2 ~ "Unknown 2",
      
      TRUE ~ "Other"
    )
  ) |>
  select(
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
  arrange(game_id, period, desc(seconds_remaining), game_event_id)


# QA ---------------------------------------------------------------------------

qa_summary <- enriched_shots |>
  summarise(
    rows = n(),
    games = n_distinct(game_id),
    players = n_distinct(player_id),
    matched_shotdetail = sum(!is.na(shot_zone_basic)),
    unmatched_shotdetail = sum(is.na(shot_zone_basic)),
    match_rate = matched_shotdetail / rows,
    made_shots = sum(shot_result == "Made", na.rm = TRUE),
    three_attempts = sum(is_three, na.rm = TRUE),
    restricted_area_attempts = sum(is_restricted_area, na.rm = TRUE),
    paint_non_ra_attempts = sum(is_paint_non_ra, na.rm = TRUE),
    midrange_attempts = sum(is_midrange, na.rm = TRUE),
    corner_three_attempts = sum(is_corner_three, na.rm = TRUE),
    above_break_three_attempts = sum(is_above_break_three, na.rm = TRUE),
    backcourt_attempts = sum(is_backcourt, na.rm = TRUE),
    grenade_attempts = sum(is_grenade, na.rm = TRUE)
  )

zone_summary <- enriched_shots |>
  count(shot_zone_group, sort = TRUE)

duplicate_keys <- enriched_shots |>
  count(game_id, game_event_id, player_id) |>
  filter(n > 1)

message("\nQA summary:")
print(qa_summary)

message("\nShot zone summary:")
print(zone_summary)

message("\nDuplicate key rows:")
print(duplicate_keys, n = 25)

if (qa_summary$games != 1230) {
  warning("Expected 1230 games for 2024-25 regular season. Found: ", qa_summary$games)
}

if (qa_summary$match_rate < 0.99) {
  warning("Shotdetail match rate below 99%: ", round(qa_summary$match_rate, 4))
}

if (nrow(duplicate_keys) > 0) {
  warning("Duplicate shot keys found. Inspect duplicate_keys before modeling.")
}


# Save -------------------------------------------------------------------------

readr::write_rds(enriched_shots, OUT_RDS)
readr::write_csv(enriched_shots, OUT_CSV)

message("\nSaved enriched shots:")
message("- ", OUT_RDS)
message("- ", OUT_CSV)