# R/06_attempt_quality_rules.R
# Scores enriched shot-level datasets with Attempt Quality buckets,
# expected points, and shot-making value.

required_pkgs <- c("tidyverse", "stringr", "readr", "glue")
invisible(lapply(required_pkgs, require, character.only = TRUE))


# Paths ------------------------------------------------------------------------

PROCESSED_SHOTS_DIR <- "data/processed/shots"
PROCESSED_MODEL_DIR <- "data/processed/model_outputs"

dir.create(PROCESSED_MODEL_DIR, recursive = TRUE, showWarnings = FALSE)


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

build_enriched_shots_path <- function(season, season_type) {
  data_year <- season_to_data_year(season)
  type_label <- season_type_file_label(season_type)
  
  file.path(
    PROCESSED_SHOTS_DIR,
    paste0("enriched_shots_", data_year, "_", type_label, ".rds")
  )
}

build_model_output_path <- function(prefix, season, season_type) {
  data_year <- season_to_data_year(season)
  type_label <- season_type_file_label(season_type)
  
  file.path(
    PROCESSED_MODEL_DIR,
    paste0(prefix, "_", data_year, "_", type_label, ".rds")
  )
}


# Attempt Quality rules --------------------------------------------------------

assign_attempt_quality_bucket <- function(shots) {
  shots |>
    dplyr::mutate(
      action_text = stringr::str_to_lower(
        paste(
          .data$description,
          .data$pbp_action_type,
          .data$pbp_sub_type,
          .data$shotdetail_action_type,
          sep = " "
        )
      ),
      
      aq_bucket = dplyr::case_when(
        .data$is_grenade ~ "Grenade",
        
        # Best shots: rim pressure and clean finishing chances
        .data$is_restricted_area ~ "9",
        stringr::str_detect(.data$action_text, "dunk|layup|alley oop") &
          .data$final_shot_distance <= 5 ~ "9",
        
        # Strong shots: corner threes, high-value threes, cuts, putbacks
        .data$is_corner_three ~ "7",
        .data$is_above_break_three & .data$final_shot_distance <= 29 ~ "7",
        .data$is_paint_non_ra &
          stringr::str_detect(.data$action_text, "cut|putback|alley oop|dunk") ~ "7",
        
        # Acceptable shots: paint non-RA, short twos, floaters
        .data$is_paint_non_ra ~ "5",
        .data$shot_value == 2 & .data$final_shot_distance <= 10 ~ "5",
        
        # Lower-value shots: midrange and long twos
        .data$is_midrange ~ "3",
        .data$shot_value == 2 & .data$final_shot_distance <= 23 ~ "3",
        
        # Extreme deep threes that are not grenades
        .data$shot_value == 3 & .data$final_shot_distance >= 35 ~ "1",
        
        # Deep threes that are not grenades
        .data$shot_value == 3 & .data$final_shot_distance >= 30 ~ "3",
        
        # Unknown fallback
        .data$shot_zone_group == "Unknown Rim" ~ "9",
        .data$shot_zone_group == "Unknown Paint" ~ "5",
        .data$shot_zone_group == "Unknown 3" ~ "3",
        .data$shot_zone_group == "Unknown 2" ~ "3",
        
        TRUE ~ "1"
      ),
      
      # Late-clock non-grenade shots get one bucket bump because the process burden
      # is different from an early-clock self-selected tough attempt.
      aq_bucket = dplyr::case_when(
        .data$is_late_clock & .data$aq_bucket == "1" ~ "3",
        .data$is_late_clock & .data$aq_bucket == "3" ~ "5",
        TRUE ~ .data$aq_bucket
      ),
      
      aq_bucket = factor(
        .data$aq_bucket,
        levels = c("9", "7", "5", "3", "1", "Grenade")
      )
    ) |>
    dplyr::select(-"action_text")
}

bucket_to_attempt_quality_score <- function(bucket) {
  dplyr::case_when(
    as.character(bucket) == "9" ~ 9,
    as.character(bucket) == "7" ~ 7,
    as.character(bucket) == "5" ~ 5,
    as.character(bucket) == "3" ~ 3,
    as.character(bucket) == "1" ~ 1,
    as.character(bucket) == "Grenade" ~ NA_real_,
    TRUE ~ NA_real_
  )
}

fallback_expected_points_by_bucket <- function() {
  tibble::tibble(
    aq_bucket = c("9", "7", "5", "3", "1", "Grenade"),
    fallback_expected_points = c(1.35, 1.15, 1.00, 0.80, 0.55, 0.20)
  )
}

build_expected_points_by_bucket <- function(scored_shots, min_attempts = 500) {
  bucket_levels <- c("9", "7", "5", "3", "1", "Grenade")
  
  scored_shots |>
    dplyr::mutate(
      aq_bucket = as.character(.data$aq_bucket),
      actual_points = suppressWarnings(as.numeric(.data$actual_points))
    ) |>
    dplyr::filter(.data$aq_bucket %in% bucket_levels) |>
    dplyr::group_by(.data$aq_bucket) |>
    dplyr::summarise(
      attempts = dplyr::n(),
      made_attempts = sum(.data$shot_result == "Made", na.rm = TRUE),
      actual_points = sum(.data$actual_points, na.rm = TRUE),
      sample_expected_points = .data$actual_points / .data$attempts,
      .groups = "drop"
    ) |>
    tidyr::complete(
      aq_bucket = bucket_levels,
      fill = list(
        attempts = 0,
        made_attempts = 0,
        actual_points = 0,
        sample_expected_points = NA_real_
      )
    ) |>
    dplyr::left_join(
      fallback_expected_points_by_bucket(),
      by = "aq_bucket"
    ) |>
    dplyr::mutate(
      expected_points = dplyr::if_else(
        .data$attempts >= min_attempts & !is.na(.data$sample_expected_points),
        .data$sample_expected_points,
        .data$fallback_expected_points
      ),
      expected_points_source = dplyr::if_else(
        .data$attempts >= min_attempts & !is.na(.data$sample_expected_points),
        "sample",
        "fallback"
      ),
      aq_bucket = factor(.data$aq_bucket, levels = bucket_levels)
    ) |>
    dplyr::arrange(.data$aq_bucket)
}

apply_expected_points <- function(scored_shots, expected_points_table) {
  scored_shots |>
    dplyr::mutate(aq_bucket = as.character(.data$aq_bucket)) |>
    dplyr::left_join(
      expected_points_table |>
        dplyr::mutate(aq_bucket = as.character(.data$aq_bucket)) |>
        dplyr::select("aq_bucket", "expected_points"),
      by = "aq_bucket"
    ) |>
    dplyr::mutate(
      expected_points = dplyr::coalesce(.data$expected_points, 0),
      shot_making_value = .data$actual_points - .data$expected_points,
      aq_bucket = factor(
        .data$aq_bucket,
        levels = c("9", "7", "5", "3", "1", "Grenade")
      )
    )
}


# Main scorer ------------------------------------------------------------------

build_attempt_quality_outputs <- function(
    season = "2024-25",
    season_type = "Regular Season",
    min_attempts = 500,
    write_csv = TRUE,
    force_rebuild = TRUE
) {
  enriched_path <- build_enriched_shots_path(season, season_type)
  
  if (!file.exists(enriched_path)) {
    stop("Missing enriched shots file: ", enriched_path)
  }
  
  scored_out <- build_model_output_path("scored_shots", season, season_type)
  expected_out <- build_model_output_path("expected_points_by_bucket", season, season_type)
  
  if (!force_rebuild && file.exists(scored_out) && file.exists(expected_out)) {
    message("Already built Attempt Quality outputs for ", season, " / ", season_type)
    return(
      list(
        scored_shots = readr::read_rds(scored_out),
        expected_points = readr::read_rds(expected_out)
      )
    )
  }
  
  message("\nBuilding Attempt Quality outputs")
  message("- Season: ", season)
  message("- Season type: ", season_type)
  
  enriched_shots <- readr::read_rds(enriched_path)
  
  scored_shots_base <- enriched_shots |>
    assign_attempt_quality_bucket() |>
    dplyr::mutate(
      aq_score = bucket_to_attempt_quality_score(.data$aq_bucket)
    )
  
  expected_points <- build_expected_points_by_bucket(
    scored_shots = scored_shots_base,
    min_attempts = min_attempts
  )
  
  scored_shots <- apply_expected_points(
    scored_shots = scored_shots_base,
    expected_points_table = expected_points
  )
  
  bucket_summary <- scored_shots |>
    dplyr::count(.data$aq_bucket, sort = FALSE) |>
    dplyr::left_join(
      expected_points |> dplyr::mutate(aq_bucket = as.character(.data$aq_bucket)),
      by = c("aq_bucket")
    )
  
  qa_summary <- scored_shots |>
    dplyr::summarise(
      rows = dplyr::n(),
      games = dplyr::n_distinct(.data$game_id),
      players = dplyr::n_distinct(.data$player_id),
      non_grenade_attempts = sum(.data$aq_bucket != "Grenade", na.rm = TRUE),
      grenade_attempts = sum(.data$aq_bucket == "Grenade", na.rm = TRUE),
      avg_aq_score = mean(.data$aq_score, na.rm = TRUE),
      total_actual_points = sum(.data$actual_points, na.rm = TRUE),
      total_expected_points = sum(.data$expected_points, na.rm = TRUE),
      total_shot_making_value = sum(.data$shot_making_value, na.rm = TRUE)
    )
  
  message("\nAttempt Quality QA summary:")
  print(qa_summary)
  
  message("\nBucket summary:")
  print(bucket_summary, n = 25)
  
  readr::write_rds(scored_shots, scored_out)
  readr::write_rds(expected_points, expected_out)
  
  if (write_csv) {
    readr::write_csv(
      scored_shots,
      stringr::str_replace(scored_out, "\\.rds$", ".csv")
    )
    
    readr::write_csv(
      expected_points,
      stringr::str_replace(expected_out, "\\.rds$", ".csv")
    )
  }
  
  message("\nSaved Attempt Quality outputs:")
  message("- ", scored_out)
  message("- ", expected_out)
  
  invisible(
    list(
      scored_shots = scored_shots,
      expected_points = expected_points,
      qa_summary = qa_summary,
      bucket_summary = bucket_summary
    )
  )
}