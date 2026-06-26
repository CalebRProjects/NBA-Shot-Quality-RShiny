# Shot Quality rules -----------------------------------------------------------
# These rules are intentionally simple and transparent.
# They are public-data proxies, not a film-grade contest/location/context model.

score_shot_quality <- function(shot_events) {
  if (nrow(shot_events) == 0) return(tibble::tibble())
  
  shot_events |>
    dplyr::mutate(
      sq_bucket = assign_sq_bucket(
        shot_value = .data$shot_value,
        shot_distance = .data$shot_distance,
        action_type = .data$action_type,
        sub_type = .data$sub_type,
        description = .data$description,
        seconds_remaining = .data$seconds_remaining
      ),
      sq_score = bucket_to_score(.data$sq_bucket),
      actual_points = dplyr::if_else(
        .data$shot_result == "Made",
        as.numeric(.data$shot_value),
        0
      ),
      is_grenade = .data$sq_bucket == "Grenade"
    )
}

assign_sq_bucket <- function(
    shot_value,
    shot_distance,
    action_type,
    sub_type,
    description,
    seconds_remaining
) {
  desc <- stringr::str_to_lower(paste(action_type, sub_type, description, sep = " "))
  dist <- suppressWarnings(as.numeric(shot_distance))
  val <- suppressWarnings(as.numeric(shot_value))
  sec <- suppressWarnings(as.numeric(seconds_remaining))
  
  bucket <- dplyr::case_when(
    !is.na(sec) & sec <= 2 & dist >= 35 ~ "Grenade",
    
    # Elite rim attempts
    stringr::str_detect(desc, "dunk|layup|finger roll|alley oop") & dist <= 5 ~ "9",
    dist <= 4 ~ "9",
    
    # High-value attempts
    val == 3 & dist <= 31 ~ "7",
    stringr::str_detect(desc, "cut|putback|alley oop|dunk") & dist <= 8 ~ "7",
    
    # Neutral/acceptable shots
    dist <= 10 ~ "5",
    
    # Low-value twos
    val == 2 & dist <= 22 ~ "3",
    
    # Very deep threes, but not grenades
    val == 3 & dist <= 34 ~ "3",
    
    TRUE ~ "1"
  )
  
  # Late-clock bump for non-grenade shots.
  # Logic: a difficult attempt late in the clock should be tracked as a lower-burden process event.
  late_clock <- !is.na(sec) & sec <= 4 & bucket != "Grenade"
  
  bucket <- dplyr::case_when(
    late_clock & bucket == "1" ~ "3",
    late_clock & bucket == "3" ~ "5",
    TRUE ~ bucket
  )
  
  factor(bucket, levels = c("9", "7", "5", "3", "1", "Grenade"))
}

bucket_to_score <- function(bucket) {
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

# Expected points by bucket ----------------------------------------------------
# Current version calculates expected points from the active cache sample.
# Once the cache includes the full league, this becomes league-average expected
# points by bucket for that season / season type.

fallback_expected_points_by_bucket <- function() {
  tibble::tibble(
    sq_bucket = c("9", "7", "5", "3", "1", "Grenade"),
    fallback_expected_points = c(1.35, 1.15, 1.00, 0.80, 0.55, 0.20)
  )
}

build_expected_points_by_bucket <- function(scored_shots) {
  bucket_levels <- c("9", "7", "5", "3", "1", "Grenade")
  
  scored_shots |>
    dplyr::mutate(
      sq_bucket = as.character(.data$sq_bucket),
      actual_points = dplyr::if_else(
        .data$shot_result == "Made",
        as.numeric(.data$shot_value),
        0
      )
    ) |>
    dplyr::filter(.data$sq_bucket %in% bucket_levels) |>
    dplyr::group_by(.data$sq_bucket) |>
    dplyr::summarise(
      attempts = dplyr::n(),
      made_attempts = sum(.data$shot_result == "Made", na.rm = TRUE),
      actual_points = sum(.data$actual_points, na.rm = TRUE),
      sample_expected_points = .data$actual_points / .data$attempts,
      .groups = "drop"
    ) |>
    tidyr::complete(
      sq_bucket = bucket_levels,
      fill = list(
        attempts = 0,
        made_attempts = 0,
        actual_points = 0,
        sample_expected_points = NA_real_
      )
    ) |>
    dplyr::left_join(
      fallback_expected_points_by_bucket(),
      by = "sq_bucket"
    ) |>
    dplyr::mutate(
      expected_points = dplyr::if_else(
        .data$attempts >= 500 & !is.na(.data$sample_expected_points),
        .data$sample_expected_points,
        .data$fallback_expected_points
      ),
      sq_bucket = factor(.data$sq_bucket, levels = bucket_levels)
    ) |>
    dplyr::arrange(.data$sq_bucket)
}

apply_expected_points <- function(scored_shots, expected_points_table) {
  scored_shots |>
    dplyr::mutate(
      sq_bucket = as.character(.data$sq_bucket),
      actual_points = dplyr::if_else(
        .data$shot_result == "Made",
        as.numeric(.data$shot_value),
        0
      )
    ) |>
    dplyr::left_join(
      expected_points_table |>
        dplyr::mutate(sq_bucket = as.character(.data$sq_bucket)) |>
        dplyr::select("sq_bucket", "expected_points"),
      by = "sq_bucket"
    ) |>
    dplyr::mutate(
      expected_points = dplyr::coalesce(.data$expected_points, 0),
      shot_making_value = .data$actual_points - .data$expected_points,
      
      # Keep this name too so older summary code does not break.
      shot_making_score = .data$shot_making_value
    )
}