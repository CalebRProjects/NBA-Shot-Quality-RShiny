# Shot Quality rules -----------------------------------------------------------
# These rules are intentionally simple and transparent.
# They are public-data proxies, not a film-grade contest/location/context model.

score_shot_quality <- function(shot_events) {
  if (nrow(shot_events) == 0) return(tibble::tibble())

  shot_events |>
    mutate(
      sq_bucket = assign_sq_bucket(
        shot_value = shot_value,
        shot_distance = shot_distance,
        action_type = action_type,
        sub_type = sub_type,
        description = description,
        seconds_remaining = seconds_remaining
      ),
      sq_score = bucket_to_score(sq_bucket),
      expected_points = unname(SQ_EXPECTED_POINTS[as.character(sq_bucket)]),
      actual_points = if_else(shot_result == "Made", shot_value, 0),
      shot_making_score = actual_points - expected_points,
      is_prayer = sq_bucket == "Prayer"
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
    !is.na(sec) & sec <= 2 & dist >= 35 ~ "Prayer",

    stringr::str_detect(desc, "dunk|layup|finger roll|alley oop") & dist <= 5 ~ "9",
    dist <= 4 ~ "9",

    stringr::str_detect(desc, "cut|putback|driving") & dist <= 8 ~ "7",
    val == 3 & dist <= 24 ~ "7",

    dist <= 14 ~ "5",
    val == 3 & dist <= 28 ~ "5",

    dist <= 34 ~ "3",

    TRUE ~ "1"
  )

  # Late-clock bump for non-prayer shots.
  # Logic: a difficult attempt late in the clock should be tracked as a lower-burden process event.
  late_clock <- !is.na(sec) & sec <= 4 & bucket != "Prayer"

  bucket <- dplyr::case_when(
    late_clock & bucket == "1" ~ "3",
    late_clock & bucket == "3" ~ "5",
    TRUE ~ bucket
  )

  factor(bucket, levels = c("9", "7", "5", "3", "1", "Prayer"))
}

bucket_to_score <- function(bucket) {
  dplyr::case_when(
    as.character(bucket) == "9" ~ 9,
    as.character(bucket) == "7" ~ 7,
    as.character(bucket) == "5" ~ 5,
    as.character(bucket) == "3" ~ 3,
    as.character(bucket) == "1" ~ 1,
    as.character(bucket) == "Prayer" ~ NA_real_,
    TRUE ~ NA_real_
  )
}
