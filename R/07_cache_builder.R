# Cache builder ----------------------------------------------------------------
# Run this script manually/local before launching or deploying app.
#
# Example:
#   source("R/07_cache_builder.R")
#   build_sq_cache(
#     player_ids = c("203999", "1629029", "1628369"),
#     season = "2024-25",
#     season_type = "Playoffs",
#     force_refresh = TRUE

required_pkgs <- c(
  "tidyverse", "hoopR", "janitor", "lubridate",
  "stringr", "readr", "scales", "glue"
)

invisible(lapply(required_pkgs, require, character.only = TRUE))

source("R/00_config.R")
source("R/01_helpers.R")
source("R/03_data_pull_game_logs.R")
source("R/04_data_pull_pbp.R")
source("R/05_shot_quality_rules.R")
source("R/06_possession_quality.R")
source("R/08_player_summaries.R")
source("R/02_player_lookup.R")
source("R/12_shot_profile.R")

build_sq_cache <- function(
    player_ids,
    season = DEFAULT_SEASON,
    season_type = DEFAULT_SEASON_TYPE,
    force_refresh = FALSE
) {
  cache_dir <- get_cache_dir(season = season, season_type = season_type)
  cache_files <- get_cache_files(season = season, season_type = season_type)
  
  safe_dir_create(cache_dir)
  
  if (!force_refresh && all(file.exists(unlist(cache_files)))) {
    message("Cache already exists for ", season, " / ", season_type, ". Use force_refresh = TRUE to rebuild.")
    return(load_app_cache(season = season, season_type = season_type))
  }
  
  game_logs <- pull_many_player_game_logs(
    player_ids = player_ids,
    season = season,
    season_type = season_type
  )
  
  if (nrow(game_logs) == 0) {
    stop("No game logs returned. Check player IDs, season, season_type, or NBA.com availability.")
  }
  
  game_ids <- unique(game_logs$game_id)
  
  pbp <- pull_many_game_pbp(game_ids)
  shot_events <- filter_shot_events(pbp)
  scored_shots <- score_shot_quality(shot_events)
  
  pbp_available_game_ids <- scored_shots |>
    dplyr::filter(!is.na(.data$game_id)) |>
    dplyr::distinct(.data$game_id) |>
    dplyr::pull(.data$game_id) |>
    as.character()
  
  game_logs <- game_logs |>
    dplyr::mutate(game_id = as.character(.data$game_id))
  
  missing_pbp_game_ids <- setdiff(
    unique(game_logs$game_id),
    pbp_available_game_ids
  )
  
  if (length(missing_pbp_game_ids) > 0) {
    message(
      "Dropping ",
      length(missing_pbp_game_ids),
      " games without available play-by-play shot data: ",
      paste(missing_pbp_game_ids, collapse = ", ")
    )
  }
  
  game_logs <- game_logs |>
    dplyr::filter(!.data$game_id %in% missing_pbp_game_ids)
  
  scored_shots <- scored_shots |>
    dplyr::filter(.data$game_id %in% unique(game_logs$game_id))
  
  shot_game_summary <- build_shot_game_summary(scored_shots)
  game_summary <- calculate_possession_quality(game_logs, shot_game_summary)
  
  player_summary <- build_player_summary(game_summary)
  
  shot_profile_summary <- build_shot_profile_summary(scored_shots)
  
  player_summary <- player_summary |>
    dplyr::left_join(
      shot_profile_summary |>
        dplyr::select(
          player_id,
          fga_from_pbp,
          non_grenade_fga,
          grenade_attempts,
          rim_attempts,
          midrange_attempts,
          three_attempts,
          rim_rate,
          midrange_rate,
          three_rate,
          grenade_rate,
          avg_shot_distance,
          bucket_9_rate,
          bucket_7_rate,
          bucket_5_rate,
          bucket_3_rate,
          bucket_1_rate
        ),
      by = "player_id"
    )
  
  player_lookup <- build_player_lookup_from_game_logs(game_logs)
  
  metadata <- list(
    built_at = as.character(Sys.time()),
    season = season,
    season_type = season_type,
    cache_dir = cache_dir,
    players_requested = as.character(player_ids),
    players_processed = nrow(player_summary),
    games_requested = length(game_ids),
    games_processed = dplyr::n_distinct(game_summary$game_id),
    pbp_missing_games_count = length(missing_pbp_game_ids),
    pbp_missing_games = missing_pbp_game_ids,
    shot_events_processed = nrow(scored_shots)
  )
  
  safe_write_rds(metadata, cache_files$metadata)
  safe_write_rds(player_lookup, cache_files$player_lookup)
  safe_write_rds(game_logs, cache_files$game_logs)
  safe_write_rds(scored_shots, cache_files$shot_events)
  safe_write_rds(game_summary, cache_files$game_summary)
  safe_write_rds(player_summary, cache_files$player_summary)
  
  load_app_cache(season = season, season_type = season_type)
}

load_app_cache <- function(
    season = DEFAULT_SEASON,
    season_type = DEFAULT_SEASON_TYPE
) {
  cache_files <- get_cache_files(season = season, season_type = season_type)
  
  missing_files <- names(cache_files)[!file.exists(unlist(cache_files))]
  
  if (length(missing_files) > 0) {
    stop(
      "Missing cache files for ",
      season,
      " / ",
      season_type,
      ": ",
      paste(missing_files, collapse = ", "),
      call. = FALSE
    )
  }
  
  list(
    metadata = readRDS(cache_files$metadata),
    player_lookup = readRDS(cache_files$player_lookup),
    game_logs = readRDS(cache_files$game_logs),
    shot_events = readRDS(cache_files$shot_events),
    game_summary = readRDS(cache_files$game_summary),
    player_summary = readRDS(cache_files$player_summary)
  )
}
