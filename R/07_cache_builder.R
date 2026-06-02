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

build_sq_cache <- function(
    player_ids,
    season = DEFAULT_SEASON,
    season_type = DEFAULT_SEASON_TYPE,
    force_refresh = FALSE
) {
  safe_dir_create(CACHE_DIR)

  if (!force_refresh && all(file.exists(unlist(CACHE_FILES)))) {
    message("Cache already exists. Use force_refresh = TRUE to rebuild.")
    return(load_app_cache())
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

  shot_game_summary <- build_shot_game_summary(scored_shots)
  game_summary <- calculate_possession_quality(game_logs, shot_game_summary)
  player_summary <- build_player_summary(game_summary)
  player_lookup <- build_player_lookup_from_game_logs(game_logs)

  metadata <- list(
    built_at = as.character(Sys.time()),
    season = season,
    season_type = season_type,
    players_requested = as.character(player_ids),
    players_processed = nrow(player_summary),
    games_processed = dplyr::n_distinct(game_summary$game_id),
    shot_events_processed = nrow(scored_shots)
  )

  safe_write_rds(metadata, CACHE_FILES$metadata)
  safe_write_rds(player_lookup, CACHE_FILES$player_lookup)
  safe_write_rds(game_logs, CACHE_FILES$game_logs)
  safe_write_rds(scored_shots, CACHE_FILES$shot_events)
  safe_write_rds(game_summary, CACHE_FILES$game_summary)
  safe_write_rds(player_summary, CACHE_FILES$player_summary)

  load_app_cache()
}

load_app_cache <- function() {
  list(
    metadata = safe_read_rds(CACHE_FILES$metadata, default = list()),
    player_lookup = safe_read_rds(CACHE_FILES$player_lookup),
    game_logs = safe_read_rds(CACHE_FILES$game_logs),
    shot_events = safe_read_rds(CACHE_FILES$shot_events),
    game_summary = safe_read_rds(CACHE_FILES$game_summary),
    player_summary = safe_read_rds(CACHE_FILES$player_summary)
  )
}
