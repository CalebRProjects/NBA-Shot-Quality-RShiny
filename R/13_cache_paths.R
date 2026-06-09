clean_cache_label <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")
}

get_cache_dir <- function(season = APP_SEASON, season_type = APP_SEASON_TYPE) {
  file.path(
    "data",
    "cache",
    clean_cache_label(season),
    clean_cache_label(season_type)
  )
}

get_cache_files <- function(season = APP_SEASON, season_type = APP_SEASON_TYPE) {
  cache_dir <- get_cache_dir(season, season_type)
  
  list(
    metadata = file.path(cache_dir, "cache_metadata.rds"),
    player_lookup = file.path(cache_dir, "player_lookup.rds"),
    game_logs = file.path(cache_dir, "game_logs.rds"),
    pbp = file.path(cache_dir, "pbp_raw.rds"),
    shot_events = file.path(cache_dir, "shot_events_scored.rds"),
    game_summary = file.path(cache_dir, "game_summary.rds"),
    player_summary = file.path(cache_dir, "player_summary.rds")
  )
}