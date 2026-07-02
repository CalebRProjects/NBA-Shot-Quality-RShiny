# Pull player game logs --------------------------------------------------------

pull_player_game_log <- function(player_id, season = DEFAULT_SEASON, season_type = DEFAULT_SEASON_TYPE) {
  message(glue::glue("Pulling game log: player_id={player_id}, season={season}, season_type={season_type}"))
  
  raw <- hoopR::nba_playergamelogs(
    player_id = player_id,
    season = season,
    season_type = season_type
  )
  
  out <- pluck_table(raw, "PlayerGameLogs")
  
  if (nrow(out) == 0) {
    out <- pluck_table(raw, "player_game_logs")
  }
  
  if (nrow(out) == 0) {
    out <- pluck_table(raw, "PlayerGameLog")
  }
  
  if (nrow(out) == 0) return(tibble::tibble())
  
  names(out) <- janitor::make_clean_names(names(out))
  
  id_cols <- intersect(
    c("player_id", "person_id", "nba_person_id"),
    names(out)
  )
  
  if (length(id_cols) == 0) {
    stop("No player ID column found in nba_playergamelogs() output.")
  }
  
  out <- out |>
    mutate(player_id = as.character(.data[[id_cols[1]]])) |>
    filter(player_id == as.character(!!player_id))
  
  if (nrow(out) == 0) return(tibble::tibble())
  
  name_cols <- intersect(
    c("player_name", "player_name_i", "player", "name"),
    names(out)
  )
  
  if (length(name_cols) > 0) {
    out$player_name <- as.character(out[[name_cols[1]]])
  } else {
    out$player_name <- as.character(player_id)
  }
  
  if (!"game_id" %in% names(out)) {
    game_cols <- intersect(c("game_id", "gameid"), names(out))
    if (length(game_cols) > 0) {
      out$game_id <- out[[game_cols[1]]]
    } else {
      stop("No game_id column found in game log output.")
    }
  }
  
  if ("game_date" %in% names(out)) {
    out$game_date <- suppressWarnings(lubridate::parse_date_time(
      as.character(out$game_date),
      orders = c("ymd", "mdy", "dmy", "b d, Y", "B d, Y", "a b d, Y", "Ymd", "ymd HMS")
    )) |> as.Date()
  } else {
    out$game_date <- as.Date(NA)
  }
  
  numeric_cols <- intersect(
    c(
      "min", "minutes", "fgm", "fga", "fg3m", "fg3a", "ftm", "fta",
      "oreb", "dreb", "reb", "ast", "stl", "blk", "tov", "turnovers",
      "pf", "pfd", "pts", "plus_minus"
    ),
    names(out)
  )
  
  out |>
    mutate(
      game_id = as.character(game_id),
      across(
        all_of(numeric_cols),
        ~ suppressWarnings(as.numeric(.x))
      )
    ) |>
    rename(
      tov = any_of("turnovers"),
      min = any_of("minutes")
    )
}

pull_many_player_game_logs <- function(player_ids, season = DEFAULT_SEASON, season_type = DEFAULT_SEASON_TYPE) {
  purrr::map_dfr(
    player_ids,
    ~ safely_pull_game_log(.x, season = season, season_type = season_type)
  )
}

safely_pull_game_log <- function(player_id, season = DEFAULT_SEASON, season_type = DEFAULT_SEASON_TYPE) {
  tryCatch(
    pull_player_game_log(player_id, season = season, season_type = season_type),
    error = function(e) {
      message(glue::glue("Game log failed for {player_id}: {e$message}"))
      tibble::tibble()
    }
  )
}