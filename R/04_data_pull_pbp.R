# Pull play-by-play shot events ------------------------------------------------

pull_game_pbp <- function(game_id) {
  message(glue::glue("Pulling PBP: game_id={game_id}"))

  raw <- hoopR::nba_playbyplayv3(game_id = game_id)

  # User note: after cleaning the outer list, shot events live in play_by_play.
  pbp <- pluck_table(raw, "play_by_play")

  if (nrow(pbp) == 0) return(tibble::tibble())

  pbp |>
    mutate(
      game_id = as.character(game_id),
      player_id = as.character(person_id),
      seconds_remaining = parse_clock_seconds(clock),
      is_field_goal = as.logical(is_field_goal),
      shot_value = suppressWarnings(as.numeric(shot_value)),
      shot_distance = suppressWarnings(as.numeric(shot_distance)),
      x_legacy = suppressWarnings(as.numeric(x_legacy)),
      y_legacy = suppressWarnings(as.numeric(y_legacy))
    )
}

pull_many_game_pbp <- function(game_ids) {
  purrr::map_dfr(
    unique(game_ids),
    safely_pull_game_pbp
  )
}

safely_pull_game_pbp <- function(game_id) {
  tryCatch(
    pull_game_pbp(game_id),
    error = function(e) {
      warning(glue::glue("PBP failed for {game_id}: {e$message}"))
      tibble::tibble()
    }
  )
}

filter_shot_events <- function(pbp) {
  if (nrow(pbp) == 0) return(tibble::tibble())

  pbp |>
    filter(is_field_goal %in% TRUE) |>
    transmute(
      game_id,
      action_number,
      period,
      clock,
      seconds_remaining,
      player_id,
      player_name,
      team_tricode,
      shot_result,
      shot_value,
      shot_distance,
      description,
      action_type,
      sub_type,
      x_legacy,
      y_legacy
    )
}
