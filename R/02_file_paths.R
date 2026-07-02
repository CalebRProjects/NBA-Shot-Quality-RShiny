# Player lookup ---------------------------------------------------------------
# Step 1 assumes user supplies player IDs for cache building.
# Future version can expand this through nba_commonallplayers(), nba_playerindex(),
# or a maintained static lookup file.

build_player_lookup_from_game_logs <- function(game_logs) {
  if (nrow(game_logs) == 0) {
    return(tibble::tibble(player_id = character(), player_name = character()))
  }
  
  if (!"player_name" %in% names(game_logs)) {
    game_logs$player_name <- as.character(game_logs$player_id)
  }
  
  game_logs |>
    distinct(player_id, player_name) |>
    arrange(player_name)
}

build_player_choices <- function(player_summary) {
  if (is.null(player_summary) || nrow(player_summary) == 0) {
    return(c("No cache found" = ""))
  }

  choices <- player_summary$player_id
  names(choices) <- player_summary$player_name
  choices
}

default_player_id <- function(player_summary) {
  if (is.null(player_summary) || nrow(player_summary) == 0) return("")
  player_summary |>
    arrange(desc(games), desc(fga)) |>
    slice(1) |>
    pull(player_id)
}
