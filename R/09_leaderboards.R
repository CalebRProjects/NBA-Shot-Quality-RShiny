# Leaderboards and status tables ----------------------------------------------

build_leaderboard <- function(player_summary, metric, min_games = 1, min_fga = 0, n = 10) {
  if (nrow(player_summary) == 0) return(tibble::tibble())

  metric <- rlang::sym(metric)

  player_summary |>
    filter(games >= min_games, fga >= min_fga) |>
    arrange(desc(!!metric)) |>
    slice_head(n = n) |>
    mutate(rank = row_number()) |>
    select(
      rank, player_name, games, fga,
      avg_sq_score, avg_shot_making_score, avg_pq_score, total_pq_score
    )
}

methodology_field_table <- function() {
  tibble::tribble(
    ~Field, ~Current_Status, ~Notes,
    "Shot attempts", "Automated", "Pulled from NBA.com play-by-play shot events.",
    "Shot value", "Automated", "Uses PBP shot_value when available.",
    "Shot distance", "Automated", "Uses PBP shot_distance when available.",
    "Shot type/action", "Automated/proxy", "Uses action_type, sub_type, and description text.",
    "Clock context", "Automated/proxy", "Used for late-clock bump and prayer detection.",
    "Contest quality", "Future/manual", "Not fully captured by public PBP.",
    "Defensive matchup/context", "Future/manual", "Could use matchup or tracking endpoints later.",
    "Assists", "Automated", "From player game logs.",
    "Potential assists", "Future/proxy", "Add tracking endpoint later; calibrate by AST / potential AST conversion.",
    "Turnovers", "Automated", "From player game logs.",
    "Steals", "Automated", "From player game logs.",
    "Blocks", "Automated", "From player game logs.",
    "Offensive rebounds", "Automated", "From player game logs.",
    "Deflections", "Future/proxy", "Add hustle/tracking endpoint later; should be partial turnover-creation value.",
    "Charges drawn", "Future/manual or endpoint", "Use if available through hustle data, otherwise film/manual.",
    "Fouls drawn", "Future/proxy", "Can estimate from FTA/play descriptions, but should be labeled carefully."
  )
}

missing_or_estimated_fields <- function() {
  methodology_field_table() |>
    filter(stringr::str_detect(Current_Status, "proxy|Future|manual"))
}
