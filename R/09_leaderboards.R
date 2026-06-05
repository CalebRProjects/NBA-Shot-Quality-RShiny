# Leaderboards and status tables ----------------------------------------------

build_leaderboard <- function(player_summary, metric, min_games = 1, min_fga = 0, n = 10) {
  if (nrow(player_summary) == 0) return(tibble::tibble())
  
  metric_sym <- rlang::sym(metric)
  
  player_summary |>
    filter(games >= min_games, fga >= min_fga) |>
    arrange(desc(!!metric_sym)) |>
    slice_head(n = n) |>
    mutate(
      Rank = row_number(),
      `Attempt Quality` = round(avg_sq_score, 2),
      `Shot Making` = round(avg_shot_making_score, 2),
      `Possession Quality` = round(avg_pq_score, 2),
      `Total PQ` = round(total_pq_score, 1),
      `Avg Shot Distance` = round(avg_shot_distance, 1),
      `Rim%` = scales::percent(rim_rate, accuracy = 0.1),
      `3PA%` = scales::percent(three_rate, accuracy = 0.1),
      `Grenade%` = scales::percent(grenade_rate, accuracy = 0.1)
    ) |>
    select(
      Rank,
      Player = player_name,
      Games = games,
      FGA = fga,
      `Attempt Quality`,
      `Rim%`,
      `3PA%`,
      `Avg Shot Distance`,
      `Grenade%`,
      `Shot Making`,
      `Possession Quality`,
      `Total PQ`
    )
}

methodology_field_table <- function() {
  tibble::tribble(
    ~Field, ~Current_Status, ~Notes,
    
    "Shot attempts", "Automated", "Pulled from NBA.com play-by-play shot events.",
    "Shot value", "Automated", "Uses play-by-play shot_value when available.",
    "Shot distance", "Automated", "Uses play-by-play shot_distance when available.",
    "Shot type/action", "Automated proxy", "Uses action_type, sub_type, and description text.",
    "Clock context", "Automated proxy", "Used for late-clock adjustment and grenade detection.",
    "Grenade attempts", "Automated proxy", "Late-clock bailout or heave attempts tracked separately.",
    
    "Rim ≤5 ft rate", "Automated proxy", "Share of play-by-play shot attempts with shot_distance <= 5.",
    "3PA rate", "Automated", "Share of play-by-play shot attempts where shot_value == 3.",
    "Average shot distance", "Automated", "Average play-by-play shot_distance.",
    
    "Assists", "Automated", "From player game logs.",
    "Turnovers", "Automated", "From player game logs.",
    "Steals", "Automated", "From player game logs.",
    "Blocks", "Automated", "From player game logs.",
    "Offensive rebounds", "Automated", "From player game logs.",
    "Personal fouls", "Automated", "From player game logs.",
    "Fouls drawn", "Automated", "From player game logs using pfd.",
    
    "Potential assists", "Future", "Add tracking endpoint later; calibrate by AST / potential AST conversion.",
    "Deflections", "Future", "Add hustle/tracking endpoint later; should be partial turnover-creation value.",
    "Charges drawn", "Future", "Use if available through hustle data, otherwise film/manual.",
    "Contest quality", "Future/manual", "Not fully captured by public play-by-play.",
    "Defensive matchup/context", "Future/manual", "Could use matchup, tracking, or film tags later.",
    "Creation burden", "Future/proxy", "Needed to separate self-created difficulty from attempt quality."
  )
}

missing_or_estimated_fields <- function() {
  methodology_field_table() |>
    filter(stringr::str_detect(Current_Status, "proxy|Future|manual"))
}
