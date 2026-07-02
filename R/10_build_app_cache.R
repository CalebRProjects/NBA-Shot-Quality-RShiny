# R/10_build_app_cache.R
# Builds Shiny-ready app cache objects from combined player outputs and scored shots.

required_pkgs <- c("tidyverse", "readr", "stringr", "glue", "janitor")
invisible(lapply(required_pkgs, require, character.only = TRUE))


# Paths ------------------------------------------------------------------------

PROCESSED_COMBINED_DIR <- "data/processed/combined"
PROCESSED_MODEL_DIR <- "data/processed/model_outputs"
CACHE_DIR <- "data/cache"

dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)


# Helpers ----------------------------------------------------------------------

season_to_data_year <- function(season) {
  as.integer(stringr::str_sub(season, 1, 4))
}

season_type_file_label <- function(season_type) {
  if (season_type == "Regular Season") {
    "regular"
  } else if (season_type == "Playoffs") {
    "playoffs"
  } else {
    stop("Unsupported season_type: ", season_type)
  }
}

build_input_path <- function(base_dir, prefix, season, season_type) {
  data_year <- season_to_data_year(season)
  type_label <- season_type_file_label(season_type)
  
  file.path(
    base_dir,
    paste0(prefix, "_", data_year, "_", type_label, ".rds")
  )
}

build_cache_path <- function(season, season_type) {
  data_year <- season_to_data_year(season)
  type_label <- season_type_file_label(season_type)
  
  file.path(
    CACHE_DIR,
    paste0("app_cache_", data_year, "_", type_label, ".rds")
  )
}

safe_divide <- function(num, denom) {
  dplyr::if_else(
    !is.na(denom) & denom > 0,
    num / denom,
    NA_real_
  )
}


# Lookup tables ----------------------------------------------------------------

build_player_lookup <- function(combined_player_summary) {
  combined_player_summary |>
    dplyr::transmute(
      player_id,
      player_name,
      team_id,
      team_tricode,
      games,
      fga,
      search_label = paste0(player_name, " - ", team_tricode),
      search_key = stringr::str_to_lower(
        paste(player_name, team_tricode, player_id)
      )
    ) |>
    dplyr::arrange(player_name, team_tricode)
}


# Leaderboards -----------------------------------------------------------------

ensure_numeric_columns <- function(data, cols) {
  missing_cols <- setdiff(cols, names(data))
  
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      data[[col]] <- 0
    }
  }
  
  data
}

build_player_totals <- function(combined_player_summary) {
  needed_numeric_cols <- c(
    "bucket_9_fga",
    "bucket_7_fga",
    "bucket_5_fga",
    "bucket_3_fga",
    "bucket_1_fga",
    "grenade_fga",
    "rim_fga",
    "paint_non_ra_fga",
    "midrange_fga",
    "corner_three_fga",
    "above_break_three_fga",
    "three_pa",
    "three_pm",
    "fgm",
    "fga",
    "pts_from_shots",
    "expected_points",
    "shot_making_value",
    "non_grenade_fga",
    "pq_events",
    "possession_quality_value",
    "made_shots",
    "missed_shots",
    "assists",
    "turnovers",
    "offensive_rebounds",
    "defensive_rebounds",
    "unknown_rebounds",
    "steals",
    "blocks",
    "offensive_fouls",
    "made_free_throws",
    "missed_free_throws"
  )
  
  combined_player_summary <- combined_player_summary |>
    ensure_numeric_columns(needed_numeric_cols)
  
  single_team_players <- combined_player_summary |>
    dplyr::group_by(season, season_type, player_id, player_name) |>
    dplyr::filter(dplyr::n_distinct(team_tricode) == 1) |>
    dplyr::ungroup()
  
  traded_player_totals <- combined_player_summary |>
    dplyr::group_by(season, season_type, player_id, player_name) |>
    dplyr::filter(dplyr::n_distinct(team_tricode) > 1) |>
    dplyr::summarise(
      team_id = "TOT",
      team_tricode = "TOT",
      
      games = sum(games, na.rm = TRUE),
      fga = sum(fga, na.rm = TRUE),
      fgm = sum(fgm, na.rm = TRUE),
      three_pa = sum(three_pa, na.rm = TRUE),
      three_pm = sum(three_pm, na.rm = TRUE),
      pts_from_shots = sum(pts_from_shots, na.rm = TRUE),
      
      expected_points = sum(expected_points, na.rm = TRUE),
      shot_making_value = sum(shot_making_value, na.rm = TRUE),
      
      non_grenade_fga = sum(non_grenade_fga, na.rm = TRUE),
      aq_score_sum = sum(avg_aq_score * non_grenade_fga, na.rm = TRUE),
      
      pq_events = sum(pq_events, na.rm = TRUE),
      possession_quality_value = sum(possession_quality_value, na.rm = TRUE),
      
      made_shots = sum(made_shots, na.rm = TRUE),
      missed_shots = sum(missed_shots, na.rm = TRUE),
      assists = sum(assists, na.rm = TRUE),
      turnovers = sum(turnovers, na.rm = TRUE),
      offensive_rebounds = sum(offensive_rebounds, na.rm = TRUE),
      defensive_rebounds = sum(defensive_rebounds, na.rm = TRUE),
      unknown_rebounds = sum(unknown_rebounds, na.rm = TRUE),
      steals = sum(steals, na.rm = TRUE),
      blocks = sum(blocks, na.rm = TRUE),
      offensive_fouls = sum(offensive_fouls, na.rm = TRUE),
      made_free_throws = sum(made_free_throws, na.rm = TRUE),
      missed_free_throws = sum(missed_free_throws, na.rm = TRUE),
      
      bucket_9_fga = sum(bucket_9_fga, na.rm = TRUE),
      bucket_7_fga = sum(bucket_7_fga, na.rm = TRUE),
      bucket_5_fga = sum(bucket_5_fga, na.rm = TRUE),
      bucket_3_fga = sum(bucket_3_fga, na.rm = TRUE),
      bucket_1_fga = sum(bucket_1_fga, na.rm = TRUE),
      grenade_fga = sum(grenade_fga, na.rm = TRUE),
      
      rim_fga = sum(rim_fga, na.rm = TRUE),
      paint_non_ra_fga = sum(paint_non_ra_fga, na.rm = TRUE),
      midrange_fga = sum(midrange_fga, na.rm = TRUE),
      corner_three_fga = sum(corner_three_fga, na.rm = TRUE),
      above_break_three_fga = sum(above_break_three_fga, na.rm = TRUE),
      
      .groups = "drop"
    ) |>
    dplyr::mutate(
      fg_pct = dplyr::if_else(fga > 0, fgm / fga, NA_real_),
      three_pct = dplyr::if_else(three_pa > 0, three_pm / three_pa, NA_real_),
      efg_pct = dplyr::if_else(
        fga > 0,
        (fgm + 0.5 * three_pm) / fga,
        NA_real_
      ),
      
      avg_aq_score = dplyr::if_else(
        non_grenade_fga > 0,
        aq_score_sum / non_grenade_fga,
        NA_real_
      ),
      
      expected_points_per_attempt = dplyr::if_else(
        fga > 0,
        expected_points / fga,
        NA_real_
      ),
      
      points_per_attempt = dplyr::if_else(
        fga > 0,
        pts_from_shots / fga,
        NA_real_
      ),
      
      shot_making_per_attempt = dplyr::if_else(
        fga > 0,
        shot_making_value / fga,
        NA_real_
      ),
      
      avg_possession_quality = dplyr::if_else(
        pq_events > 0,
        possession_quality_value / pq_events,
        NA_real_
      ),
      
      pq_per_game = dplyr::if_else(
        games > 0,
        possession_quality_value / games,
        NA_real_
      ),
      
      aq_plus_pq_value = shot_making_value + possession_quality_value,
      
      aq_plus_pq_per_game = dplyr::if_else(
        games > 0,
        aq_plus_pq_value / games,
        NA_real_
      ),
      
      assist_to_turnover = dplyr::if_else(
        turnovers > 0,
        assists / turnovers,
        NA_real_
      ),
      
      turnover_rate_pq_events = dplyr::if_else(
        pq_events > 0,
        turnovers / pq_events,
        NA_real_
      ),
      
      rim_rate = dplyr::if_else(fga > 0, rim_fga / fga, NA_real_),
      paint_non_ra_rate = dplyr::if_else(fga > 0, paint_non_ra_fga / fga, NA_real_),
      midrange_rate = dplyr::if_else(fga > 0, midrange_fga / fga, NA_real_),
      corner_three_rate = dplyr::if_else(fga > 0, corner_three_fga / fga, NA_real_),
      above_break_three_rate = dplyr::if_else(fga > 0, above_break_three_fga / fga, NA_real_),
      three_rate = dplyr::if_else(fga > 0, three_pa / fga, NA_real_),
      grenade_rate = dplyr::if_else(fga > 0, grenade_fga / fga, NA_real_),
      
      bucket_9_rate = dplyr::if_else(fga > 0, bucket_9_fga / fga, NA_real_),
      bucket_7_rate = dplyr::if_else(fga > 0, bucket_7_fga / fga, NA_real_),
      bucket_5_rate = dplyr::if_else(fga > 0, bucket_5_fga / fga, NA_real_),
      bucket_3_rate = dplyr::if_else(fga > 0, bucket_3_fga / fga, NA_real_),
      bucket_1_rate = dplyr::if_else(fga > 0, bucket_1_fga / fga, NA_real_)
    ) |>
    dplyr::select(-aq_score_sum)
  
  dplyr::bind_rows(
    single_team_players,
    traded_player_totals
  )
}

build_leaderboards <- function(
    combined_player_summary,
    season_type = "Regular Season"
) {
  min_fga <- if (season_type == "Regular Season") 300 else 50
  min_games <- if (season_type == "Regular Season") 20 else 8
  min_pq_events <- if (season_type == "Regular Season") 500 else 75
  
  qualified_shooting <- combined_player_summary |>
    dplyr::filter(.data$fga >= min_fga)
  
  qualified_pq <- combined_player_summary |>
    dplyr::filter(.data$pq_events >= min_pq_events)
  
  qualified_games <- combined_player_summary |>
    dplyr::filter(.data$games >= min_games)
  
  list(
    metadata = tibble::tibble(
      season_type = season_type,
      min_fga = min_fga,
      min_games = min_games,
      min_pq_events = min_pq_events
    ),
    
    avg_attempt_quality = qualified_shooting |>
      dplyr::arrange(dplyr::desc(.data$avg_aq_score)) |>
      dplyr::select(
        player_name,
        team_tricode,
        games,
        fga,
        avg_aq_score,
        expected_points_per_attempt,
        rim_rate,
        three_rate,
        grenade_rate
      ),
    
    expected_points_per_attempt = qualified_shooting |>
      dplyr::arrange(dplyr::desc(.data$expected_points_per_attempt)) |>
      dplyr::select(
        player_name,
        team_tricode,
        games,
        fga,
        expected_points_per_attempt,
        avg_aq_score,
        rim_rate,
        three_rate,
        corner_three_rate,
        above_break_three_rate
      ),
    
    shot_making_total = qualified_shooting |>
      dplyr::arrange(dplyr::desc(.data$shot_making_value)) |>
      dplyr::select(
        player_name,
        team_tricode,
        games,
        fga,
        shot_making_value,
        shot_making_per_attempt,
        points_per_attempt,
        expected_points_per_attempt
      ),
    
    shot_making_per_attempt = qualified_shooting |>
      dplyr::arrange(dplyr::desc(.data$shot_making_per_attempt)) |>
      dplyr::select(
        player_name,
        team_tricode,
        games,
        fga,
        shot_making_per_attempt,
        shot_making_value,
        points_per_attempt,
        expected_points_per_attempt
      ),
    
    possession_quality_total = qualified_pq |>
      dplyr::arrange(dplyr::desc(.data$possession_quality_value)) |>
      dplyr::select(
        player_name,
        team_tricode,
        games,
        pq_events,
        possession_quality_value,
        avg_possession_quality,
        pq_per_game,
        assists,
        turnovers,
        steals,
        blocks
      ),
    
    possession_quality_per_event = qualified_pq |>
      dplyr::arrange(dplyr::desc(.data$avg_possession_quality)) |>
      dplyr::select(
        player_name,
        team_tricode,
        games,
        pq_events,
        avg_possession_quality,
        possession_quality_value,
        assists,
        turnovers,
        offensive_rebounds,
        defensive_rebounds,
        steals,
        blocks
      ),
    
    combined_value_total = qualified_games |>
      dplyr::arrange(dplyr::desc(.data$aq_plus_pq_value)) |>
      dplyr::select(
        player_name,
        team_tricode,
        games,
        fga,
        avg_aq_score,
        shot_making_value,
        possession_quality_value,
        aq_plus_pq_value,
        aq_plus_pq_per_game,
        assists,
        turnovers,
        steals,
        blocks
      ),
    
    combined_value_per_game = qualified_games |>
      dplyr::arrange(dplyr::desc(.data$aq_plus_pq_per_game)) |>
      dplyr::select(
        player_name,
        team_tricode,
        games,
        fga,
        avg_aq_score,
        shot_making_value,
        possession_quality_value,
        aq_plus_pq_value,
        aq_plus_pq_per_game,
        assists,
        turnovers,
        steals,
        blocks
      ),
    
    rim_rate = qualified_shooting |>
      dplyr::arrange(dplyr::desc(.data$rim_rate)) |>
      dplyr::select(
        player_name,
        team_tricode,
        games,
        fga,
        avg_aq_score,
        rim_rate,
        paint_non_ra_rate,
        three_rate
      ),
    
    three_rate = qualified_shooting |>
      dplyr::arrange(dplyr::desc(.data$three_rate)) |>
      dplyr::select(
        player_name,
        team_tricode,
        games,
        fga,
        avg_aq_score,
        three_rate,
        corner_three_rate,
        above_break_three_rate
      ),
    
    assist_creation = qualified_pq |>
      dplyr::arrange(dplyr::desc(.data$assists)) |>
      dplyr::select(
        player_name,
        team_tricode,
        games,
        pq_events,
        assists,
        turnovers,
        assist_to_turnover,
        possession_quality_value
      )
  )
}


# Player detail objects --------------------------------------------------------

build_player_detail_table <- function(combined_player_summary) {
  combined_player_summary |>
    dplyr::select(
      season,
      season_type,
      player_id,
      player_name,
      team_id,
      team_tricode,
      games,
      fga,
      fgm,
      fg_pct,
      efg_pct,
      three_pa,
      three_pm,
      three_pct,
      pts_from_shots,
      avg_aq_score,
      expected_points,
      expected_points_per_attempt,
      points_per_attempt,
      shot_making_value,
      shot_making_per_attempt,
      possession_quality_value,
      avg_possession_quality,
      pq_per_game,
      pq_events,
      aq_plus_pq_value,
      aq_plus_pq_per_game,
      assists,
      turnovers,
      assist_to_turnover,
      offensive_rebounds,
      defensive_rebounds,
      unknown_rebounds,
      steals,
      blocks,
      offensive_fouls,
      made_free_throws,
      missed_free_throws,
      rim_rate,
      paint_non_ra_rate,
      midrange_rate,
      corner_three_rate,
      above_break_three_rate,
      three_rate,
      grenade_rate,
      bucket_9_rate,
      bucket_7_rate,
      bucket_5_rate,
      bucket_3_rate,
      bucket_1_rate
    )
}


build_game_log_table <- function(combined_player_game_summary) {
  combined_player_game_summary |>
    dplyr::select(
      season,
      season_type,
      game_id,
      game_date,
      htm,
      vtm,
      team_id,
      team_tricode,
      player_id,
      player_name,
      fga,
      fgm,
      fg_pct,
      efg_pct,
      three_pa,
      three_pm,
      three_pct,
      pts_from_shots,
      avg_aq_score,
      expected_points,
      expected_points_per_attempt,
      points_per_attempt,
      shot_making_value,
      shot_making_per_attempt,
      possession_quality_value,
      avg_possession_quality,
      pq_events,
      aq_plus_pq_value,
      possession_quality_per_fga,
      assists,
      turnovers,
      assists_per_turnover,
      offensive_rebounds,
      defensive_rebounds,
      unknown_rebounds,
      steals,
      blocks,
      offensive_fouls,
      made_free_throws,
      missed_free_throws,
      rim_rate,
      paint_non_ra_rate,
      midrange_rate,
      corner_three_rate,
      above_break_three_rate,
      three_rate,
      grenade_rate
    ) |>
    dplyr::arrange(dplyr::desc(.data$game_date), .data$player_name)
}


# Main builder -----------------------------------------------------------------

build_app_cache <- function(
    season = "2024-25",
    season_type = "Regular Season",
    force_rebuild = TRUE
) {
  cache_out <- build_cache_path(season, season_type)
  
  if (!force_rebuild && file.exists(cache_out)) {
    message("Already built app cache for ", season, " / ", season_type)
    return(readr::read_rds(cache_out))
  }
  
  combined_player_summary_path <- build_input_path(
    PROCESSED_COMBINED_DIR,
    "combined_player_summary",
    season,
    season_type
  )
  
  combined_player_game_path <- build_input_path(
    PROCESSED_COMBINED_DIR,
    "combined_player_game_summary",
    season,
    season_type
  )
  
  scored_shots_path <- build_input_path(
    PROCESSED_MODEL_DIR,
    "scored_shots",
    season,
    season_type
  )
  
  expected_points_path <- build_input_path(
    PROCESSED_MODEL_DIR,
    "expected_points_by_bucket",
    season,
    season_type
  )
  
  required_files <- c(
    combined_player_summary_path,
    combined_player_game_path,
    scored_shots_path,
    expected_points_path
  )
  
  missing_files <- required_files[!file.exists(required_files)]
  
  if (length(missing_files) > 0) {
    stop(
      "Missing required app cache input files:\n",
      paste(missing_files, collapse = "\n")
    )
  }
  
  message("\nBuilding app cache")
  message("- Season: ", season)
  message("- Season type: ", season_type)
  
  combined_player_summary <- readr::read_rds(combined_player_summary_path)
  leaderboard_player_summary <- build_player_totals(combined_player_summary)
  combined_player_game_summary <- readr::read_rds(combined_player_game_path)
  scored_shots <- readr::read_rds(scored_shots_path)
  expected_points_by_bucket <- readr::read_rds(expected_points_path)
  
  player_lookup <- build_player_lookup(combined_player_summary)
  leaderboards <- build_leaderboards(leaderboard_player_summary, season_type)
  player_detail <- build_player_detail_table(combined_player_summary)
  player_game_log <- build_game_log_table(combined_player_game_summary)
  
  cache_metadata <- tibble::tibble(
    season = season,
    season_type = season_type,
    data_year = season_to_data_year(season),
    players = dplyr::n_distinct(combined_player_summary$player_id),
    player_team_rows = nrow(combined_player_summary),
    games = dplyr::n_distinct(combined_player_game_summary$game_id),
    total_fga = sum(combined_player_summary$fga, na.rm = TRUE),
    total_pq_events = sum(combined_player_summary$pq_events, na.rm = TRUE),
    total_shot_making_value = sum(combined_player_summary$shot_making_value, na.rm = TRUE),
    total_possession_quality_value = sum(combined_player_summary$possession_quality_value, na.rm = TRUE),
    built_at = Sys.time()
  )
  
  app_cache <- list(
    metadata = cache_metadata,
    player_lookup = player_lookup,
    player_detail = player_detail,
    player_game_log = player_game_log,
    combined_player_summary = combined_player_summary,
    combined_player_game_summary = combined_player_game_summary,
    scored_shots = scored_shots,
    expected_points_by_bucket = expected_points_by_bucket,
    leaderboards = leaderboards
  )
  
  message("\nApp cache metadata:")
  print(cache_metadata)
  
  message("\nCache object names:")
  print(names(app_cache))
  
  message("\nLeaderboard names:")
  print(names(leaderboards))
  
  readr::write_rds(app_cache, cache_out)
  
  message("\nSaved app cache:")
  message("- ", cache_out)
  
  invisible(app_cache)
}