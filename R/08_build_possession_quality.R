# R/08_build_possession_quality.R
# Builds player-game and player-level Possession Quality outputs from local nbastatsv3 data.
#
# PQ v1 is a public-data proxy model:
# - player attribution comes from nbastatsv3 play-by-play rows
# - assists are parsed from made-shot descriptions
# - steals/blocks come from actionType = NA stat-credit rows
# - rebounds are classified by changes in player cumulative Off/Def rebound counts

required_pkgs <- c("tidyverse", "readr", "stringr", "glue", "janitor", "stringi")
invisible(lapply(required_pkgs, require, character.only = TRUE))


# Paths ------------------------------------------------------------------------

RAW_COMPRESSED_DIR <- "data/raw_local/nba_data/compressed"
RAW_EXTRACTED_DIR <- "data/raw_local/nba_data/extracted"
PROCESSED_MODEL_DIR <- "data/processed/model_outputs"

dir.create(PROCESSED_MODEL_DIR, recursive = TRUE, showWarnings = FALSE)


# Helpers ----------------------------------------------------------------------

normalize_game_id <- function(game_id) {
  game_id_chr <- as.character(game_id)
  
  dplyr::case_when(
    stringr::str_detect(game_id_chr, "^00") ~ game_id_chr,
    stringr::str_length(game_id_chr) == 8 ~ paste0("00", game_id_chr),
    TRUE ~ game_id_chr
  )
}

season_to_data_year <- function(season) {
  as.integer(stringr::str_sub(season, 1, 4))
}

season_type_suffix <- function(season_type) {
  if (season_type == "Regular Season") {
    ""
  } else if (season_type == "Playoffs") {
    "_po"
  } else {
    stop("Unsupported season_type: ", season_type)
  }
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

build_source_file_name <- function(data_type, season, season_type) {
  data_year <- season_to_data_year(season)
  suffix <- season_type_suffix(season_type)
  
  paste0(data_type, suffix, "_", data_year, ".tar.xz")
}

build_csv_file_name <- function(data_type, season, season_type) {
  data_year <- season_to_data_year(season)
  suffix <- season_type_suffix(season_type)
  
  paste0(data_type, suffix, "_", data_year, ".csv")
}

build_pq_output_path <- function(prefix, season, season_type) {
  data_year <- season_to_data_year(season)
  type_label <- season_type_file_label(season_type)
  
  file.path(
    PROCESSED_MODEL_DIR,
    paste0(prefix, "_", data_year, "_", type_label, ".rds")
  )
}

extract_if_needed <- function(tar_file, csv_file, exdir = RAW_EXTRACTED_DIR) {
  if (file.exists(csv_file)) {
    return(invisible(csv_file))
  }
  
  if (!file.exists(tar_file)) {
    stop("Missing compressed source file: ", tar_file)
  }
  
  message("Extracting: ", tar_file)
  utils::untar(tar_file, exdir = exdir)
  
  if (!file.exists(csv_file)) {
    stop("Expected extracted csv not found: ", csv_file)
  }
  
  invisible(csv_file)
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

normalize_name_key <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("\\.", "") |>
    stringr::str_replace_all("[^a-z0-9\\s-]", "") |>
    stringr::str_squish()
}

last_token_key <- function(x) {
  normalize_name_key(x) |>
    stringr::word(-1)
}


# Base event scoring -----------------------------------------------------------

add_pbp_shot_value <- function(data) {
  if ("shot_value" %in% names(data)) {
    data |>
      dplyr::mutate(
        pbp_shot_value = suppressWarnings(as.numeric(.data$shot_value))
      )
  } else {
    data |>
      dplyr::mutate(
        pbp_shot_value = dplyr::case_when(
          stringr::str_detect(
            stringr::str_to_lower(dplyr::coalesce(.data$description, "")),
            "3pt"
          ) ~ 3,
          TRUE ~ 2
        )
      )
  }
}

prepare_pbp <- function(pbp) {
  pbp |>
    janitor::clean_names() |>
    dplyr::mutate(
      game_id = normalize_game_id(.data$game_id),
      player_id = as.character(.data$person_id),
      team_id = as.character(.data$team_id),
      desc_lower = stringr::str_to_lower(dplyr::coalesce(.data$description, "")),
      action_type_lower = stringr::str_to_lower(dplyr::coalesce(.data$action_type, "")),
      sub_type_lower = stringr::str_to_lower(dplyr::coalesce(.data$sub_type, "")),
      is_valid_player_event =
        !is.na(.data$player_id) &
        .data$player_id != "0" &
        !is.na(.data$player_name) &
        !is.na(.data$team_tricode) &
        .data$player_id != .data$team_id &
        !stringr::str_detect(.data$player_id, "^161061")
    ) |>
    add_pbp_shot_value()
}

add_rebound_type <- function(pbp_clean) {
  rebound_counts <- pbp_clean |>
    dplyr::filter(.data$action_type == "Rebound") |>
    dplyr::mutate(
      off_count = safe_numeric(
        stringr::str_match(.data$description, "Off:([0-9]+)")[, 2]
      ),
      def_count = safe_numeric(
        stringr::str_match(.data$description, "Def:([0-9]+)")[, 2]
      )
    ) |>
    dplyr::arrange(.data$game_id, .data$player_id, .data$action_number) |>
    dplyr::group_by(.data$game_id, .data$player_id) |>
    dplyr::mutate(
      prev_off_count = dplyr::lag(.data$off_count, default = 0),
      prev_def_count = dplyr::lag(.data$def_count, default = 0),
      rebound_type = dplyr::case_when(
        !is.na(.data$off_count) &
          .data$off_count > .data$prev_off_count ~ "offensive_rebound",
        !is.na(.data$def_count) &
          .data$def_count > .data$prev_def_count ~ "defensive_rebound",
        TRUE ~ "unknown_rebound"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      .data$game_id,
      .data$action_number,
      .data$player_id,
      .data$rebound_type
    )
  
  pbp_clean |>
    dplyr::left_join(
      rebound_counts,
      by = c("game_id", "action_number", "player_id")
    )
}

classify_pq_events <- function(pbp) {
  pbp_clean <- pbp |>
    prepare_pbp() |>
    add_rebound_type()
  
  pbp_clean |>
    dplyr::mutate(
      is_made_shot = .data$action_type == "Made Shot",
      is_missed_shot = .data$action_type == "Missed Shot",
      is_turnover = .data$action_type == "Turnover",
      is_foul = .data$action_type == "Foul",
      is_free_throw = .data$action_type == "Free Throw",
      
      is_steal = is.na(.data$action_type) &
        stringr::str_detect(.data$desc_lower, "\\bsteal\\b"),
      
      is_block = is.na(.data$action_type) &
        stringr::str_detect(.data$desc_lower, "\\bblock\\b"),
      
      is_offensive_foul = .data$is_foul &
        stringr::str_detect(.data$sub_type_lower, "offensive|charge"),
      
      is_missed_free_throw = .data$is_free_throw &
        stringr::str_detect(.data$desc_lower, "^miss"),
      
      is_made_free_throw = .data$is_free_throw &
        !.data$is_missed_free_throw,
      
      pq_event_type = dplyr::case_when(
        .data$is_made_shot ~ "made_shot",
        .data$is_missed_shot ~ "missed_shot",
        .data$is_turnover ~ "turnover",
        .data$rebound_type == "offensive_rebound" ~ "offensive_rebound",
        .data$rebound_type == "defensive_rebound" ~ "defensive_rebound",
        .data$rebound_type == "unknown_rebound" ~ "unknown_rebound",
        .data$is_steal ~ "steal",
        .data$is_block ~ "block",
        .data$is_offensive_foul ~ "offensive_foul",
        .data$is_made_free_throw ~ "made_free_throw",
        .data$is_missed_free_throw ~ "missed_free_throw",
        TRUE ~ NA_character_
      ),
      
      pq_event_value = dplyr::case_when(
        .data$pq_event_type == "made_shot" ~ safe_numeric(.data$pbp_shot_value),
        .data$pq_event_type == "missed_shot" ~ 0,
        .data$pq_event_type == "turnover" ~ -1.0,
        .data$pq_event_type == "offensive_rebound" ~ 0.7,
        .data$pq_event_type == "defensive_rebound" ~ 0.25,
        .data$pq_event_type == "unknown_rebound" ~ 0.35,
        .data$pq_event_type == "steal" ~ 1.3,
        .data$pq_event_type == "block" ~ 0.8,
        .data$pq_event_type == "offensive_foul" ~ -1.2,
        .data$pq_event_type == "made_free_throw" ~ 1.0,
        .data$pq_event_type == "missed_free_throw" ~ -0.25,
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::filter(
      .data$is_valid_player_event,
      !is.na(.data$pq_event_type)
    ) |>
    dplyr::transmute(
      game_id,
      game_date = as.Date(NA),
      period = as.integer(.data$period),
      clock,
      team_id,
      team_tricode,
      player_id,
      player_name,
      action_number = .data$action_number,
      action_type,
      sub_type,
      description,
      pq_event_type,
      pq_event_value
    )
}


# Assist parsing ---------------------------------------------------------------

build_assist_events <- function(pbp) {
  pbp_clean <- pbp |>
    prepare_pbp()
  
  player_lookup <- pbp_clean |>
    dplyr::filter(
      !is.na(.data$player_id),
      .data$player_id != "0",
      !is.na(.data$player_name),
      !is.na(.data$team_tricode)
    ) |>
    dplyr::distinct(
      .data$game_id,
      .data$team_id,
      .data$team_tricode,
      .data$player_id,
      .data$player_name
    ) |>
    dplyr::mutate(
      full_name_key = normalize_name_key(.data$player_name),
      last_name_key = last_token_key(.data$player_name)
    )
  
  # Alias lookup from the actual feed text.
  # This catches cases like Jal. Williams, Jay. Williams, K. Williams, L. James, etc.
  player_alias_lookup <- pbp_clean |>
    dplyr::filter(
      !is.na(.data$player_id),
      .data$player_id != "0",
      !is.na(.data$player_name),
      !is.na(.data$team_tricode),
      !is.na(.data$description)
    ) |>
    dplyr::mutate(
      description_clean = stringr::str_remove(.data$description, "^MISS\\s+"),
      primary_name_raw = stringr::str_match(
        .data$description_clean,
        "^(.+?)(?:\\s+\\d+'|\\s+3PT|\\s+Dunk|\\s+Layup|\\s+Jump|\\s+Pullup|\\s+Running|\\s+Driving|\\s+Cutting|\\s+Floating|\\s+Turnaround|\\s+Fadeaway|\\s+Hook|\\s+Alley|\\s+Putback|\\s+Tip|\\s+Free Throw|\\s+BLOCK|\\s+STEAL|\\s+REBOUND)"
      )[, 2],
      alias_full_key = normalize_name_key(.data$primary_name_raw)
    ) |>
    dplyr::filter(
      !is.na(.data$alias_full_key),
      .data$alias_full_key != ""
    ) |>
    dplyr::distinct(
      .data$game_id,
      .data$team_id,
      .data$team_tricode,
      .data$alias_full_key,
      .data$player_id,
      .data$player_name
    )
  
  assist_raw <- pbp_clean |>
    dplyr::filter(
      .data$action_type == "Made Shot",
      stringr::str_detect(.data$description, "AST")
    ) |>
    dplyr::mutate(
      assist_text = stringr::str_match(
        .data$description,
        "\\(([^\\)]*AST)\\)"
      )[, 2],
      assister_name_raw = stringr::str_remove(
        .data$assist_text,
        "\\s+\\d+\\s+AST"
      ),
      assister_name_raw = stringr::str_squish(.data$assister_name_raw),
      assister_full_key = normalize_name_key(.data$assister_name_raw),
      assister_last_key = last_token_key(.data$assister_name_raw)
    ) |>
    dplyr::filter(!is.na(.data$assister_full_key))
  
  assist_raw_count <- nrow(assist_raw)
  
  alias_matches <- assist_raw |>
    dplyr::left_join(
      player_alias_lookup,
      by = c(
        "game_id",
        "team_id",
        "team_tricode",
        "assister_full_key" = "alias_full_key"
      ),
      relationship = "many-to-many",
      suffix = c("_shot", "_assist")
    ) |>
    dplyr::filter(!is.na(.data$player_id_assist))
  
  alias_match_count <- alias_matches |>
    dplyr::distinct(.data$game_id, .data$action_number) |>
    nrow()
  
  unmatched_alias <- assist_raw |>
    dplyr::anti_join(
      alias_matches |>
        dplyr::distinct(.data$game_id, .data$action_number),
      by = c("game_id", "action_number")
    )
  
  exact_matches <- unmatched_alias |>
    dplyr::left_join(
      player_lookup,
      by = c(
        "game_id",
        "team_id",
        "team_tricode",
        "assister_full_key" = "full_name_key"
      ),
      relationship = "many-to-many",
      suffix = c("_shot", "_assist")
    ) |>
    dplyr::filter(!is.na(.data$player_id_assist))
  
  exact_match_count <- exact_matches |>
    dplyr::distinct(.data$game_id, .data$action_number) |>
    nrow()
  
  unmatched_exact <- unmatched_alias |>
    dplyr::anti_join(
      exact_matches |>
        dplyr::distinct(.data$game_id, .data$action_number),
      by = c("game_id", "action_number")
    )
  
  last_name_matches <- unmatched_exact |>
    dplyr::left_join(
      player_lookup,
      by = c(
        "game_id",
        "team_id",
        "team_tricode",
        "assister_last_key" = "last_name_key"
      ),
      relationship = "many-to-many",
      suffix = c("_shot", "_assist")
    ) |>
    dplyr::filter(!is.na(.data$player_id_assist))
  
  last_name_match_count <- last_name_matches |>
    dplyr::distinct(.data$game_id, .data$action_number) |>
    nrow()
  
  assist_events <- dplyr::bind_rows(
    alias_matches,
    exact_matches,
    last_name_matches
  ) |>
    dplyr::group_by(
      .data$game_id,
      .data$action_number,
      .data$team_id,
      .data$team_tricode,
      .data$assister_name_raw
    ) |>
    dplyr::filter(dplyr::n() == 1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      game_id,
      game_date = as.Date(NA),
      period = as.integer(.data$period),
      clock,
      team_id,
      team_tricode,
      player_id = .data$player_id_assist,
      player_name = .data$player_name_assist,
      action_number = .data$action_number,
      action_type = "Assist",
      sub_type = NA_character_,
      description = paste0(
        .data$player_name_assist,
        " assist on made shot: ",
        .data$description
      ),
      pq_event_type = "assist",
      pq_event_value = 0.7
    )
  
  message(
    "Assist parser QA: raw=", assist_raw_count,
    " | alias=", alias_match_count,
    " | exact=", exact_match_count,
    " | last-name=", last_name_match_count,
    " | final=", nrow(assist_events)
  )
  
  assist_events
}


# Summaries --------------------------------------------------------------------

build_player_game_pq <- function(possession_events) {
  possession_events |>
    dplyr::group_by(
      .data$game_id,
      .data$team_id,
      .data$team_tricode,
      .data$player_id,
      .data$player_name
    ) |>
    dplyr::summarise(
      pq_events = dplyr::n(),
      possession_quality_value = sum(.data$pq_event_value, na.rm = TRUE),
      
      made_shots = sum(.data$pq_event_type == "made_shot", na.rm = TRUE),
      missed_shots = sum(.data$pq_event_type == "missed_shot", na.rm = TRUE),
      assists = sum(.data$pq_event_type == "assist", na.rm = TRUE),
      turnovers = sum(.data$pq_event_type == "turnover", na.rm = TRUE),
      offensive_rebounds = sum(.data$pq_event_type == "offensive_rebound", na.rm = TRUE),
      defensive_rebounds = sum(.data$pq_event_type == "defensive_rebound", na.rm = TRUE),
      unknown_rebounds = sum(.data$pq_event_type == "unknown_rebound", na.rm = TRUE),
      steals = sum(.data$pq_event_type == "steal", na.rm = TRUE),
      blocks = sum(.data$pq_event_type == "block", na.rm = TRUE),
      offensive_fouls = sum(.data$pq_event_type == "offensive_foul", na.rm = TRUE),
      made_free_throws = sum(.data$pq_event_type == "made_free_throw", na.rm = TRUE),
      missed_free_throws = sum(.data$pq_event_type == "missed_free_throw", na.rm = TRUE),
      
      .groups = "drop"
    ) |>
    dplyr::mutate(
      avg_possession_quality = dplyr::if_else(
        .data$pq_events > 0,
        .data$possession_quality_value / .data$pq_events,
        NA_real_
      )
    ) |>
    dplyr::arrange(.data$game_id, .data$team_tricode, .data$player_name)
}


build_player_pq_summary <- function(player_game_pq) {
  player_game_pq |>
    dplyr::group_by(
      .data$player_id,
      .data$player_name,
      .data$team_id,
      .data$team_tricode
    ) |>
    dplyr::summarise(
      games = dplyr::n_distinct(.data$game_id),
      pq_events = sum(.data$pq_events, na.rm = TRUE),
      possession_quality_value = sum(.data$possession_quality_value, na.rm = TRUE),
      
      made_shots = sum(.data$made_shots, na.rm = TRUE),
      missed_shots = sum(.data$missed_shots, na.rm = TRUE),
      assists = sum(.data$assists, na.rm = TRUE),
      turnovers = sum(.data$turnovers, na.rm = TRUE),
      offensive_rebounds = sum(.data$offensive_rebounds, na.rm = TRUE),
      defensive_rebounds = sum(.data$defensive_rebounds, na.rm = TRUE),
      unknown_rebounds = sum(.data$unknown_rebounds, na.rm = TRUE),
      steals = sum(.data$steals, na.rm = TRUE),
      blocks = sum(.data$blocks, na.rm = TRUE),
      offensive_fouls = sum(.data$offensive_fouls, na.rm = TRUE),
      made_free_throws = sum(.data$made_free_throws, na.rm = TRUE),
      missed_free_throws = sum(.data$missed_free_throws, na.rm = TRUE),
      
      .groups = "drop"
    ) |>
    dplyr::mutate(
      avg_possession_quality = dplyr::if_else(
        .data$pq_events > 0,
        .data$possession_quality_value / .data$pq_events,
        NA_real_
      ),
      pq_per_game = dplyr::if_else(
        .data$games > 0,
        .data$possession_quality_value / .data$games,
        NA_real_
      ),
      turnover_rate_pq_events = dplyr::if_else(
        .data$pq_events > 0,
        .data$turnovers / .data$pq_events,
        NA_real_
      ),
      assist_to_turnover = dplyr::if_else(
        .data$turnovers > 0,
        .data$assists / .data$turnovers,
        NA_real_
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$possession_quality_value))
}


# Main builder -----------------------------------------------------------------

build_possession_quality_outputs <- function(
    season = "2024-25",
    season_type = "Regular Season",
    write_csv = TRUE,
    force_rebuild = TRUE
) {
  pbp_tar <- file.path(
    RAW_COMPRESSED_DIR,
    build_source_file_name("nbastatsv3", season, season_type)
  )
  
  pbp_csv <- file.path(
    RAW_EXTRACTED_DIR,
    build_csv_file_name("nbastatsv3", season, season_type)
  )
  
  extract_if_needed(pbp_tar, pbp_csv)
  
  possession_events_out <- build_pq_output_path("possession_events", season, season_type)
  player_game_pq_out <- build_pq_output_path("player_game_pq", season, season_type)
  player_pq_summary_out <- build_pq_output_path("player_pq_summary", season, season_type)
  
  if (
    !force_rebuild &&
    file.exists(possession_events_out) &&
    file.exists(player_game_pq_out) &&
    file.exists(player_pq_summary_out)
  ) {
    message("Already built PQ outputs for ", season, " / ", season_type)
    
    return(
      list(
        possession_events = readr::read_rds(possession_events_out),
        player_game_pq = readr::read_rds(player_game_pq_out),
        player_pq_summary = readr::read_rds(player_pq_summary_out)
      )
    )
  }
  
  message("\nBuilding Possession Quality outputs")
  message("- Season: ", season)
  message("- Season type: ", season_type)
  
  pbp <- readr::read_csv(pbp_csv, show_col_types = FALSE)
  
  base_events <- classify_pq_events(pbp)
  assist_events <- build_assist_events(pbp)
  
  possession_events <- dplyr::bind_rows(
    base_events,
    assist_events
  ) |>
    dplyr::mutate(
      season = season,
      season_type = season_type,
      .before = 1
    )
  
  player_game_pq <- build_player_game_pq(possession_events) |>
    dplyr::mutate(
      season = season,
      season_type = season_type,
      .before = 1
    )
  
  player_pq_summary <- build_player_pq_summary(player_game_pq) |>
    dplyr::mutate(
      season = season,
      season_type = season_type,
      .before = 1
    )
  
  qa_summary <- list(
    possession_events = possession_events |>
      dplyr::count(.data$pq_event_type, sort = TRUE),
    
    player_game_pq = player_game_pq |>
      dplyr::summarise(
        rows = dplyr::n(),
        games = dplyr::n_distinct(.data$game_id),
        players = dplyr::n_distinct(.data$player_id),
        total_pq_events = sum(.data$pq_events, na.rm = TRUE),
        total_possession_quality_value = sum(.data$possession_quality_value, na.rm = TRUE)
      ),
    
    player_pq_summary = player_pq_summary |>
      dplyr::summarise(
        rows = dplyr::n(),
        players = dplyr::n_distinct(.data$player_id),
        total_pq_events = sum(.data$pq_events, na.rm = TRUE),
        total_possession_quality_value = sum(.data$possession_quality_value, na.rm = TRUE),
        avg_possession_quality_weighted = sum(
          .data$avg_possession_quality * .data$pq_events,
          na.rm = TRUE
        ) / sum(.data$pq_events, na.rm = TRUE)
      ),
    
    top_assists = player_pq_summary |>
      dplyr::arrange(dplyr::desc(.data$assists)) |>
      dplyr::select(
        .data$player_name,
        .data$team_tricode,
        .data$games,
        .data$assists,
        .data$turnovers,
        .data$assist_to_turnover,
        .data$possession_quality_value
      ) |>
      dplyr::slice_head(n = 20)
  )
  
  message("\nPQ event counts:")
  print(qa_summary$possession_events, n = 30)
  
  message("\nPlayer-game PQ QA:")
  print(qa_summary$player_game_pq)
  
  message("\nPlayer PQ summary QA:")
  print(qa_summary$player_pq_summary)
  
  message("\nTop assist players:")
  print(qa_summary$top_assists, n = 20)
  
  message("\nTop PQ value players:")
  print(
    player_pq_summary |>
      dplyr::select(
        .data$player_name,
        .data$team_tricode,
        .data$games,
        .data$pq_events,
        .data$possession_quality_value,
        .data$avg_possession_quality,
        .data$pq_per_game,
        .data$assists,
        .data$turnovers,
        .data$offensive_rebounds,
        .data$defensive_rebounds,
        .data$steals,
        .data$blocks
      ) |>
      dplyr::slice_head(n = 20),
    n = 20
  )
  
  readr::write_rds(possession_events, possession_events_out)
  readr::write_rds(player_game_pq, player_game_pq_out)
  readr::write_rds(player_pq_summary, player_pq_summary_out)
  
  if (write_csv) {
    readr::write_csv(
      possession_events,
      stringr::str_replace(possession_events_out, "\\.rds$", ".csv")
    )
    
    readr::write_csv(
      player_game_pq,
      stringr::str_replace(player_game_pq_out, "\\.rds$", ".csv")
    )
    
    readr::write_csv(
      player_pq_summary,
      stringr::str_replace(player_pq_summary_out, "\\.rds$", ".csv")
    )
  }
  
  message("\nSaved Possession Quality outputs:")
  message("- ", possession_events_out)
  message("- ", player_game_pq_out)
  message("- ", player_pq_summary_out)
  
  invisible(
    list(
      possession_events = possession_events,
      player_game_pq = player_game_pq,
      player_pq_summary = player_pq_summary,
      qa_summary = qa_summary
    )
  )
}