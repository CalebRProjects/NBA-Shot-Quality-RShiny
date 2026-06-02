# Global configuration ---------------------------------------------------------
# Keep constants here so model assumptions are visible and easy to update.

APP_TITLE <- "NBA SQ + PQ"

CACHE_DIR <- file.path("data", "cache")

CACHE_FILES <- list(
  metadata = file.path(CACHE_DIR, "cache_metadata.rds"),
  player_lookup = file.path(CACHE_DIR, "player_lookup.rds"),
  game_logs = file.path(CACHE_DIR, "game_logs.rds"),
  shot_events = file.path(CACHE_DIR, "shot_events_scored.rds"),
  game_summary = file.path(CACHE_DIR, "game_summary.rds"),
  player_summary = file.path(CACHE_DIR, "player_summary.rds")
)

DEFAULT_SEASON <- "2024-25"
DEFAULT_SEASON_TYPE <- "Playoffs"

# Placeholder expected points by SQ bucket.
# Future version: replace with empirical league-average points per shot by bucket.
SQ_EXPECTED_POINTS <- c(
  "9" = 1.35,
  "7" = 1.15,
  "5" = 1.00,
  "3" = 0.80,
  "1" = 0.55,
  "Prayer" = 0.20
)

# Placeholder possession quality weights.
# Future version: calibrate from league-average expected possession value.
PQ_WEIGHTS <- list(
  ast = 1.00,
  potential_ast = 0.40,
  tov = -2.00,
  stl = 1.25,
  blk = 1.00,
  oreb = 1.10,
  pf = -0.40,
  fouls_drawn = 0.60,
  deflections = 0.35,
  charges_drawn = 1.25
)
