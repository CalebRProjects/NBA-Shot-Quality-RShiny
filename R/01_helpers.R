# General helper functions -----------------------------------------------------

safe_dir_create <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

safe_read_rds <- function(path, default = tibble::tibble()) {
  if (!file.exists(path)) return(default)
  readRDS(path)
}

safe_write_rds <- function(x, path) {
  safe_dir_create(dirname(path))
  saveRDS(x, path)
  invisible(path)
}

clean_names_if_df <- function(x) {
  if (is.data.frame(x)) janitor::clean_names(x) else x
}

pluck_table <- function(x, table_name) {
  # hoopR NBA endpoints usually return named lists of data frames.
  # This helper keeps the pipeline from breaking if names are absent or changed.
  if (is.data.frame(x)) return(janitor::clean_names(x))

  if (is.list(x) && table_name %in% names(x)) {
    return(janitor::clean_names(x[[table_name]]))
  }

  if (is.list(x)) {
    first_df <- purrr::keep(x, is.data.frame)
    if (length(first_df) > 0) return(janitor::clean_names(first_df[[1]]))
  }

  tibble::tibble()
}

parse_clock_seconds <- function(clock) {
  # Handles clock strings such as "PT04M32.00S", "04:32", or already-clean values.
  clock_chr <- as.character(clock)

  mins <- stringr::str_match(clock_chr, "PT(\\d+)M")[, 2]
  secs <- stringr::str_match(clock_chr, "M([0-9.]+)S")[, 2]

  out <- suppressWarnings(as.numeric(mins) * 60 + as.numeric(secs))

  mmss <- stringr::str_match(clock_chr, "^(\\d{1,2}):(\\d{2})")
  out2 <- suppressWarnings(as.numeric(mmss[, 2]) * 60 + as.numeric(mmss[, 3]))

  dplyr::coalesce(out, out2)
}

cache_status <- function(cache) {
  metadata <- cache$metadata

  list(
    cache_built_at = metadata$built_at %||% NA_character_,
    season = metadata$season %||% NA_character_,
    season_type = metadata$season_type %||% NA_character_,
    players_processed = nrow(cache$player_summary),
    games_processed = dplyr::n_distinct(cache$game_summary$game_id),
    shot_events_processed = nrow(cache$shot_events),
    cache_dir = CACHE_DIR
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}
