# scripts/04_build_attempt_quality_outputs.R

source("R/06_attempt_quality_rules.R")

build_plan <- tidyr::crossing(
  season = c(
    "2020-21",
    "2021-22",
    "2022-23",
    "2023-24",
    "2024-25"
  ),
  season_type = c("Regular Season", "Playoffs")
)

purrr::pwalk(
  build_plan,
  function(season, season_type) {
    build_attempt_quality_outputs(
      season = season,
      season_type = season_type,
      min_attempts = 500,
      write_csv = TRUE,
      force_rebuild = TRUE
    )
  }
)