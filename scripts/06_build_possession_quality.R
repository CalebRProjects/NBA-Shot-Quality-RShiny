# scripts/06_build_possession_quality.R

source("R/08_build_possession_quality.R")

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
    build_possession_quality_outputs(
      season = season,
      season_type = season_type,
      write_csv = TRUE,
      force_rebuild = TRUE
    )
  }
)