# scripts/08_build_app_cache.R

source("R/10_build_app_cache.R")

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
    build_app_cache(
      season = season,
      season_type = season_type,
      force_rebuild = TRUE
    )
  }
)