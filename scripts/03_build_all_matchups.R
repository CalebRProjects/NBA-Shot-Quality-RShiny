# scripts/03_build_all_matchups.R

source("R/05_build_matchups.R")

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
    build_clean_matchups(
      season = season,
      season_type = season_type,
      write_csv = TRUE,
      force_rebuild = TRUE
    )
  }
)