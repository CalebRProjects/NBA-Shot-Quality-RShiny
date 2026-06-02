# NBA Shot Quality + Possession Quality Shiny App

This project is a cache-first R/Shiny app for exploring NBA player shot quality, shot-making, and broader possession quality using public NBA.com data through `hoopR`.

The current version is a working proof of concept. It is not intended to be a final player evaluation model yet. The app is designed to make the data pipeline, scoring rules, assumptions, and limitations visible while providing a stable foundation for future league-average calibration and richer possession-value modeling.

## Project Goals

* Pull NBA.com player game logs and play-by-play data through `hoopR`
* Cache data locally so the app does not rely on live NBA.com pulls every time it opens
* Score shot attempts using transparent public-data proxy rules
* Separate shot quality from shot-making
* Create game-level and player-level summaries
* Support player search and simple leaderboards
* Clearly label what is automated, proxied, estimated, and planned for future development

## Current Status

The first working version successfully builds a local cache for a single test player using:

* `hoopR::nba_playergamelogs()` for player game logs
* `hoopR::nba_playbyplayv3()` for game-level play-by-play
* Cached `.rds` files for app loading
* Starter public-data proxy rules for Shot Quality and Possession Quality

During testing, `hoopR::nba_playergamelog()` returned empty lists for tested player IDs, so the current backend uses `hoopR::nba_playergamelogs()` with `player_id` passed explicitly.

The current app includes:

* Home / overview page
* Methodology page
* Player search page
* Game-by-game shot quality chart
* Shot bucket distribution chart
* Player game log table
* Basic leaderboards
* Data / pipeline status page

## Folder Structure

* `app.R`
  Main Shiny app shell.

* `R/00_config.R`
  Global settings, cache paths, placeholder Shot Quality expected values, and placeholder Possession Quality weights.

* `R/01_helpers.R`
  Shared helper functions for reading and writing cache files, parsing game clocks, and handling `hoopR` list outputs.

* `R/02_player_lookup.R`
  Player lookup and Shiny player selector helpers.

* `R/03_data_pull_game_logs.R`
  Pulls player game logs using `hoopR::nba_playergamelogs()`.

* `R/04_data_pull_pbp.R`
  Pulls play-by-play using `hoopR::nba_playbyplayv3()` and filters shot events.

* `R/05_shot_quality_rules.R`
  Public-data proxy rules for assigning shot quality buckets.

* `R/06_possession_quality.R`
  Starter possession quality scoring using available box-score events and shot quality summaries.

* `R/07_cache_builder.R`
  Manual cache builder script.

* `R/08_player_summaries.R`
  Player-level and game-level summary helpers.

* `R/09_leaderboards.R`
  Leaderboard, methodology, and missing-field tables.

* `data/cache/`
  Local cache directory. Generated `.rds` files should not be committed unless intentionally publishing a small sample cache.

* `www/`
  Static assets for future CSS, images, or logo work.

## Setup

Install required packages:

```r
install.packages(c(
  "shiny", "tidyverse", "hoopR", "janitor", "lubridate", "stringr",
  "readr", "scales", "glue", "DT", "plotly"
))
```

Build a small test cache:

```r
source("R/07_cache_builder.R")

build_sq_cache(
  player_ids = c("1629029"),
  season = "2024-25",
  season_type = "Playoffs",
  force_refresh = TRUE
)
```

Run the app:

```r
shiny::runApp()
```

## Cache Strategy

The app is designed to load cached data by default instead of pulling from NBA.com every time it opens.

Generated cache files are stored in:

```text
data/cache/
```

The cache currently includes:

* `cache_metadata.rds`
* `player_lookup.rds`
* `game_logs.rds`
* `shot_events_scored.rds`
* `game_summary.rds`
* `player_summary.rds`

Recommended `.gitignore` entries:

```gitignore
data/cache/*.rds
.Rhistory
.RData
.Rproj.user/
.DS_Store
```

This keeps the repository lightweight and prevents generated local cache files from being committed accidentally.

## Data Pipeline

The current pipeline follows this structure:

```text
NBA.com data through hoopR
        ↓
Player game logs
        ↓
Game IDs
        ↓
Play-by-play data
        ↓
Shot events
        ↓
Shot Quality bucket rules
        ↓
Game-level player summaries
        ↓
Possession Quality scoring
        ↓
Player summaries and leaderboards
        ↓
Shiny app
```

## Metric Overview

### Shot Quality Score

Shot Quality Score is a process-focused score for the shots a player takes. It is not the same as shot-making.

The current version uses public play-by-play fields such as:

* Shot value
* Shot distance
* Shot result
* Clock context
* Action type
* Sub type
* Play description

The starter model assigns shot attempts into transparent buckets:

* `9` = highest-quality attempts, such as dunks, layups, and very strong rim attempts
* `7` = strong-quality looks
* `5` = neutral looks
* `3` = difficult looks
* `1` = very poor attempts
* `Prayer` = end-clock heaves or bailout attempts tracked separately

Prayer shots are excluded from average Shot Quality so they do not overly punish the player.

### Shot-Making Layer

Shot-making is separated from shot quality.

The current version uses placeholder expected values by shot bucket. The long-term goal is to replace those placeholder values with league-average points per shot by bucket.

Current concept:

```text
Shot-Making Score = Actual Points - Expected Points by Shot Quality Bucket
```

This allows the app to distinguish between:

* A player generating good shots
* A player making or missing shots relative to the quality of those attempts

### Possession Quality Score

Possession Quality Score is a broader value score that extends beyond shot attempts.

The current version uses available game-log stats and placeholder weights for:

* Assists
* Turnovers
* Steals
* Blocks
* Offensive rebounds
* Personal fouls
* Personal fouls drawn
* Shot quality process component
* Shot-making component

The current weights are placeholders and should not be treated as final player-value estimates.

## Current Metric Status

### Automated Now

* Player game logs
* Game date
* Matchup
* Minutes
* Field goal attempts
* Points
* Assists
* Turnovers
* Steals
* Blocks
* Offensive rebounds
* Personal fouls
* Personal fouls drawn
* Play-by-play shot attempts
* Shot distance
* Shot value
* Shot result
* Clock context
* Basic shot bucket assignment

### Estimated / Proxied Now

* Shot Quality Score
* Prayer shot detection
* Late-clock shot adjustment
* Shot-making versus placeholder expected value
* Possession Quality Score
* Possession-value event weights

### Future Work

* Expand the cached player pool
* Improve player lookup handling
* Improve shot quality bucket rules after inspecting more play-by-play descriptions
* Add league-average baselines by shot bucket
* Center average shot process around roughly 0
* Separate shot quality and shot-making more rigorously
* Add potential assists if available through tracking data
* Add deflections if available through hustle/tracking data
* Add charges drawn if available through hustle/tracking data or manual tagging
* Add player archetype and position filters
* Improve game-level visualizations
* Add game-level bucket distribution by player
* Add a small sample cache or mock data option for easier demo use
* Improve deployment workflow
* Build a methodology page with more detailed formulas and assumptions

## Model Philosophy

Shot Quality is process.
Shot Making is results versus expectation.
Possession Quality is broader contribution beyond shot attempts.

The current version is a public-data proxy model, not a final film model. It uses available NBA.com data and transparent placeholder assumptions where richer tracking or manual tagging is not yet available.

Placeholder weights should remain visible until league-average calibration is built.

## Important Limitations

The current model does not fully capture:

* Contest quality
* Defensive matchup difficulty
* Off-ball creation
* Advantage creation before the shot
* Play call context
* Help defense and rotation context
* Manual film tags
* Full possession-level expected value

Because of this, current scores should be interpreted as exploratory process indicators, not definitive player grades.

## Development Notes

This project is being built in stages.

Current milestone:

```text
Working cache-first Shiny scaffold with one tested player example.
```

Next milestone:

```text
Expand from one tested player to a small validated player pool, then improve player search, leaderboard reliability, and game-level visualizations.
```
