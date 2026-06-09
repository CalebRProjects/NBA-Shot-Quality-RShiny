# Test player ID lists ---------------------------------------------------------
# Controlled player pools for cache-building tests.
# These are used to stress-test the SQ/PQ model across player types without
# editing core pipeline code.

# Confirmed working single-player test ----------------------------------------

test_players_luka <- c(
  "1629029" # Luka Doncic
)

# Small multi-player test ------------------------------------------------------
# Useful for checking player dropdown, charts, leaderboards, and reactivity.

test_players_small_playoff_sample <- c(
  "1629029", # Luka Doncic
  "2544",    # LeBron James
  "1630162", # Anthony Edwards
  "201939",  # Stephen Curry
  "203999",  # Nikola Jokic
  "1628369", # Jayson Tatum
  "1627759"  # Jaylen Brown
)

# 30-player archetype test -----------------------------------------------------
# Purpose: stress-test SQ/PQ across creators, finishers, bigs, guards, wings,
# stars, role players, and lower-usage players.
#
# Note: depending on endpoint availability and playoff participation, not every
# requested player may return game-log data. The cache builder is expected to
# skip players without returned postseason data.

test_players_30_archetype_sample <- c(
  # Primary creators / high-usage engines
  "1629029", # Luka Doncic
  "203999",  # Nikola Jokic
  "1628983", # Shai Gilgeous-Alexander
  "1628973", # Jalen Brunson
  "201939",  # Stephen Curry
  "1630162", # Anthony Edwards
  "1628378", # Donovan Mitchell
  "1630169", # Tyrese Haliburton
  
  # Secondary creators / scoring wings
  "2544",    # LeBron James
  "1628369", # Jayson Tatum
  "1627759", # Jaylen Brown
  "1631094", # Paolo Banchero
  "1631114", # Jalen Williams
  "1628971", # Michael Porter Jr.
  "1628969", # Mikal Bridges
  
  # Bigs / rim pressure / post hubs / finishers
  "203507",  # Giannis Antetokounmpo
  "1626157", # Karl-Anthony Towns
  "1630578", # Alperen Sengun
  "1630596", # Evan Mobley
  "1631096", # Chet Holmgren
  "203497",  # Rudy Gobert
  "1628392", # Isaiah Hartenstein
  
  # Guards / movement / pull-up / smaller creators
  "1629636", # Darius Garland
  "1629630", # Ja Morant
  "201935",  # James Harden
  "203081",  # Damian Lillard
  "1630559", # Austin Reaves
  
  # Role players / defenders / lower-usage connectors
  "1628384", # OG Anunoby
  "1630178", # Tari Eason
  "201144"   # Mike Conley
)

# 50-player playoff sample -----------------------------------------------------
# Purpose: larger playoff-only sample using players from 2024-25 playoff teams.
# This gives the app a broader v1 leaderboard while avoiding obvious non-playoff
# players who will return empty postseason game logs.

test_players_50_playoff_sample <- c(
  # Existing 30-player archetype sample
  test_players_30_archetype_sample,
  
  # Thunder
  "1629652", # Luguentz Dort
  "1627936", # Alex Caruso
  "1630198", # Isaiah Joe
  "1641717", # Cason Wallace
  
  # Pacers
  "1627783", # Pascal Siakam
  "1626167", # Myles Turner
  "1629614", # Andrew Nembhard
  "1630174", # Aaron Nesmith
  "1631097", # Bennedict Mathurin
  
  # Knicks
  "1628404", # Josh Hart
  "1628978", # Donte DiVincenzo
  "1629011", # Mitchell Robinson
  "1630540", # Miles McBride
  
  # Nuggets
  "1627750", # Jamal Murray
  "203932",  # Aaron Gordon
  "1629008", # Michael Porter Jr.
  
  # Timberwolves
  "1630183", # Jaden McDaniels
  "1629675", # Naz Reid
  "203944",  # Julius Randle
  
  # Celtics
  "1628401", # Derrick White
  "201950",  # Jrue Holiday
  "204001",  # Kristaps Porzingis
  "201143",  # Al Horford
  "1630202", # Payton Pritchard
  
  # Cavaliers
  "1628386", # Jarrett Allen
  "1629622", # Max Strus
  "1630205", # Isaac Okoro
  
  # Heat
  "1628389", # Bam Adebayo
  "1629639", # Tyler Herro
  "1631170", # Jaime Jaquez Jr.
  
  # Magic
  "1630532", # Franz Wagner
  "1630591", # Jalen Suggs
  "1628976", # Wendell Carter Jr.
  "1630175", # Cole Anthony
  
  # Rockets
  "1630224", # Jalen Green
  "1627832", # Fred VanVleet
  "1628415", # Dillon Brooks
  "1641708", # Amen Thompson
  
  # Warriors
  "202710",  # Jimmy Butler
  "203110",  # Draymond Green
  "1630228", # Jonathan Kuminga
  "1641764", # Brandin Podziemski
  "1627741", # Buddy Hield
  
  # Lakers
  "1629060", # Rui Hachimura
  "1627827", # Dorian Finney-Smith
  "1629216"  # Gabe Vincent
) |>
  unique()

test_players_regular_season_small <- c(
  "1628983", # Shai Gilgeous-Alexander
  "203999",  # Nikola Jokic
  "201939",  # Stephen Curry
  "1629029", # Luka Doncic
  "1630162"  # Anthony Edwards
)

# Player archetype labels ------------------------------------------------------
# Used for diagnostics, methodology checks, and future leaderboard filters.

test_player_archetypes <- tibble::tribble(
  ~player_id, ~archetype,
  
  "1629029", "Primary creator",
  "203999",  "Primary creator big",
  "1628983", "Primary creator",
  "1628973", "Primary creator",
  "201939",  "Movement shooter/creator",
  "1630162", "Scoring wing creator",
  "1628378", "Scoring guard creator",
  "1630169", "Pass-first creator",
  
  "2544",    "Point forward",
  "1628369", "Scoring wing",
  "1627759", "Scoring wing",
  "1631094", "Scoring forward",
  "1631114", "Secondary creator",
  "1628971", "Play-finishing wing",
  "1628969", "Two-way wing",
  
  "203507",  "Rim pressure big",
  "1626157", "Stretch big",
  "1630578", "Post hub",
  "1630596", "Defensive big",
  "1631096", "Stretch/rim big",
  "203497",  "Rim-running big",
  "1628392", "Connector big",
  
  "1629636", "Scoring guard",
  "1629630", "Rim pressure guard",
  "201935",  "Pick-and-roll creator",
  "203081",  "Pull-up guard",
  "1630559", "Secondary guard",
  
  "1628384", "3-and-D wing",
  "1630178", "Energy forward",
  "201144",  "Veteran connector"
)

# Current default test group ---------------------------------------------------
# Change this object when you want the cache builder to use a different sample.

test_players_default <- test_players_50_playoff_sample
