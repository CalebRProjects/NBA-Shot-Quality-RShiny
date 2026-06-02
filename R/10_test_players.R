# Test player ID lists ---------------------------------------------------------
# Small controlled player pools for cache-building tests.
# Keep this separate from the main cache builder so we can quickly test
# different groups without editing core pipeline code.

test_players_luka <- c(
  "1629029" # Luka Doncic
)

test_players_lakers_wolves <- c(
  "1629029", # Luka Doncic
  "2544",    # LeBron James
  "1630162"  # Anthony Edwards
)

test_players_small_playoff_sample <- c(
  "1629029", # Luka Doncic
  "2544",    # LeBron James
  "1630162", # Anthony Edwards
  "201939",  # Stephen Curry
  "203999",  # Nikola Jokic
  "1628369", # Jayson Tatum
  "1627759"  # Jaylen Brown
)
