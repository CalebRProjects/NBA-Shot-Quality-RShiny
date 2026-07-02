# app.R
# NBA Attempt Quality + Possession Quality Shiny app.

required_pkgs <- c(
  "shiny",
  "bslib",
  "tidyverse",
  "readr",
  "stringr",
  "DT",
  "scales",
  "glue"
)

invisible(lapply(required_pkgs, require, character.only = TRUE))

source("R/app_helpers.R")
source("R/app_ui.R")
source("R/app_server.R")

shiny::shinyApp(
  ui = app_ui(),
  server = app_server
)