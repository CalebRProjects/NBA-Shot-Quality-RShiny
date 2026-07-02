# R/app_ui.R
# Main Shiny UI for the Attempt Quality + Possession Quality app.

required_pkgs <- c("shiny", "bslib", "DT", "glue")
invisible(lapply(required_pkgs, require, character.only = TRUE))


app_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Inter")
)


app_ui <- function() {
  shiny::fluidPage(
    theme = app_theme,
    
    shiny::tags$head(
      shiny::tags$style(
        HTML(
          "
          body {
            background-color: #f7f4ee;
          }

          .app-title {
            font-weight: 800;
            margin-top: 18px;
            margin-bottom: 4px;
          }

          .app-subtitle {
            color: #555;
            font-size: 16px;
            margin-bottom: 18px;
          }

          .metric-card {
            background: #ffffff;
            border-radius: 14px;
            padding: 16px;
            margin-bottom: 14px;
            box-shadow: 0 1px 6px rgba(0,0,0,0.08);
          }

          .metric-label {
            color: #666;
            font-size: 13px;
            text-transform: uppercase;
            letter-spacing: 0.04em;
            margin-bottom: 4px;
          }

          .metric-value {
            font-size: 24px;
            font-weight: 800;
          }

          .section-card {
            background: #ffffff;
            border-radius: 14px;
            padding: 18px;
            margin-bottom: 18px;
            box-shadow: 0 1px 6px rgba(0,0,0,0.08);
          }

          .method-list li {
            margin-bottom: 8px;
          }
          
          .player-hero {
            background: #ffffff;
            border: 1px solid #e8dfd3;
            border-radius: 18px;
            padding: 20px 22px;
            margin-bottom: 18px;
            box-shadow: 0 1px 8px rgba(0,0,0,0.05);
          }

          .player-hero-top {
            display: flex;
            justify-content: space-between;
            align-items: flex-end;
            gap: 18px;
            margin-bottom: 18px;
          }

          .player-name {
            font-size: 30px;
            font-weight: 800;
            line-height: 1.05;
            color: #1f1f1f;
          }

          .player-context {
            font-size: 13px;
            color: #6f6a63;
            margin-top: 6px;
          }

          .player-volume {
            font-size: 13px;
            color: #6f6a63;
            text-align: right;
          }

          .stat-rail {
            display: grid;
            grid-template-columns: repeat(4, minmax(160px, 1fr));
            gap: 20px;
            border-top: 1px solid #eee6dc;
            padding-top: 16px;
          }

          .stat-unit {
            min-width: 0;
          }

          .stat-label {
            font-size: 11px;
            color: #7b756e;
            text-transform: uppercase;
            letter-spacing: 0.08em;
            font-weight: 800;
            margin-bottom: 5px;
          }

          .stat-line {
            display: flex;
            align-items: baseline;
            gap: 8px;
            margin-bottom: 7px;
          }

          .stat-value {
            font-size: 25px;
            font-weight: 850;
            color: #202020;
          }

          .stat-rank {
            font-size: 12px;
            color: #6f6a63;
            font-weight: 700;
          }

          .thin-bar {
            width: 100%;
            height: 6px;
            background: #ece6dd;
            border-radius: 999px;
            overflow: hidden;
          }

          .thin-bar-fill {
            height: 100%;
            border-radius: 999px;
            background: linear-gradient(90deg, #d66a5f 0%, #e8bd65 45%, #67a86f 100%);
          }

          .stat-note {
            font-size: 12px;
            color: #7b756e;
            margin-top: 12px;
          }
          "
        )
      )
    ),
    
    shiny::div(
      class = "container-fluid",
      
      shiny::h1("NBA Attempt Quality + Possession Quality", class = "app-title"),
      shiny::div(
        "A public-data NBA app for player shot profile, expected attempt value, shot-making, and possession event value.",
        class = "app-subtitle"
      ),
      
      shiny::tabsetPanel(
        id = "main_tabs",
        
        shiny::tabPanel(
          "Home",
          shiny::br(),
          
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shiny::div(
                class = "metric-card",
                shiny::div("Season", class = "metric-label"),
                shiny::div(textOutput("home_season"), class = "metric-value")
              )
            ),
            shiny::column(
              width = 3,
              shiny::div(
                class = "metric-card",
                shiny::div("Players", class = "metric-label"),
                shiny::div(textOutput("home_players"), class = "metric-value")
              )
            ),
            shiny::column(
              width = 3,
              shiny::div(
                class = "metric-card",
                shiny::div("Games", class = "metric-label"),
                shiny::div(textOutput("home_games"), class = "metric-value")
              )
            ),
            shiny::column(
              width = 3,
              shiny::div(
                class = "metric-card",
                shiny::div("Attempts", class = "metric-label"),
                shiny::div(textOutput("home_fga"), class = "metric-value")
              )
            )
          ),
          
          shiny::div(
            class = "section-card",
            shiny::h3("What this app measures"),
            shiny::p(
              "Attempt Quality grades each field-goal attempt based on shot location, shot type, and public-data context. ",
              "Possession Quality adds event value for actions like assists, offensive rebounds, steals, blocks, turnovers, and free throws."
            ),
            shiny::p(
              "The goal is not to replace tracking data. It is a transparent public-data proxy that separates shot diet, shot-making, and broader possession events."
            )
          ),
          
          shiny::div(
            class = "section-card",
            shiny::h3("Top Combined Value"),
            DT::DTOutput("home_top_combined")
          )
        ),
        
        shiny::tabPanel(
          "Methodology",
          shiny::br(),
          
          shiny::div(
            class = "section-card",
            shiny::h3("Attempt Quality"),
            shiny::tags$ul(
              class = "method-list",
              shiny::tags$li("Each shot receives an Attempt Quality bucket: 9, 7, 5, 3, 1, or Grenade."),
              shiny::tags$li("Rim attempts, dunks, layups, and alley-oops are treated as the highest-value attempts."),
              shiny::tags$li("Corner threes, normal above-break threes, and valuable paint attempts are strong attempts."),
              shiny::tags$li("Midrange, long twos, and difficult late-clock/deep attempts receive lower scores."),
              shiny::tags$li("Grenades are separated from the normal 1-to-9 scale because they represent forced or extreme attempts.")
            )
          ),
          
          shiny::div(
            class = "section-card",
            shiny::h3("Expected Points + Shot Making"),
            shiny::p(
              "Expected points are calculated from the observed value of each Attempt Quality bucket within the selected season type. ",
              "Shot Making Value equals actual points minus expected points."
            ),
            DT::DTOutput("method_expected_points")
          ),
          
          shiny::div(
            class = "section-card",
            shiny::h3("Possession Quality"),
            shiny::tags$ul(
              class = "method-list",
              shiny::tags$li("Made shots receive their point value."),
              shiny::tags$li("Assists, offensive rebounds, steals, and blocks add possession value."),
              shiny::tags$li("Turnovers and offensive fouls subtract value."),
              shiny::tags$li("Free throws are included as made and missed free-throw events."),
              shiny::tags$li("This is an event-value proxy, not a true possession-by-possession model.")
            )
          )
        ),
        
        shiny::tabPanel(
          "Player Search",
          shiny::br(),
          
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shiny::selectInput(
                "player_season",
                "Season",
                choices = NULL
              )
            ),
            shiny::column(
              width = 3,
              shiny::selectInput(
                "player_season_type",
                "Season Type",
                choices = c(
                  "Regular Season" = "regular",
                  "Playoffs" = "playoffs"
                ),
                selected = "regular"
              )
            ),
            shiny::column(
              width = 4,
              shiny::uiOutput("player_selector_ui")
            ),
            shiny::column(
              width = 2,
              shiny::sliderInput(
                "last_n_games",
                "Last N Games",
                min = 1,
                max = 25,
                value = 10,
                step = 1
              )
            )
          ),
          
          shiny::uiOutput("player_hero_ui"),
          
          shiny::div(
            class = "section-card",
            shiny::h3("Game Log"),
            DT::DTOutput("player_game_log")
          ),
          
          shiny::div(
            class = "section-card",
            shiny::h3("Recent Shot Attempts"),
            DT::DTOutput("player_shots")
          )
        ),
        
        shiny::tabPanel(
          "Leaderboards",
          shiny::br(),
          
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shiny::selectInput(
                "leaderboard_season",
                "Season",
                choices = NULL
              )
            ),
            shiny::column(
              width = 3,
              shiny::selectInput(
                "leaderboard_season_type",
                "Season Type",
                choices = c(
                  "Regular Season" = "regular",
                  "Playoffs" = "playoffs"
                ),
                selected = "regular"
              )
            ),
            shiny::column(
              width = 4,
              shiny::selectInput(
                "leaderboard_key",
                "Leaderboard",
                choices = stats::setNames(
                  names(leaderboard_labels),
                  leaderboard_labels
                ),
                selected = "avg_attempt_quality"
              )
            ),
            shiny::column(
              width = 2,
              shiny::sliderInput(
                "leaderboard_n",
                "Rows",
                min = 10,
                max = 100,
                value = 25,
                step = 5
              )
            )
          ),
          
          shiny::div(
            class = "section-card",
            shiny::h3(textOutput("leaderboard_title")),
            DT::DTOutput("leaderboard_table")
          )
        ),
        
        shiny::tabPanel(
          "Data Status",
          shiny::br(),
          
          shiny::div(
            class = "section-card",
            shiny::h3("Regular Season Cache"),
            DT::DTOutput("regular_metadata")
          ),
          
          shiny::div(
            class = "section-card",
            shiny::h3("Playoff Cache"),
            DT::DTOutput("playoff_metadata")
          )
        )
      )
    )
  )
}