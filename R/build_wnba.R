#' Build WNBA Player Data
#'
#' @description
#' Builds a dataset of WNBA players with performance data from the wehoop package.
#' By default, uses pre-packaged data included with the package. When use_cached=FALSE,
#' fetches and processes data from APIs, which may take significant time.
#'
#' @param output_path Character string. Optional path to save output CSV file.
#' @param use_cached Logical. Whether to use cached data (TRUE) or rebuild from APIs (FALSE).
#' @param seasons Integer vector. Seasons to pull data for. Default is 2003:2024.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A data frame containing WNBA player data if output_path is NULL.
#'         Otherwise, writes to the specified path and returns that path invisibly.
#'
#' @importFrom wehoop load_wnba_player_box load_wnba_team_box wnba_teams wnba_commonteamroster
#' @importFrom magrittr %>%
#' @importFrom lubridate interval as.period year ymd mdy
#' @importFrom dplyr select mutate filter group_by summarize ungroup arrange left_join rename distinct n_distinct
#' @importFrom stringr str_squish
#'
#' @examples
#' \dontrun{
#' # Get pre-packaged data
#' wnba_data <- build_wnba()
#'
#' # Rebuild data from APIs (takes time)
#' wnba_data_fresh <- build_wnba(use_cached = FALSE)
#'
#' # Rebuild for specific seasons
#' wnba_data_recent <- build_wnba(use_cached = FALSE, seasons = 2020:2023)
#'
#' # Save to specific location
#' build_wnba(output_path = "path/to/save/directory")
#' }
#'
#' @export
build_wnba <- function(output_path = NULL, use_cached = TRUE, seasons = 2003:2024, ...) {
  # First try package data if use_cached is TRUE
  if(use_cached) {
    wnba_data <- tryCatch({
      # Try to load package data
      wnba_tidy <- NULL
      data("wnba_tidy", envir = environment())
      wnba_tidy
    }, error = function(e) {
      # If loading package data fails, try to load from CSV
      csv_path <- system.file("extdata", "wnba_final.csv", package = "yourpackage")
      if (csv_path == "") {
        warning("Could not find cached WNBA data. Attempting to rebuild from APIs.")
        return(NULL)
      }
      tryCatch({
        read.csv(csv_path)
      }, error = function(e2) {
        warning("Could not read CSV file. Attempting to rebuild from APIs.")
        return(NULL)
      })
    })

    # If we successfully loaded the data, either return it or save it
    if (!is.null(wnba_data)) {
      if (!is.null(output_path)) {
        # Create directory if it doesn't exist
        if (!dir.exists(output_path)) {
          dir.create(output_path, recursive = TRUE)
        }
        write.csv(wnba_data, file.path(output_path, "wnba_tidy.csv"), row.names = FALSE)
        message("WNBA data saved to ", file.path(output_path, "wnba_tidy.csv"))
        return(invisible(file.path(output_path, "wnba_tidy.csv")))
      } else {
        return(wnba_data)
      }
    }
  }

  # If we get here, either use_cached was FALSE or we couldn't load cached data
  message("Building WNBA data from APIs. This may take significant time...")

  # Check for required package
  if (!requireNamespace("wehoop", quietly = TRUE)) {
    stop("Package 'wehoop' is required for building WNBA data. Please install it with install.packages('wehoop').")
  }

  # Create data-raw directory if it doesn't exist
  if (!dir.exists("data-raw")) {
    dir.create("data-raw", recursive = TRUE)
  }

  # Load player box score data
  message("Loading WNBA player box score data...")
  WNBAplayerStats <- wehoop::load_wnba_player_box(seasons = seasons)

  # Add position information
  WNBAplayerStats <- WNBAplayerStats %>%
    dplyr::mutate(
      position = athlete_position_abbreviation
    )

  # Load team box score data
  message("Loading WNBA team box score data...")
  WNBAgameStats <- wehoop::load_wnba_team_box(seasons = seasons)

  # Aggregate game stats
  WNBAcombined_games <- WNBAgameStats %>%
    dplyr::group_by(game_id) %>%
    dplyr::summarize(
      game_pts = sum(team_score),
      game_fg_m = sum(field_goals_made),
      game_ft_m = sum(free_throws_made),
      game_fg_a = sum(field_goals_attempted),
      game_ft_a = sum(free_throws_attempted),
      game_def_reb = sum(defensive_rebounds),
      game_off_reb = sum(offensive_rebounds),
      game_assists = sum(assists),
      game_steals = sum(steals),
      game_blocks = sum(blocks),
      game_pf = sum(fouls),
      game_to = sum(turnovers)
    )

  # Extract relevant box score data
  WNBAboxStats <- WNBAplayerStats %>%
    dplyr::select(game_id, season, athlete_id, position, athlete_display_name, minutes,
                  field_goals_made, field_goals_attempted, three_point_field_goals_made,
                  three_point_field_goals_attempted, free_throws_made, free_throws_attempted,
                  offensive_rebounds, defensive_rebounds, rebounds, assists, steals, blocks,
                  turnovers, fouls, points, athlete_position_abbreviation, team_score,
                  team_winner) %>%
    dplyr::arrange(athlete_id) %>%
    dplyr::filter(!is.na(minutes))

  # Join box score data with game aggregates
  WNBAcombined_stats <- WNBAboxStats %>%
    dplyr::left_join(WNBAcombined_games, by = "game_id")

  # Remove any NA rows
  WNBAcombined_stats_final <- stats::na.omit(WNBAcombined_stats)

  # Calculate Player Impact Estimate (PIE)
  WNBAcombined_stats <- WNBAcombined_stats_final %>%
    dplyr::mutate(pie = ((points + field_goals_made + free_throws_made - field_goals_attempted - free_throws_attempted + defensive_rebounds +
                            (offensive_rebounds*0.5) + assists + steals + (blocks*0.5) - fouls - turnovers) / (game_pts + game_fg_m + game_ft_m - game_fg_a -
                                                                                                                 game_ft_a + game_def_reb + (game_off_reb * 0.5) +
                                                                                                                 game_assists + game_steals + (game_blocks*0.5) -
                                                                                                                 game_pf - game_to)))

  # Aggregate to season level
  WNBAseason_stats <- WNBAcombined_stats %>%
    dplyr::group_by(athlete_id, athlete_display_name, season) %>%
    dplyr::summarize(
      position = first(position),
      pie.season = sum(pie),
      pie.game = mean(pie),
      games_played = dplyr::n_distinct(game_id),
      pie.season.f = pie.game * 44  # WNBA season is 44 games
    ) %>%
    dplyr::select(
      athlete_id, position, season, pie.season, pie.game, games_played, pie.season.f
    )

  # Scale PIE values within position groups and across league
  WNBAseason.scaled <- WNBAseason_stats %>%
    dplyr::group_by(season, position) %>%
    dplyr::mutate(
      pie.season.s.p = scale(pie.season),
      pie.game.s.p = scale(pie.game),
      pct_season_played = games_played / 44,  # WNBA season is 44 games
      pie.season.f.s.p = scale(pie.season.f)
    ) %>%
    dplyr::group_by(season, position) %>%
    dplyr::mutate(
      pie.season.s = scale(pie.season.s.p),
      pie.game.s = scale(pie.game.s.p),
      pct_season_played = games_played / 44,
      pie.season.f.s = scale(pie.season.f.s.p)
    ) %>%
    dplyr::ungroup()

  # Fetch roster data to get birth dates
  message("Fetching roster data for birth dates...")

  # Either fetch live or use cached data
  # This is resource intensive, so we'll check for a cached file first
  if (file.exists("data-raw/all_wnba_rosters.csv")) {
    message("Using cached roster data from data-raw/all_wnba_rosters.csv")
    all_rosters <- read.csv("data-raw/all_wnba_rosters.csv")
  } else {
    message("Fetching roster data from API (this will take time)...")
    # Get WNBA team info
    wnba <- wehoop::wnba_teams()
    team_ids <- wnba$team_id

    # Initialize empty data frame
    all_rosters <- data.frame()

    # Loop through seasons and teams
    for(season in seasons){
      for(team_id in team_ids){
        tryCatch({
          roster_data <- wehoop::wnba_commonteamroster(
            league_id = "10",
            season = season,
            team_id = team_id
          )
          roster <- roster_data$CommonTeamRoster

          all_rosters <- dplyr::bind_rows(all_rosters, roster)
          Sys.sleep(1)  # Avoid rate limiting
        }, error = function(e) {
          message(paste("Error fetching roster for team", team_id, "in season", season, ":", conditionMessage(e)))
        })
      }
    }

    # Save roster data for future use
    write.csv(all_rosters, "data-raw/all_wnba_rosters.csv", row.names = FALSE)
  }

  # Process roster data
  all_rosters <- all_rosters %>%
    dplyr::rename(athlete_display_name = PLAYER, season = SEASON)

  all_rosters2 <- all_rosters %>%
    dplyr::select(season, athlete_display_name, BIRTH_DATE, PLAYER_ID) %>%
    stats::na.omit() %>%
    dplyr::arrange(athlete_display_name)

  all_rosters2$season <- as.integer(all_rosters2$season)

  # Clean player names to handle potential whitespace issues
  WNBAseason.scaled$athlete_display_name <- stringr::str_squish(WNBAseason.scaled$athlete_display_name)

  # Create roster data for joining
  Wroster_data <- all_rosters2 %>%
    dplyr::select(athlete_display_name, PLAYER_ID, BIRTH_DATE) %>%
    dplyr::distinct(athlete_display_name, PLAYER_ID, BIRTH_DATE)

  # Join with birth dates
  Wseason_data_birthday <- WNBAseason.scaled %>%
    dplyr::left_join(Wroster_data, by = "athlete_display_name") %>%
    dplyr::select(athlete_id, PLAYER_ID, BIRTH_DATE)

  Wseason_data_birthday <- Wseason_data_birthday %>%
    dplyr::mutate(birth_date = lubridate::mdy(BIRTH_DATE)) %>%
    dplyr::group_by(athlete_id) %>%
    dplyr::summarize(
      birth_date = first(birth_date)
    ) %>%
    stats::na.omit()

  # Create final dataset with ages
  WNBA.final <- WNBAseason.scaled %>%
    dplyr::left_join(Wseason_data_birthday, by = "athlete_id") %>%
    stats::na.omit() %>%
    dplyr::mutate(
      # Convert season year to May 1st start date (WNBA season typically starts in May)
      season_start = lubridate::ymd(paste0(season, "-5-01")),

      # Calculate age at season start using lubridate
      age = lubridate::interval(birth_date, season_start) %>%
        lubridate::as.period() %>%
        lubridate::year()
    )

  # Save final dataset
  write.csv(WNBA.final, "data-raw/wnba_final.csv", row.names = FALSE)

  # Prepare output
  if (!is.null(output_path)) {
    # Create directory if it doesn't exist
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE)
    }
    write.csv(WNBA.final, file.path(output_path, "wnba_tidy.csv"), row.names = FALSE)
    message("WNBA data saved to ", file.path(output_path, "wnba_tidy.csv"))
    return(invisible(file.path(output_path, "wnba_tidy.csv")))
  } else {
    return(WNBA.final)
  }
}
