#' Build NHL Player Data
#'
#' @description
#' Builds a dataset of NHL players with performance data from the fastRhockey package.
#' By default, uses pre-packaged data included with the package. When use_cached=FALSE,
#' fetches and processes data from APIs, which may take significant time.
#'
#' @param output_path Character string. Optional path to save output CSV file.
#' @param use_cached Logical. Whether to use cached data (TRUE) or rebuild from APIs (FALSE).
#' @param seasons Integer vector. Seasons to pull data for. Default is 2011:2023.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A data frame containing NHL player data if output_path is NULL.
#'         Otherwise, writes to the specified path and returns that path invisibly.
#'
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate filter select rename left_join inner_join group_by summarize ungroup arrange sym case_when row_number n across everything
#' @importFrom purrr map map_df map_chr map_lgl map_dbl
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' # Get pre-packaged data
#' nhl_data <- build_nhl()
#'
#' # Rebuild data from APIs (takes time)
#' nhl_data_fresh <- build_nhl(use_cached = FALSE)
#'
#' # Rebuild data for specific seasons
#' nhl_data_recent <- build_nhl(use_cached = FALSE, seasons = 2020:2023)
#'
#' # Save to specific location
#' build_nhl(output_path = "path/to/save/directory")
#' }
#'
#' @export
build_nhl <- function(output_path = NULL, use_cached = TRUE, seasons = 2011:2023, ...) {
  ms <- function(x) {
    if(is.na(x) || x == "") return(NA_real_)
    parts <- strsplit(x, ":")[[1]]
    as.numeric(parts[1]) * 60 + as.numeric(parts[2])
  }

  period_to_seconds <- function(x) {
    if(is.na(x)) return(NA_real_)
    return(as.numeric(x))
  }
  # First try package data if use_cached is TRUE
  if(use_cached) {
    nhl_data <- tryCatch({
      # Try to load package data
      nhl_tidy <- NULL
      data("nhl_tidy", envir = environment())
      nhl_tidy
    }, error = function(e) {
      # If loading package data fails, try to load from CSV
      csv_path <- system.file("extdata", "nhl_final.csv", package = "yourpackage")
      if (csv_path == "") {
        warning("Could not find cached NHL data. Attempting to rebuild from APIs.")
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
    if (!is.null(nhl_data)) {
      if (!is.null(output_path)) {
        # Create directory if it doesn't exist
        if (!dir.exists(output_path)) {
          dir.create(output_path, recursive = TRUE)
        }
        write.csv(nhl_data, file.path(output_path, "nhl_tidy.csv"), row.names = FALSE)
        message("NHL data saved to ", file.path(output_path, "nhl_tidy.csv"))
        return(invisible(file.path(output_path, "nhl_tidy.csv")))
      } else {
        return(nhl_data)
      }
    }
  }

  # If we get here, either use_cached was FALSE or we couldn't load cached data
  message("Building NHL data from APIs. This may take significant time...")

  # Check for required package
  if (!requireNamespace("fastRhockey", quietly = TRUE)) {
    stop("Package 'fastRhockey' is required for building NHL data. Please install it with install.packages('fastRhockey').")
  }

  # Create data-raw directory if it doesn't exist
  if (!dir.exists("data-raw")) {
    dir.create("data-raw", recursive = TRUE)
  }

  # Load player box score data
  message("Loading NHL player box score data...")
  nhl.player.games <- fastRhockey::load_nhl_player_box(seasons = seasons)

  # Split players by position type
  skaters <- nhl.player.games %>%
    dplyr::filter(position_type == "Forward" | position_type == "Defenseman")

  goalies <- nhl.player.games %>%
    dplyr::filter(position_type == "Goalie")

  # Calculate fantasy points for skaters
  skaters.fantasy <- skaters %>%
    dplyr::mutate(
      fantasy_points = (3 * skater_stats_goals) + (2 * skater_stats_assists) +
        (0.5 * skater_stats_shots) + (skater_stats_plus_minus) + (0.5 * skater_stats_blocked) +
        (0.5 * (skater_stats_power_play_goals + skater_stats_power_play_assists)) +
        (0.5 * (skater_stats_short_handed_goals + skater_stats_short_handed_assists))
    )

  # Calculate fantasy points for goalies
  goalies.fantasy <- goalies %>%
    dplyr::mutate(
      wins = ifelse(goalie_stats_decision == "W", 1, 0),
      shutout = ifelse(goalie_stats_save_percentage == 100, 1, 0),
      fantasy_points = (3*wins) + (-1* ((1-(goalie_stats_save_percentage/100)) * goalie_stats_shots)) +
        (0.2 * ((goalie_stats_save_percentage/100) * goalie_stats_shots)) + (2 * shutout)
    )

  # Aggregate skater data to season level
  skaters.season <- skaters.fantasy %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(
      fantasy_points_season = sum(fantasy_points, na.rm = TRUE),
      player_full_name = first(player_full_name),
      games_played = n_distinct(game_id),
      position = first(position_type),
      time_on_ice = sum(period_to_seconds(ms(skater_stats_time_on_ice)), na.rm = TRUE),
      skater_stats_assists = sum(skater_stats_assists, na.rm = TRUE),
      skater_stats_goals = sum(skater_stats_goals, na.rm = TRUE),
      skater_stats_shots = sum(skater_stats_shots, na.rm = TRUE),
      skater_stats_hits = sum(skater_stats_hits, na.rm = TRUE),
      skater_stats_power_play_goals = sum(skater_stats_power_play_goals, na.rm = TRUE),
      skater_stats_power_play_assists = sum(skater_stats_power_play_assists, na.rm = TRUE),
      skater_stats_penalty_minutes = sum(skater_stats_penalty_minutes, na.rm = TRUE),
      skater_stats_face_off_wins = sum(skater_stats_face_off_wins, na.rm = TRUE),
      skater_stats_faceoff_taken = sum(skater_stats_faceoff_taken, na.rm = TRUE),
      skater_stats_takeaways = sum(skater_stats_takeaways, na.rm = TRUE),
      skater_stats_giveaways = sum(skater_stats_giveaways, na.rm = TRUE),
      skater_stats_short_handed_goals = sum(skater_stats_short_handed_goals, na.rm = TRUE),
      skater_stats_plus_minus = sum(skater_stats_plus_minus, na.rm = TRUE),
      skater_stats_even_time_on_ice = sum(period_to_seconds(ms(skater_stats_even_time_on_ice)), na.rm = TRUE),
      skater_stats_power_play_time_on_ice = sum(period_to_seconds(ms(skater_stats_power_play_time_on_ice)), na.rm = TRUE),
      skater_stats_short_handed_time_on_ice = sum(period_to_seconds(ms(skater_stats_short_handed_time_on_ice)), na.rm = TRUE)
    )

  # Add per-game metrics for skaters
  skaters.season <- skaters.season %>%
    dplyr::mutate(
      fpts.game = fantasy_points_season / games_played,
      fpts.season = fpts.game * 82,
      pct.season.played = games_played / 82
    ) %>%
    # Handle cases where games_played = 0 to avoid division errors
    dplyr::mutate(
      fpts.game = ifelse(games_played == 0, NA_real_, fpts.game),
      fpts.season = ifelse(games_played == 0, NA_real_, fpts.season),
      pct.season.played = ifelse(games_played == 0, NA_real_, pct.season.played),
      pct.season.played = ifelse(pct.season.played > 1, 1, pct.season.played)
    )

  # Aggregate goalie data to season level
  goalies.season <- goalies.fantasy %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(
      fantasy_points_season = sum(fantasy_points, na.rm = TRUE),
      player_full_name = first(player_full_name),
      games_played = n_distinct(game_id),
      position = first(position_type),
      goalie_stats_time_on_ice = sum(period_to_seconds(ms(goalie_stats_time_on_ice)), na.rm = TRUE),
      goalie_stats_assists = sum(goalie_stats_assists, na.rm = TRUE),
      goalie_stats_goals = sum(goalie_stats_goals, na.rm = TRUE),
      goalie_stats_pim = sum(goalie_stats_pim, na.rm = TRUE),
      goalie_stats_shots = sum(goalie_stats_shots, na.rm = TRUE),
      goalie_stats_saves = sum(goalie_stats_saves, na.rm = TRUE),
      goalie_stats_power_play_saves = sum(goalie_stats_power_play_saves, na.rm = TRUE),
      goalie_stats_short_handed_saves = sum(goalie_stats_short_handed_saves, na.rm = TRUE),
      goalie_stats_even_saves = sum(goalie_stats_even_saves, na.rm = TRUE),
      goalie_stats_short_handed_shots_against = sum(goalie_stats_short_handed_shots_against, na.rm = TRUE),
      goalie_stats_even_shots_against = sum(goalie_stats_even_shots_against, na.rm = TRUE),
      goalie_stats_power_play_shots_against = sum(goalie_stats_power_play_shots_against, na.rm = TRUE),
      goalie_wins = sum(wins, na.rm = TRUE),
      goalie_stats_save_percentage = sum(goalie_stats_save_percentage, na.rm = TRUE)
    )

  # Add per-game metrics for goalies
  goalies.season <- goalies.season %>%
    dplyr::mutate(
      fpts.game = fantasy_points_season / games_played,
      fpts.season = fpts.game * 82,
      pct.season.played = games_played / 82,
    ) %>%
    # Handle cases where games_played = 0 to avoid division errors
    dplyr::mutate(
      fpts.game = ifelse(games_played == 0, NA_real_, fpts.game),
      fpts.season = ifelse(games_played == 0, NA_real_, fpts.season),
      pct.season.played = ifelse(games_played == 0, NA_real_, pct.season.played),
      pct.season.played = ifelse(pct.season.played > 1, 1, pct.season.played)
    )

  # Calculate advanced per-game metrics for skaters
  skaters.prepped <- skaters.season %>%
    dplyr::mutate(
      # Offensive stats
      goals_per_game = skater_stats_goals / games_played,
      assists_per_game = skater_stats_assists / games_played,
      shots_per_game = skater_stats_shots / games_played,
      ppg_per_game = skater_stats_power_play_goals / games_played,
      ppa_per_game = skater_stats_power_play_assists / games_played,

      # Defensive/physical stats
      hits_per_game = skater_stats_hits / games_played,
      takeaways_per_game = skater_stats_takeaways / games_played,
      giveaways_per_game = skater_stats_giveaways / games_played,

      # Time metrics
      toi_per_game = time_on_ice / games_played,
      even_toi_per_game = skater_stats_even_time_on_ice / games_played,
      pp_toi_per_game = skater_stats_power_play_time_on_ice / games_played,
      sh_toi_per_game = skater_stats_short_handed_time_on_ice / games_played,

      # Faceoff win percentage (only relevant for centers, a subset of forwards)
      faceoff_win_pct = ifelse(skater_stats_faceoff_taken > 0,
                               skater_stats_face_off_wins / skater_stats_faceoff_taken * 100,
                               NA)
    )

  # Create position-specific value metrics for skaters
  skaters.with.metrics <- skaters.prepped %>%
    dplyr::mutate(
      # Forward Value Metric (FVM)
      position_value = dplyr::case_when(
        position == "Forward" ~ (
          (goals_per_game * 5) +             # Goals are highly valuable
            (assists_per_game * 3) +           # Assists are valuable but less than goals
            (shots_per_game * 0.5) +           # Shot generation has value
            (ppg_per_game * 2) +               # Power play goals add extra value
            (ppa_per_game * 1.5) +             # Power play assists add value
            (takeaways_per_game * 1) +         # Good defensive play
            (giveaways_per_game * -0.5) +      # Negative for turnovers
            (skater_stats_plus_minus * 0.2) +  # Plus/minus has some value
            # Add faceoff component only if player takes faceoffs
            (ifelse(skater_stats_faceoff_taken > 100,
                    (faceoff_win_pct - 50) * 0.05,
                    0))
        ),
        # Defenseman Value Metric (DVM)
        position == "Defenseman" ~ (
          (toi_per_game * 0.2) +             # Ice time indicates trust/value
            (assists_per_game * 3) +           # Assists are important for defensemen
            (goals_per_game * 3) +             # Goals are valuable but less common
            (hits_per_game * 1) +              # Physical play
            (takeaways_per_game * 2) +         # Good defensive play
            (giveaways_per_game * -1) +        # Negative for turnovers
            (skater_stats_plus_minus * 0.5) +  # Plus/minus more important for D
            (sh_toi_per_game * 0.5) +          # Shorthanded time indicates defensive value
            (pp_toi_per_game * 0.3)            # Power play time indicates offensive skills
        ),
        TRUE ~ NA_real_  # Only calculate for defined positions
      )
    )

  # Calculate advanced metrics for goalies
  goalies.prepped <- goalies.season %>%
    dplyr::mutate(
      # Per-game statistics
      toi_per_game = goalie_stats_time_on_ice / games_played,
      saves_per_game = goalie_stats_saves / games_played,
      shots_against_per_game = goalie_stats_shots / games_played,

      # Calculate save percentages by situation
      save_pct = goalie_stats_saves / goalie_stats_shots,
      even_save_pct = goalie_stats_even_saves / goalie_stats_even_shots_against,
      pp_save_pct = goalie_stats_power_play_saves / goalie_stats_power_play_shots_against,
      sh_save_pct = goalie_stats_short_handed_saves / goalie_stats_short_handed_shots_against,

      # Calculate goals against and goals against average
      goals_against = goalie_stats_shots - goalie_stats_saves,
      goals_against_avg = (goals_against * 60) / goalie_stats_time_on_ice,

      # Win percentage
      win_pct = goalie_wins / games_played,

      # Workload metrics
      workload = goalie_stats_shots / games_played,

      # Quality start calculation
      quality_start_threshold = 0.915,
      is_quality_start = save_pct >= quality_start_threshold,

      # Goals saved above average (GSAA)
      league_avg_save_pct = 0.910, # Approximate league average
      expected_goals_against = goalie_stats_shots * (1 - league_avg_save_pct),
      gsaa = expected_goals_against - goals_against
    )

  # Create goalie value metric
  goalies.with.metrics <- goalies.prepped %>%
    dplyr::mutate(
      # Goalie Value Metric (GVM)
      position_value = (
        # Base save percentage (most important factor)
        (save_pct * 1000 - 900) * 2 +  # Transform from ~.900-.930 to ~0-60 range

          # Goals saved above average (per game played)
          (gsaa / games_played) * 5 +

          # Win percentage (contributes significantly)
          (win_pct * 20) +

          # Special teamssave percentage (bonus for strong PK performance)
          ifelse(!is.na(sh_save_pct),
                 (sh_save_pct * 100 - 85) * 0.5, 0.2) +

          # Workload adjustment (small bonus for handling high shot volumes)
          (workload - 30) * 0.1
      )
    )

  # Combine skaters and goalies
  nhl.combined <- dplyr::bind_rows(skaters.with.metrics, goalies.with.metrics)

  # Fetch birth dates from API (or load cached data)
  message("Fetching player birth dates...")

  # In the original code, birthdays are loaded from a CSV
  # For production code, we should make API calls to fetch this data
  # For now, we'll either use a cached file or create API calls

  if (file.exists("data-raw/nhl_birthdays.csv")) {
    message("Using cached birthday data from data-raw/nhl_birthdays.csv")
    rosters.2 <- read.csv("data-raw/nhl_birthdays.csv")
  } else {
    message("Fetching birthdays from API (this will take time)...")

    # Function to process a single player
    process_player <- function(player, position_type) {
      # Extract basic player info
      player_data <- list(
        id = player$id,
        birthDate = player$birthDate,
        position = position_type  # Add the position type
      )

      return(player_data)
    }

    # Function to process a complete roster
    process_roster <- function(roster_data) {
      # Convert data frames to lists of rows
      forwards_list <- split(roster_data$forwards, seq(nrow(roster_data$forwards)))
      defensemen_list <- split(roster_data$defensemen, seq(nrow(roster_data$defensemen)))
      goalies_list <- split(roster_data$goalies, seq(nrow(roster_data$goalies)))

      # Process each group of players
      forwards <- purrr::map(forwards_list, ~process_player(., "Forward"))
      defensemen <- purrr::map(defensemen_list, ~process_player(., "Defenseman"))
      goalies <- purrr::map(goalies_list, ~process_player(., "Goalie"))

      # Combine all players and convert to data frame
      all_players <- c(forwards, defensemen, goalies)
      players_df <- dplyr::bind_rows(all_players)

      return(players_df)
    }

    # Fetch roster from API
    fetch_roster <- function(team_id, season) {
      # Format season
      season_formatted <- paste0(season, season + 1)

      # Construct API URL
      api_url <- paste0("https://api-web.nhle.com/v1/roster/", team_id, "/", season_formatted)

      # Make API request
      response <- httr::GET(api_url)

      if (httr::status_code(response) == 200) {
        # Parse JSON response
        roster_data <- jsonlite::fromJSON(rawToChar(response$content), flatten = FALSE)

        # Process roster data
        processed_roster <- process_roster(roster_data)

        return(processed_roster)
      } else {
        warning(paste("Failed to fetch data for team", team_id, "in season", season_formatted))
        return(NULL)
      }
    }

    # Function to fetch multiple seasons/teams
    fetch_multiple_rosters <- function(team_ids, seasons) {
      all_rosters <- data.frame()

      for (season in seasons) {
        for (team_id in team_ids) {
          # Skip teamsthat didn't exist in certain seasons
          if (team_id == "VGK" && season < 2017) next
          if (team_id == "SEA" && season < 2021) next

          tryCatch({
            roster <- fetch_roster(team_id, season)
            if (!is.null(roster)) {
              all_rosters <- dplyr::bind_rows(all_rosters, roster)
            }

            # Add delay to avoid rate limiting
            Sys.sleep(2)
          }, error = function(e) {
            message(paste("Error fetching data for team", team_id, "in season", season, ":", e$message))
          })
        }
      }

      return(all_rosters)
    }

    # Define team IDs and seasons
    team_ids <- c(
      "ANA", "ARI", "BOS", "BUF", "CGY", "CAR", "CHI", "COL", "CBJ", "DAL",
      "DET", "EDM", "FLA", "LAK", "MIN", "MTL", "NSH", "NJD", "NYI", "NYR",
      "OTT", "PHI", "PIT", "SJS", "SEA", "STL", "TBL", "TOR", "VAN", "VGK",
      "WSH", "WPG"
    )

    # Fetch rosters (this will take time)
    rosters <- fetch_multiple_rosters(team_ids, seasons)

    # Process the roster data to get birth dates
    rosters.1 <- rosters %>% dplyr::select(id, birthDate)
    rosters.2 <- rosters.1 %>% dplyr::group_by(id) %>% dplyr::summarize(birthDate = first(birthDate))

    # Save for future use
    write.csv(rosters.2, "data-raw/nhl_birthdays.csv", row.names = FALSE)
  }

  # Join player info with birth dates
  nhl.final_with_birthday <- nhl.combined %>%
    dplyr::left_join(rosters.2, by = c("player_id" = "id"))

  # Calculate age for each season observation
  nhl.final <- nhl.final_with_birthday %>%
    dplyr::filter(!is.na(birthDate)) %>%
    dplyr::mutate(
      birth_year = as.numeric(format(as.Date(birthDate), "%Y")),  # Extract birth year
      season_start_year = as.numeric(substr(season, 1, 4)),  # Extract season start year
      age = season_start_year - birth_year,  # Calculate age at start of season
      position_value = ifelse(!is.na(position_value), position_value, 0)
    ) %>%
    dplyr::select(-birth_year)  # Clean up temporary column

  # Save final data
  write.csv(nhl.final, "data-raw/nhl_final.csv", row.names = FALSE)

  # Prepare output
  if (!is.null(output_path)) {
    # Create directory if it doesn't exist
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE)
    }
    write.csv(nhl.final, file.path(output_path, "nhl_tidy.csv"), row.names = FALSE)
    message("NHL data saved to ", file.path(output_path, "nhl_tidy.csv"))
    return(invisible(file.path(output_path, "nhl_tidy.csv")))
  } else {
    return(nhl.final)
  }
}
