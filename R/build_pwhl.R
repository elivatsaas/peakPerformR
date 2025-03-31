#' Build PWHL Player Data
#'
#' @description
#' Builds a dataset of PWHL (Professional Women's Hockey League) players with performance data from
#' the fastRhockey package. By default, uses pre-packaged data included with the package.
#' When use_cached=FALSE, fetches and processes data from APIs.
#'
#' @param output_path Character string. Optional path to save output CSV file.
#' @param use_cached Logical. Whether to use cached data (TRUE) or rebuild from APIs (FALSE).
#' @param seasons Integer vector. Seasons to pull data for. Default is 2016:2024.
#' @param scrape_birthdays Logical. Whether to scrape player birthdays from EliteProspects
#'        when missing (requires rvest package). Default is FALSE.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A data frame containing PWHL player data if output_path is NULL.
#'         Otherwise, writes to the specified path and returns that path invisibly.
#'
#' @importFrom fastRhockey load_phf_player_box
#' @importFrom dplyr mutate filter select rename left_join inner_join group_by summarize ungroup arrange distinct slice sym case_when row_number n across everything
#' @importFrom lubridate interval as.period year
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # Get pre-packaged data
#' pwhl_data <- build_pwhl()
#'
#' # Rebuild data from APIs (takes time)
#' pwhl_data_fresh <- build_pwhl(use_cached = FALSE)
#'
#' # Rebuild data for specific seasons
#' pwhl_data_recent <- build_pwhl(use_cached = FALSE, seasons = 2020:2024)
#'
#' # Rebuild data and scrape missing birthdays
#' pwhl_data_complete <- build_pwhl(use_cached = FALSE, scrape_birthdays = TRUE)
#'
#' # Save to specific location
#' build_pwhl(output_path = "path/to/save/directory")
#' }
#'
#' @export
build_pwhl <- function(output_path = NULL, use_cached = TRUE, seasons = 2016:2024,
                       scrape_birthdays = FALSE, ...) {
  # Helper functions to handle time formatting
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
    pwhl_data <- tryCatch({
      # Try to load package data
      pwhl_tidy <- NULL
      data("pwhl_tidy", envir = environment())
      pwhl_tidy
    }, error = function(e) {
      # If loading package data fails, try to load from CSV
      csv_path <- system.file("extdata", "pwhl_final.csv", package = "yourpackage")
      if (csv_path == "") {
        warning("Could not find cached PWHL data. Attempting to rebuild from APIs.")
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
    if (!is.null(pwhl_data)) {
      if (!is.null(output_path)) {
        # Create directory if it doesn't exist
        if (!dir.exists(output_path)) {
          dir.create(output_path, recursive = TRUE)
        }
        write.csv(pwhl_data, file.path(output_path, "pwhl_tidy.csv"), row.names = FALSE)
        message("PWHL data saved to ", file.path(output_path, "pwhl_tidy.csv"))
        return(invisible(file.path(output_path, "pwhl_tidy.csv")))
      } else {
        return(pwhl_data)
      }
    }
  }

  # If we get here, either use_cached was FALSE or we couldn't load cached data
  message("Building PWHL data from APIs. This may take significant time...")

  # Check for required package
  if (!requireNamespace("fastRhockey", quietly = TRUE)) {
    stop("Package 'fastRhockey' is required for building PWHL data. Please install it with install.packages('fastRhockey').")
  }

  # Create data-raw directory if it doesn't exist
  if (!dir.exists("data-raw")) {
    dir.create("data-raw", recursive = TRUE)
  }

  # Load player box score data
  message("Loading PWHL/PHF player box score data...")
  pwhl.player.games <- fastRhockey::load_phf_player_box(seasons = seasons)

  # Define position mapping
  position_mapping <- c(
    "D" = "Defense",
    "F" = "Forward",
    "F/D" = "Forward",
    "P" = "Forward",
    "G" = "Goalie"
  )

  # Add a new column for the grouped positions
  pwhl.player.games <- pwhl.player.games %>%
    dplyr::mutate(position_type = position_mapping[position])

  # Split players by position type
  skaters <- pwhl.player.games %>%
    dplyr::filter(position_type == "Forward" | position_type == "Defense")

  goalies <- pwhl.player.games %>%
    dplyr::filter(position_type == "Goalie")

  # Calculate fantasy points for skaters
  skaters.fantasy <- skaters %>%
    dplyr::mutate(
      fantasy_points = (3 * goals) + (2 * assists) +
        (0.5 * shots_on_goal) + (0.5 * blocks) +
        (0.5 * (powerplay_goals)) +
        (0.5 * (shorthanded_goals)) + (0.5 * takeaways)
    )

  # Calculate fantasy points for goalies
  goalies.fantasy <- goalies %>%
    dplyr::mutate(
      shutout = ifelse(save_percent == 100, 1, 0),
      fantasy_points = (-1* ((1-save_percent) * shots_against)) +
        (0.2 * (saves)) + (2 * shutout)
    )

  # Aggregate skater data to season level
  skaters.season <- skaters.fantasy %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(
      fantasy_points_season = sum(fantasy_points, na.rm = TRUE),
      player_full_name = first(player_full_name),
      games_played = n_distinct(game_id),
      position = first(position_type),
      goals = sum(goals, na.rm = TRUE),
      assists = sum(assists, na.rm = TRUE),
      points = sum(points, na.rm = TRUE),
      penalty_minutes = sum(penalty_minutes, na.rm = TRUE),
      shots_on_goal = sum(shots_on_goal, na.rm = TRUE),
      blocks = sum(blocks, na.rm = TRUE),
      giveaways = sum(giveaways, na.rm = TRUE),
      takeaways = sum(takeaways, na.rm = TRUE),
      powerplay_goals = sum(powerplay_goals, na.rm = TRUE),
      shorthanded_goals = sum(shorthanded_goals, na.rm = TRUE),
      shots = sum(shots, na.rm = TRUE),
      shots_blocked = sum(shots_blocked, na.rm = TRUE),
      faceoffs_won = sum(faceoffs_won, na.rm = TRUE),
      faceoffs_lost = sum(faceoffs_lost, na.rm = TRUE)
    )

  # Add per-game metrics for skaters
  skaters.season <- skaters.season %>%
    dplyr::mutate(
      fpts.game = fantasy_points_season / games_played,
      fpts.season = fpts.game * 30, # 30 games in PWHL season
      pct.season.played = games_played / 30,
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
      goals = sum(goals, na.rm = TRUE),
      penalty_minutes = sum(penalty_minutes, na.rm = TRUE),
      minutes_played = sum((period_to_seconds(ms(minutes_played))), na.rm = TRUE),
      shots_against = sum(shots_against, na.rm = TRUE),
      goals_against = sum(goals_against, na.rm = TRUE),
      saves = sum(saves, na.rm = TRUE),
      save_percent = sum(save_percent, na.rm = TRUE),
      shutout = sum(shutout, na.rm = TRUE),
      fantasy_points = sum(fantasy_points, na.rm = TRUE)
    )

  # Add per-game metrics for goalies
  goalies.season <- goalies.season %>%
    dplyr::mutate(
      fpts.game = fantasy_points_season / games_played,
      fpts.season = fpts.game * 30, # 30 games in PWHL season
      pct.season.played = games_played / 30,
    ) %>%
    # Handle cases where games_played = 0 to avoid division errors
    dplyr::mutate(
      fpts.game = ifelse(games_played == 0, NA_real_, fpts.game),
      fpts.season = ifelse(games_played == 0, NA_real_, fpts.season),
      pct.season.played = ifelse(games_played == 0, NA_real_, pct.season.played),
      pct.season.played = ifelse(pct.season.played > 1, 1, pct.season.played)
    )

  # Calculate advanced per-game metrics for skaters
  skaters.normalized <- skaters.season %>%
    dplyr::mutate(
      # Offensive stats
      goals_per_game = goals / games_played,
      assists_per_game = assists / games_played,
      points_per_game = points / games_played,
      shots_per_game = shots / games_played,
      shots_on_goal_per_game = shots_on_goal / games_played,

      # Special teams
      pp_goals_per_game = powerplay_goals / games_played,
      sh_goals_per_game = shorthanded_goals / games_played,

      # Defensive/physical stats
      blocks_per_game = blocks / games_played,
      takeaways_per_game = takeaways / games_played,
      giveaways_per_game = giveaways / games_played,
      pim_per_game = penalty_minutes / games_played,
      shots_blocked_per_game = shots_blocked / games_played,

      # Faceoff metrics
      faceoffs_taken = faceoffs_won + faceoffs_lost,
      faceoff_win_pct = ifelse(faceoffs_taken > 0,
                               faceoffs_won / faceoffs_taken * 100,
                               NA_real_)
    )

  # Create position-specific value metrics for skaters
  skaters.with.metrics <- skaters.normalized %>%
    dplyr::mutate(
      # Forward Value Metric (FVM)
      position_value = dplyr::case_when(
        position == "Forward" ~ (
          (goals_per_game * 5) +             # Goals are highly valuable
            (assists_per_game * 3) +           # Assists are valuable but less than goals
            (shots_on_goal_per_game * 0.5) +   # Shot generation has value
            (pp_goals_per_game * 2) +          # Power play goals add extra value
            (sh_goals_per_game * 3) +          # Shorty goals are rare and valuable
            (takeaways_per_game * 1) +         # Good defensive play
            (giveaways_per_game * -0.5) +      # Negative for turnovers
            (blocks_per_game * 0.5) +          # Shot blocking has some value for forwards
            # Add faceoff component only if player takes faceoffs
            (ifelse(faceoffs_taken > 100,
                    (faceoff_win_pct - 50) * 0.05,
                    0))
        ),
        # Defenseman Value Metric (DVM)
        position == "Defense" ~ (
          (assists_per_game * 3) +           # Assists are important for defensemen
            (goals_per_game * 4) +             # Goals are valuable but less common for D
            (blocks_per_game * 2) +            # Shot blocking is crucial
            (shots_blocked_per_game * 2) +     # Another blocking metric
            (takeaways_per_game * 1.5) +       # Good defensive play
            (giveaways_per_game * -1) +        # Negative for turnovers
            (pp_goals_per_game * 2) +          # Power play contributions
            (shots_per_game * 0.3)             # Shot generation from blue line
        ),
        TRUE ~ NA_real_
      )
    )

  # Calculate advanced metrics for goalies
  goalies.prepped <- goalies.season %>%
    dplyr::mutate(
      # Per-game statistics
      minutes_per_game = minutes_played / games_played,
      saves_per_game = saves / games_played,
      shots_against_per_game = shots_against / games_played,

      # Calculate goals against average if not already present
      goals_against_avg = (goals_against * 60) / minutes_played,

      # Win ratio approximation (using fantasy points as proxy if wins not directly available)
      # Typically, wins contribute significantly to fantasy points in hockey
      estimated_wins = fantasy_points_season / 4,  # Approximate conversion
      win_pct = estimated_wins / games_played,

      # Quality start calculation
      # A quality start is when save percentage is better than league average
      quality_start_threshold = 0.915,
      is_quality_start = save_percent >= quality_start_threshold,

      # Goals saved above average (GSAA)
      # This compares a goalie to league average performance
      league_avg_save_pct = 0.910, # Approximate league average
      expected_goals_against = shots_against * (1 - league_avg_save_pct),
      gsaa = expected_goals_against - goals_against
    )

  # Create goalie value metric
  goalies.with.metrics <- goalies.prepped %>%
    dplyr::mutate(
      # Goalie Value Metric (GVM)
      position_value = (
        # Base save percentage (most important factor)
        (save_percent * 1000 - 900) * 2 +  # Transform from ~.900-.930 to ~0-60 range

          # Goals saved above average (per game played)
          (gsaa / games_played) * 5 +

          # Win percentage (contributes significantly)
          (win_pct * 20) +

          # Shutout bonus
          (shutout / games_played) * 15 +

          # Workload adjustment (small bonus for handling high shot volumes)
          (shots_against_per_game - 30) * 0.1
      )
    )

  # Combine skaters and goalies
  pwhl.combined <- dplyr::bind_rows(skaters.with.metrics, goalies.with.metrics)

  # Add player name information
  pwhl.combined <- pwhl.combined %>%
    dplyr::rename(player_name = player_full_name)

  # Handle missing birthdays - either load cached or scrape new data
  if (file.exists("data-raw/pwhl_players_with_ages.csv")) {
    message("Using cached PWHL player birthdays from data-raw/pwhl_players_with_ages.csv")
    birthdays <- read.csv("data-raw/pwhl_players_with_ages.csv")
  } else if (scrape_birthdays) {
    message("No player birth data file found. Attempting to scrape from EliteProspects...")
    # Check if required packages are installed
    if (!requireNamespace("rvest", quietly = TRUE) ||
        !requireNamespace("stringr", quietly = TRUE) ||
        !requireNamespace("readr", quietly = TRUE)) {
      stop("Packages 'rvest', 'stringr', and 'readr' are required for scraping player data. Please install them.")
    }

    # Get unique player names for scraping
    player_names <- unique(pwhl.combined$player_name)

    # Create data-raw directory if it doesn't exist
    if (!dir.exists("data-raw")) {
      dir.create("data-raw", recursive = TRUE)
    }

    # Function to scrape roster data directly from HTML elements
    scrape_team_roster <- function(url) {
      message(paste("Scraping team roster:", url))

      # Try to read the page
      page <- tryCatch({
        rvest::read_html(url)
      }, error = function(e) {
        message(paste("Error reading page:", e$message))
        return(NULL)
      })

      if (is.null(page)) {
        return(NULL)
      }

      # Extract team name from URL
      team_name <- stringr::str_extract(url, "/team/\\d+/([^/]+)")
      team_name <- stringr::str_replace(team_name, "/team/\\d+/", "")
      team_name <- stringr::str_replace_all(team_name, "-", " ")
      team_name <- stringr::str_to_title(team_name)

      # Extract season from URL
      season <- stringr::str_extract(url, "\\d{4}-\\d{4}")
      if (is.na(season)) {
        # Default to current season
        current_year <- as.numeric(format(Sys.Date(), "%Y"))
        season <- paste0(current_year - 1, "-", current_year)
      }

      # Direct HTML extraction approach - find player rows
      player_rows <- page %>% rvest::html_nodes("tr")

      # Create empty data frame to store results
      roster_data <- data.frame(
        player_name = character(),
        position = character(),
        birth_year = numeric(),
        team = character(),
        season = character(),
        league = character(),
        team_url = character(),
        stringsAsFactors = FALSE
      )

      # Process each row to find player data
      for (row in player_rows) {
        # Look for player links
        player_link <- row %>% rvest::html_nodes("a") %>%
          .[grep("/player/", rvest::html_attr(., "href"))]

        if (length(player_link) > 0) {
          # This row likely contains a player
          player_name <- rvest::html_text(player_link[1], trim = TRUE)

          # Check for position in parentheses
          position <- NA
          pos_match <- stringr::str_extract(player_name, "\\([^)]+\\)")
          if (!is.na(pos_match)) {
            position <- stringr::str_replace_all(pos_match, "[\\(\\)]", "")
            player_name <- stringr::str_replace(player_name, "\\s*\\([^)]+\\)", "")
          }

          # Look for birth year in cells
          cells <- row %>% rvest::html_nodes("td")
          birth_year <- NA

          if (length(cells) > 2) {
            for (cell in cells) {
              cell_text <- rvest::html_text(cell, trim = TRUE)
              year_match <- stringr::str_extract(cell_text, "\\b(19|20)\\d{2}\\b")
              if (!is.na(year_match)) {
                birth_year <- as.numeric(year_match)
                break
              }
            }
          }

          # Add to roster data
          roster_data <- rbind(roster_data, data.frame(
            player_name = player_name,
            position = position,
            birth_year = birth_year,
            team = team_name,
            season = season,
            league = if(grepl("PWHL|Fleet|Frost|Victoire|Sirens|Charge|Sceptres", team_name)) {
              "PWHL"
            } else if(as.numeric(substr(season, 1, 4)) >= 2021) {
              "PHF"
            } else {
              "NWHL"
            },
            team_url = url,
            stringsAsFactors = FALSE
          ))
        }
      }

      # If we found players, calculate ages
      if (nrow(roster_data) > 0) {
        roster_data$age <- ifelse(!is.na(roster_data$birth_year),
                                  as.numeric(format(Sys.Date(), "%Y")) - roster_data$birth_year,
                                  NA)
      } else {
        message("No players found on page")
        return(NULL)
      }

      return(roster_data)
    }

    # Define team URLs to scrape
    # PWHL teams (2023-2024)
    pwhl_teams <- c(
      "https://www.eliteprospects.com/team/37963/boston-fleet/2023-2024",
      "https://www.eliteprospects.com/team/37965/minnesota-frost/2023-2024",
      "https://www.eliteprospects.com/team/37967/montreal-victoire/2023-2024",
      "https://www.eliteprospects.com/team/37964/new-york-sirens/2023-2024",
      "https://www.eliteprospects.com/team/37966/ottawa-charge/2023-2024",
      "https://www.eliteprospects.com/team/37968/toronto-sceptres/2023-2024"
    )

    # PHF Teams (formerly NWHL) - just the last few seasons
    phf_teams <- c(
      "https://www.eliteprospects.com/team/19354/boston-pride/2022-2023",
      "https://www.eliteprospects.com/team/19355/buffalo-beauts/2022-2023",
      "https://www.eliteprospects.com/team/19356/connecticut-whale/2022-2023",
      "https://www.eliteprospects.com/team/19357/metropolitan-riveters/2022-2023",
      "https://www.eliteprospects.com/team/19483/minnesota-whitecaps/2022-2023",
      "https://www.eliteprospects.com/team/30498/toronto-six/2022-2023",
      "https://www.eliteprospects.com/team/66674/montreal-force/2022-2023"
    )

    # Combine team URLs
    all_team_urls <- c(pwhl_teams, phf_teams)

    # Initialize list to store roster data
    all_rosters <- list()

    # Scrape team rosters
    for (i in seq_along(all_team_urls)) {
      url <- all_team_urls[i]
      roster <- tryCatch({
        scrape_team_roster(url)
      }, error = function(e) {
        message(paste("Error processing", url, ":", e$message))
        NULL
      })

      if (!is.null(roster) && nrow(roster) > 0) {
        all_rosters[[length(all_rosters) + 1]] <- roster
      }

      # Be nice to the server
      Sys.sleep(5)
    }

    # Combine all roster data
    if (length(all_rosters) > 0) {
      womens_hockey_rosters <- dplyr::bind_rows(all_rosters)

      # Save scraped data
      readr::write_csv(womens_hockey_rosters, "data-raw/womens_hockey_rosters.csv")

      # Process the combined roster data to create a player birthdate dataset
      player_birth_years <- womens_hockey_rosters %>%
        # Keep only the most essential columns
        dplyr::select(player_name, birth_year, age, position) %>%
        # Remove any rows with missing player names
        dplyr::filter(!is.na(player_name)) %>%
        # Sort by player name and birth year
        dplyr::arrange(player_name, birth_year) %>%
        # Keep only one entry per player (the one with birth year data if available)
        dplyr::group_by(player_name) %>%
        dplyr::slice(which.max(!is.na(birth_year))) %>%
        dplyr::ungroup()

      # Match with our player list
      matched_data <- data.frame(player_name = player_names) %>%
        dplyr::left_join(player_birth_years, by = "player_name")

      # Create a data frame with player IDs from our combined dataset
      pwhl.players_with_ages <- pwhl.combined %>%
        dplyr::distinct(player_id, player_name) %>%
        dplyr::left_join(matched_data, by = "player_name") %>%
        dplyr::select(player_id, birth_year, player_name)

      # Save for future use
      readr::write_csv(pwhl.players_with_ages, "data-raw/pwhl_players_with_ages.csv")

      birthdays <- pwhl.players_with_ages

      message("Successfully scraped birth year data for ", sum(!is.na(birthdays$birth_year)),
              " out of ", nrow(birthdays), " players.")
    } else {
      message("No roster data was successfully scraped.")
      # Create a minimal birthdays dataframe with player_ids
      birthdays <- pwhl.combined %>%
        dplyr::distinct(player_id, player_name) %>%
        dplyr::mutate(birth_year = NA)
      readr::write_csv(birthdays, "data-raw/pwhl_players_with_ages.csv")
      warning("Created empty birthdays file at data-raw/pwhl_players_with_ages.csv. Please populate with birth_year values.")
    }
  } else {
    # Create a minimal birthdays dataframe with player_ids
    message("No player birth data file found and scrape_birthdays=FALSE. Creating template file.")
    birthdays <- pwhl.combined %>%
      dplyr::distinct(player_id, player_name) %>%
      dplyr::mutate(birth_year = NA)
    write.csv(birthdays, "data-raw/pwhl_players_with_ages.csv", row.names = FALSE)
    warning("Created template birth year file at data-raw/pwhl_players_with_ages.csv. Please populate with birth_year values or run again with scrape_birthdays=TRUE.")
  }

  # Join with birthdays and calculate ages
  pwhl.final <- pwhl.combined %>%
    dplyr::mutate(
      player_id = as.integer(player_id)
    ) %>%
    dplyr::left_join(birthdays, by = c("player_id", "player_name"))

  # Filter out rows with missing birth years with a warning
  missing_birth_years <- sum(is.na(pwhl.final$birth_year))
  if (missing_birth_years > 0) {
    warning(paste(missing_birth_years, "players are missing birth year data. These will be filtered out."))
    pwhl.final <- pwhl.final %>%
      dplyr::filter(!is.na(birth_year))
  }

  if (nrow(pwhl.final) == 0) {
    stop("No players with valid birth year data found. Please populate data-raw/pwhl_players_with_ages.csv with birth_year values or run with scrape_birthdays=TRUE.")
  }

  # Calculate ages
  pwhl.final <- pwhl.final %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(
      # Calculate age at season start
      age = season - birth_year
    )

  # Save the final dataset
  write.csv(pwhl.final, "data-raw/pwhl_final.csv", row.names = FALSE)

  # Prepare output
  if (!is.null(output_path)) {
    # Create directory if it doesn't exist
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE)
    }
    write.csv(pwhl.final, file.path(output_path, "pwhl_tidy.csv"), row.names = FALSE)
    message("PWHL data saved to ", file.path(output_path, "pwhl_tidy.csv"))
    return(invisible(file.path(output_path, "pwhl_tidy.csv")))
  } else {
    return(pwhl.final)
  }
}
