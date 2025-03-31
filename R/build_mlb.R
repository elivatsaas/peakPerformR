#' Build MLB Player Data
#'
#' @description
#' Builds a dataset of MLB players with performance data from baseballr.
#' By default, uses pre-packaged data included with the package. When use_cached=FALSE,
#' fetches and processes data from APIs, which may take significant time.
#'
#' @param output_path Character string. Optional path to save output CSV file.
#' @param use_cached Logical. Whether to use cached data (TRUE) or rebuild from APIs (FALSE).
#' @param years Integer vector. Years to pull data for. Default is 1994:2024.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A data frame containing MLB player data if output_path is NULL.
#'         Otherwise, writes to the specified path and returns that path invisibly.
#'
#' @importFrom dplyr select rename mutate filter group_by ungroup summarize bind_rows left_join distinct
#' @importFrom purrr map_df map2
#' @importFrom tidyr unnest
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # Get pre-packaged data
#' mlb_data <- build_mlb()
#'
#' # Rebuild data from APIs (takes time)
#' mlb_data_fresh <- build_mlb(use_cached = FALSE)
#'
#' # Rebuild data for specific years
#' mlb_data_recent <- build_mlb(use_cached = FALSE, years = 2020:2023)
#'
#' # Save to specific location
#' build_mlb(output_path = "path/to/save/directory")
#' }
#'
#' @export
build_mlb <- function(output_path = NULL, use_cached = TRUE, years = 1994:2024, ...) {
  # First try package data if use_cached is TRUE
  if(use_cached) {
    mlb_data <- tryCatch({
      # Try to load package data
      mlb_tidy <- NULL
      data("mlb_tidy", envir = environment())
      mlb_tidy
    }, error = function(e) {
      # If loading package data fails, try to load from CSV
      csv_path <- system.file("extdata", "mlb_final.csv", package = "yourpackage")
      if (csv_path == "") {
        warning("Could not find cached MLB data. Attempting to rebuild from APIs.")
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
    if (!is.null(mlb_data)) {
      if (!is.null(output_path)) {
        # Create directory if it doesn't exist
        if (!dir.exists(output_path)) {
          dir.create(output_path, recursive = TRUE)
        }
        write.csv(mlb_data, file.path(output_path, "mlb_tidy.csv"), row.names = FALSE)
        message("MLB data saved to ", file.path(output_path, "mlb_tidy.csv"))
        return(invisible(file.path(output_path, "mlb_tidy.csv")))
      } else {
        return(mlb_data)
      }
    }
  }

  # If we get here, either use_cached was FALSE or we couldn't load cached data
  message("Building MLB data from APIs. This may take significant time...")

  # Check for required package
  if (!requireNamespace("baseballr", quietly = TRUE)) {
    stop("Package 'baseballr' is required for building MLB data. Please install it with install.packages('baseballr').")
  }

  # Create data-raw directory if it doesn't exist
  if (!dir.exists("data-raw")) {
    dir.create("data-raw", recursive = TRUE)
  }

  # Fetch data for all batters while error handling
  message("Fetching batter data...")
  fg_war_batters <- purrr::map_df(years, ~{
    tryCatch({
      # Add delay to avoid rate limiting
      Sys.sleep(1)  # 1 second between requests

      # Fetch qualified batters for this year
      baseballr::fg_batter_leaders(
        startseason = .x,
        endseason = .x
      ) %>%
        dplyr::mutate(Season = .x)  # Add season column
    }, error = function(e) {
      message(paste("Error for year", .x, ":", e$message))
      return(NULL)  # Skip failed years
    })
  })

  # Fetch fielding data
  message("Fetching fielder data...")
  fielding_data <- purrr::map_df(years, ~{
    tryCatch({
      # Add delay to avoid rate limiting
      Sys.sleep(1)

      # Fetch fielding data
      baseballr::fg_fielder_leaders(
        startseason = .x,
        endseason = .x
      ) %>%
        dplyr::mutate(Season = .x)
    }, error = function(e) {
      message(paste("Error for year", .x, ":", e$message))
      return(NULL)
    })
  })

  # Fetch pitcher data
  message("Fetching pitcher data...")
  fg_war_pitchers <- purrr::map_df(years, ~{
    tryCatch({
      Sys.sleep(1)  # Stop from hitting rate limit

      baseballr::fg_pitcher_leaders(
        startseason = .x,
        endseason = .x
      ) %>%
        dplyr::mutate(Season = .x)  # Add season column
    }, error = function(e) {
      message(paste("Pitcher error for", .x, ":", e$message))
      return(NULL)
    })
  })

  # Save intermediate files
  write.csv(fg_war_batters, file = "data-raw/games_batters.csv", row.names = FALSE)
  write.csv(fielding_data, file = "data-raw/games_fielders.csv", row.names = FALSE)
  write.csv(fg_war_pitchers, file = "data-raw/games_pitchers.csv", row.names = FALSE)

  # Identify players who appear in both batter and pitcher datasets
  message("Identifying two-way players...")

  # Create combined dataset with position indicators
  combined_war_data <- dplyr::bind_rows(
    fg_war_batters %>% dplyr::mutate(position = "B"),
    fg_war_pitchers %>% dplyr::mutate(position = "P")
  )

  # Find player-years where a player was both a batter and pitcher
  batter_pitcher_ids <- combined_war_data %>%
    dplyr::group_by(playerid, Season) %>%
    dplyr::filter(any(position == "B") & any(position == "P")) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(playerid, Season)

  write.csv(batter_pitcher_ids, file = "data-raw/batter_pitcher_ids.csv", row.names = FALSE)

  # Function to retrieve game logs for a player-year combo with error handling
  get_all_game_logs <- function(playerid, Season) {
    tryCatch({
      # Fetch all batter game logs
      batter_logs <- baseballr::fg_batter_game_logs(playerid = playerid, year = Season)

      # Fetch all pitcher game logs
      pitcher_logs <- baseballr::fg_pitcher_game_logs(playerid = playerid, year = Season)

      # Combine logs and add playerid for reference
      dplyr::bind_rows(
        batter_logs %>% dplyr::mutate(position = "B"),
        pitcher_logs %>% dplyr::mutate(position = "P")
      ) %>%
        dplyr::mutate(playerid = playerid)
    }, error = function(e) {
      message("Error fetching data for player ", playerid, ": ", e$message)
      return(NULL)  # Return NULL if there's an error
    })
  }

  # Retrieve game-level data for all player-season combos (two-way players)
  if (nrow(batter_pitcher_ids) > 0) {
    message("Retrieving game logs for two-way players...")
    all_games <- batter_pitcher_ids %>%
      dplyr::mutate(data = purrr::map2(
        playerid,
        Season,
        ~ {
          Sys.sleep(1)
          get_all_game_logs(.x, .y) %>%
            dplyr::select(-playerid)  # Remove duplicate playerid from nested data
        }
      )) %>%
      tidyr::unnest(data)

    write.csv(all_games, file = "data-raw/all_games_batter_pitcher.csv", row.names = FALSE)

    # Count unique games for each player-season
    unique_games <- all_games %>%
      dplyr::group_by(playerid, Season) %>%
      dplyr::summarize(
        GP = n_distinct(Date, Team, Opp),  # Count unique games using Date, Team and Opp
        .groups = "drop"
      )
  } else {
    # Create empty unique_games dataframe if no two-way players found
    message("No two-way players found in the dataset.")
    unique_games <- data.frame(
      playerid = character(0),
      Season = numeric(0),
      GP = numeric(0)
    )
  }

  write.csv(unique_games, file = "data-raw/unique_games_batter_pitcher.csv", row.names = FALSE)

  # Add ID fields for joining
  fg_war_batters$id <- paste(fg_war_batters$playerid, fg_war_batters$Season, sep = "_")
  fg_war_fielders <- fielding_data  # Rename for consistency
  fg_war_fielders$id <- paste(fg_war_fielders$playerid, fg_war_fielders$Season, sep = "_")
  fg_war_pitchers$id <- paste(fg_war_pitchers$playerid, fg_war_pitchers$Season, sep = "_")

  # Process batting data
  batting_value <- fg_war_batters %>%
    dplyr::select(
      playerid, PlayerName, Season, Age, G, PA,
      WAR, RAR, OPS, wOBA, Batting, Defense, Offense, BaseRunning,
      Positional, BB_pct, K_pct, ISO, BABIP, wRC
    ) %>%
    dplyr::rename(
      games_batting = G,
      war_batting = WAR,
    ) %>%
    dplyr::mutate(id = paste(playerid, Season, sep = "_"))

  # Process pitching data
  pitching_value <- fg_war_pitchers %>%
    dplyr::select(
      playerid, PlayerName, Season, Age, G, GS, IP,
      WAR, RAR, ERA, FIP, K_9, BB_9, HR_9, WHIP, K_pct, BB_pct
    ) %>%
    dplyr::rename(
      games_pitching = G,
      starts_pitching = GS,
      war_pitching = WAR,
    ) %>%
    dplyr::mutate(id = paste(playerid, Season, sep = "_"))

  # Process fielding data
  fielding_value <- fg_war_fielders %>%
    dplyr::group_by(playerid, Season) %>%
    dplyr::summarize(
      fielding_games = sum(G, na.rm = TRUE),
      fielding_innings = sum(Inn, na.rm = TRUE),
      fielding_pct = weighted.mean(FP, Inn, na.rm = TRUE),
      primary_position = Pos[which.max(Inn)]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id = paste(playerid, Season, sep = "_"))

  # Process two-way player data
  two_way_games <- unique_games %>%
    dplyr::mutate(id = paste(playerid, Season, sep = "_")) %>%
    dplyr::select(id, GP)

  # Create comprehensive dataset starting with batters
  season_value <- batting_value %>%
    # Left join pitching data
    dplyr::left_join(
      pitching_value %>% dplyr::select(-PlayerName, -Age),
      by = c("id", "playerid", "Season")
    ) %>%
    # Left join fielding data
    dplyr::left_join(
      fielding_value,
      by = c("id", "playerid", "Season")
    ) %>%
    # Left join two-way games data
    dplyr::left_join(
      two_way_games,
      by = "id"
    ) %>%
    # Fill in missing values and identify player types
    dplyr::mutate(
      # Fill NA values for players who don't pitch
      war_pitching = ifelse(is.na(war_pitching), 0, war_pitching),
      games_pitching = ifelse(is.na(games_pitching), 0, games_pitching),

      # Identify two-way players correctly
      is_two_way = !is.na(GP),

      # Determine player type
      player_type = dplyr::case_when(
        is_two_way ~ "Two-Way",
        games_pitching > 0 & games_batting == 0 ~ "Pitcher",
        TRUE ~ "Position Player"
      ),

      # Set total games correctly based on player type
      total_games = dplyr::case_when(
        is_two_way ~ pmax(games_batting, games_pitching, na.rm = TRUE), # Use max for two-way players
        player_type == "Pitcher" ~ games_pitching,
        player_type == "Position Player" ~ games_batting
      ),

      # Create combined season value metrics
      total_war = war_batting + war_pitching,

      # Fill NA values for fielding
      fielding_pct = ifelse(is.na(fielding_pct), 0, fielding_pct)
    )

  # Add pitchers who don't bat
  pitcher_only <- pitching_value %>%
    dplyr::filter(!(id %in% season_value$id)) %>%
    dplyr::mutate(
      # Add default values for batting stats
      war_batting = 0,
      games_batting = 0,

      # Create total values
      total_war = war_pitching,
      total_games = games_pitching,

      # Set player type
      is_two_way = FALSE,
      player_type = "Pitcher"
    )

  # Combine all players
  all_players_season <- dplyr::bind_rows(season_value, pitcher_only)

  # Calculate max games by season and player type
  max_games <- all_players_season %>%
    dplyr::group_by(Season, player_type) %>%
    dplyr::summarize(
      max_games = max(total_games)
    )

  # Join with max games and calculate percentage of season played
  mlb.final <- all_players_season %>%
    dplyr::left_join(max_games, by = c("Season", "player_type")) %>%
    dplyr::mutate(
      pct.season.played = total_games / max_games,
    )

  # Fix player types to be consistent across seasons
  mlb_fixed <- mlb.final %>%
    # First identify player types across all seasons
    dplyr::group_by(playerid) %>%
    dplyr::mutate(
      # Check if player was ever a two-way player in any season
      ever_two_way = any(is_two_way == TRUE, na.rm = TRUE),

      # Check if player was ever a pitcher in any season
      ever_pitcher = any(player_type == "Pitcher", na.rm = TRUE),

      # Apply consistent player type across all seasons
      player_type_consistent = dplyr::case_when(
        ever_two_way ~ "Two-Way",
        ever_pitcher & !ever_two_way ~ "Pitcher",
        TRUE ~ "Position Player"
      )
    ) %>%
    # Replace the original player_type with the consistent one
    dplyr::mutate(
      player_type = player_type_consistent
    ) %>%
    # Remove helper columns
    dplyr::select(-ever_two_way, -ever_pitcher, -player_type_consistent) %>%
    dplyr::ungroup()

  # Save final results
  write.csv(mlb_fixed, "data-raw/mlb_final.csv", row.names = FALSE)

  # Prepare output
  if (!is.null(output_path)) {
    # Create directory if it doesn't exist
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE)
    }
    write.csv(mlb_fixed, file.path(output_path, "mlb_tidy.csv"), row.names = FALSE)
    message("MLB data saved to ", file.path(output_path, "mlb_tidy.csv"))
    return(invisible(file.path(output_path, "mlb_tidy.csv")))
  } else {
    return(mlb_fixed)
  }
}
