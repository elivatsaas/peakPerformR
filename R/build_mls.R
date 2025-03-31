#' Build MLS Player Data
#'
#' @description
#' Builds a dataset of MLS (Major League Soccer) players with performance data from
#' the American Soccer Analysis API. By default, uses pre-packaged data included
#' with the package. When use_cached=FALSE, fetches and processes data from APIs.
#'
#' @param output_path Character string. Optional path to save output CSV file.
#' @param use_cached Logical. Whether to use cached data (TRUE) or rebuild from APIs (FALSE).
#' @param start_year Integer. First year to fetch data for when rebuilding. Default is 2013.
#' @param end_year Integer. Last year to fetch data for when rebuilding. Default is current year.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A data frame containing MLS player data if output_path is NULL.
#'         Otherwise, writes to the specified path and returns that path invisibly.
#'
#' @importFrom dplyr select inner_join filter mutate group_by summarize ungroup rename bind_rows
#' @importFrom lubridate ymd interval year as.period
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # Get pre-packaged data
#' mls_data <- build_mls()
#'
#' # Rebuild data from APIs (takes time)
#' mls_data_fresh <- build_mls(use_cached = FALSE)
#'
#' # Get data for specific years
#' mls_data_recent <- build_mls(use_cached = FALSE, start_year = 2020, end_year = 2023)
#'
#' # Save to specific location
#' build_mls(output_path = "path/to/save/directory")
#' }
#'
#' @export
build_mls <- function(output_path = NULL, use_cached = TRUE, start_year = 2013,
                      end_year = as.integer(format(Sys.Date(), "%Y")), ...) {
  # First try package data if use_cached is TRUE
  if(use_cached) {
    mls_data <- tryCatch({
      # Try to load package data
      mls_tidy <- NULL
      data("mls_tidy", envir = environment())
      mls_tidy
    }, error = function(e) {
      # If loading package data fails, try to load from CSV
      csv_path <- system.file("extdata", "mls_final.csv", package = "yourpackage")
      if (csv_path == "") {
        warning("Could not find cached MLS data. Attempting to rebuild from APIs.")
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
    if (!is.null(mls_data)) {
      if (!is.null(output_path)) {
        # Create directory if it doesn't exist
        if (!dir.exists(output_path)) {
          dir.create(output_path, recursive = TRUE)
        }
        write.csv(mls_data, file.path(output_path, "mls_tidy.csv"), row.names = FALSE)
        message("MLS data saved to ", file.path(output_path, "mls_tidy.csv"))
        return(invisible(file.path(output_path, "mls_tidy.csv")))
      } else {
        return(mls_data)
      }
    }
  }

  # If we get here, either use_cached was FALSE or we couldn't load cached data
  message("Building MLS data from APIs. This may take some time...")

  # Check for required package
  if (!requireNamespace("itscalledsoccer", quietly = TRUE)) {
    stop("Package 'itscalledsoccer' is required. Please install it with install.packages('itscalledsoccer').")
  }

  # Create data-raw directory if it doesn't exist
  if (!dir.exists("data-raw")) {
    dir.create("data-raw", recursive = TRUE)
  }

  # Initialize client
  asa_client <- itscalledsoccer::AmericanSoccerAnalysis$new()

  # Initialize an empty list to store results
  all_seasons_data <- list()

  # Loop through each year and attempt to fetch player data
  for (year in start_year:end_year) {
    # Define the start and end dates for the season
    start_date <- paste0(year, "-02-15")
    end_date <- paste0(year, "-11-15")

    # Try to fetch player data for the season
    tryCatch({
      # Get player goals added data
      player_ga <- asa_client$get_player_goals_added(
        leagues = "mls",
        start_date = start_date,
        end_date = end_date,
        above_replacement = TRUE
      )

      # Get player xGoals data
      player_xg <- asa_client$get_player_xgoals(
        leagues = "mls",
        start_date = start_date,
        end_date = end_date
      )

      # Get player passing data
      player_xpass <- asa_client$get_player_xpass(
        leagues = "mls",
        start_date = start_date,
        end_date = end_date
      )

      # Combine datasets
      player_data <- player_ga %>%
        dplyr::select(
          player_id, team_id, general_position, minutes_played,
          goals_added_raw, goals_added_above_replacement, count_actions
        ) %>%
        dplyr::inner_join(
          player_xg %>% dplyr::select(
            player_id, xgoals, goals, goals_minus_xgoals,
            xassists, primary_assists, goals_plus_primary_assists,
            points_added, xpoints_added
          ),
          by = "player_id"
        ) %>%
        dplyr::inner_join(
          player_xpass %>% dplyr::select(
            player_id, pass_completion_percentage,
            passes_completed_over_expected_p100, share_team_touches
          ),
          by = "player_id"
        )

      # Add a column for the season
      player_data$season <- year

      # Append the data to the list
      all_seasons_data[[as.character(year)]] <- player_data

      message("Successfully fetched field player data for ", year)
    }, error = function(e) {
      message("No field player data found for ", year, ": ", conditionMessage(e))
    })
  }

  # Process team IDs to ensure consistent format
  for (year in names(all_seasons_data)) {
    all_seasons_data[[year]] <- all_seasons_data[[year]] %>%
      dplyr::mutate(team_id = as.character(team_id))
  }

  # Combine all seasons' data into a single data frame
  combined_data <- dplyr::bind_rows(all_seasons_data, .id = "season")

  # Calculate normalized per-96 metrics for player comparisons
  player_metrics <- combined_data %>%
    dplyr::mutate(
      # Goals-related metrics per 96 minutes
      goals_added_per_96 = goals_added_raw / minutes_played * 96,
      goals_per_96 = goals / minutes_played * 96,
      xgoals_per_96 = xgoals / minutes_played * 96,

      # Assists metrics per 96 minutes
      assists_per_96 = primary_assists / minutes_played * 96,
      xassists_per_96 = xassists / minutes_played * 96,

      # Overall contribution metrics
      g_plus_a_per_96 = goals_plus_primary_assists / minutes_played * 96,
      points_added_per_96 = points_added / minutes_played * 96,

      # Passing metrics (already normalized)
      pass_value = passes_completed_over_expected_p100,

      # Position categories for appropriate comparison
      position_group = dplyr::case_when(
        general_position == "GK" ~ "GK",
        general_position %in% c("CB") ~ "DEF",
        general_position %in% c("FB") ~ "DEF",
        general_position %in% c("DM") ~ "DEF",
        general_position %in% c("CM") ~ "MID",
        general_position %in% c("AM", "W") ~ "MID",
        general_position %in% c("ST") ~ "FWD",
        TRUE ~ "OTHER"
      )
    )

  # Calculate percentile rankings for key metrics
  player_scores <- player_metrics %>%
    dplyr::group_by(position_group) %>%
    dplyr::mutate(
      # Core metrics as percentiles (0-100)
      ga_score = dplyr::ntile(goals_added_per_96, 100),
      finishing_score = dplyr::ntile(goals_minus_xgoals, 100),
      creation_score = dplyr::ntile(xassists_per_96, 100),
      passing_score = dplyr::ntile(pass_value, 100),
    ) %>%
    dplyr::ungroup()

  # Create weighted overall player quality score
  player_scores <- player_scores %>%
    dplyr::mutate(
      # Different weights for different positions
      player_quality_score = (ga_score*0.4 + passing_score*0.2 +
                                creation_score*0.2 + finishing_score*0.2),
      goals_above_replacement_per_96 = goals_added_above_replacement / minutes_played * 96,
    )

  # Summarize to season-level data
  summary_data <- player_scores %>%
    dplyr::group_by(season, player_id) %>%
    dplyr::summarize(
      player_quality_score = mean(player_quality_score, na.rm = TRUE),
      goals_above_replacement_per_96 = mean(goals_above_replacement_per_96, na.rm = TRUE),
      g_plus_a_per_96 = mean(g_plus_a_per_96, na.rm = TRUE),
      minutes_played = first(minutes_played),
      general_position = first(general_position),
      position_group = first(position_group),
      team_id = first(team_id)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      season,
      player_id,
      team_id,
      general_position,
      position_group,
      minutes_played,
      goals_above_replacement_per_96,
      g_plus_a_per_96,
      player_quality_score
    ) %>%
    dplyr::rename(
      position_value = g_plus_a_per_96
    )

  # Now fetch and process goalkeeper data
  goalie_seasons_data <- list()

  # Loop through each year for goalkeeper data
  for (year in start_year:end_year) {
    # Define the season date range
    start_date <- paste0(year, "-02-15")
    end_date <- paste0(year, "-11-15")

    # Try to fetch goalkeeper data for the season
    tryCatch({
      # Get goalkeeper goals added data
      gk_ga <- asa_client$get_goalkeeper_goals_added(
        leagues = "mls",
        start_date = start_date,
        end_date = end_date,
        above_replacement = TRUE
      )

      # Get goalkeeper xGoals data
      gk_xg <- asa_client$get_goalkeeper_xgoals(
        leagues = "mls",
        start_date = start_date,
        end_date = end_date
      )

      # Join the datasets
      gk_data <- gk_ga %>%
        dplyr::select(player_id, team_id, minutes_played, goals_added_raw, goals_added_above_replacement, count_actions) %>%
        dplyr::inner_join(
          gk_xg %>% dplyr::select(
            player_id, shots_faced, goals_conceded, saves,
            xgoals_gk_faced, goals_minus_xgoals_gk, goals_divided_by_xgoals_gk
          ),
          by = "player_id"
        )

      # Add season column
      gk_data$season <- year

      # Append to list
      goalie_seasons_data[[as.character(year)]] <- gk_data

      message("Successfully fetched goalkeeper data for ", year)
    }, error = function(e) {
      message("No goalkeeper data found for ", year, ": ", conditionMessage(e))
    })
  }

  # Standardize team_id format for goalkeepers
  for (year in names(goalie_seasons_data)) {
    if (length(goalie_seasons_data[[year]]) > 0) {
      goalie_seasons_data[[year]] <- goalie_seasons_data[[year]] %>%
        dplyr::mutate(team_id = as.character(team_id))
    }
  }

  # Combine all goalkeeper seasons into a single data frame
  combined_gk_data <- dplyr::bind_rows(goalie_seasons_data, .id = "season")

  # Calculate goalkeeper-specific metrics
  gk_metrics <- combined_gk_data %>%
    dplyr::filter(minutes_played >= 500) %>%  # Minimum minutes threshold
    dplyr::mutate(
      # Calculate save percentage
      save_percentage = saves / shots_faced,

      # Per 96 minutes metrics
      ga_per_96 = goals_added_raw / minutes_played * 96,
      goals_above_replacement_per_96 = goals_added_above_replacement / minutes_played * 96,

      # Goalkeeping specific metrics
      goals_saved_above_expected = -goals_minus_xgoals_gk, # Negative because lower is better for GKs
      goals_saved_above_expected_per_96 = goals_saved_above_expected / minutes_played * 96,

      # Shot stopping metrics
      shots_faced_per_96 = shots_faced / minutes_played * 96,

      # Add position group
      position_group = "GK"
    )

  # Calculate percentile rankings for goalkeepers
  gk_scores <- gk_metrics %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(
      ga_score = dplyr::ntile(ga_per_96, 100),
      gsae_score = dplyr::ntile(goals_saved_above_expected, 100),
      save_pct_score = dplyr::ntile(save_percentage, 100),
      workload_score = dplyr::ntile(shots_faced_per_96, 100),
      goals_saved_above_expected_per_96 = mean(goals_saved_above_expected_per_96, na.rm = TRUE),
      goals_above_replacement_per_96 = mean(goals_above_replacement_per_96, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # Create goalkeeper quality score
  gk_quality <- gk_scores %>%
    dplyr::mutate(
      # Goalkeeper-specific weighting
      player_quality_score = 0.35 * gsae_score +
        0.25 * ga_score +
        0.20 * save_pct_score +
        0.20 * workload_score,
    )

  # Add rankings and position information
  gk_rankings <- gk_quality %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(
      position_rank = rank(-player_quality_score, ties.method = "min")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(general_position = "GK")

  # Format goalkeeper data to match field player structure
  summary_goalie <- gk_rankings %>%
    dplyr::select(
      season,
      player_id,
      team_id,
      general_position,
      position_group,
      minutes_played,
      goals_above_replacement_per_96,
      goals_saved_above_expected_per_96,
      player_quality_score,
      position_rank
    ) %>%
    dplyr::rename(
      position_value = goals_saved_above_expected_per_96
    )

  # Combine field players and goalkeepers
  season.full <- dplyr::bind_rows(summary_data, summary_goalie %>% dplyr::select(-position_rank))

  # Map positions to broader groups
  position_mapping <- c(
    "CB" = "Defense",  # Center Back
    "CM" = "Midfield", # Center Midfield
    "ST" = "Offense",  # Striker
    "FB" = "Defense",  # Full Back
    "W"  = "Offense",  # Winger
    "AM" = "Offense",  # Attacking Midfield
    "DM" = "Midfield",  # Defensive Midfield,
    "GK" = "Goalie"
  )

  # Add a new column for the grouped positions
  season.full <- season.full %>%
    dplyr::mutate(position_group = position_mapping[general_position])

  # Calculate percentage of season played
  season.full <- season.full %>%
    dplyr::mutate(
      pct.season.played = minutes_played / 3264  # Maximum minutes in a 36-game season (36 games * 90 minutes + stoppage)
    )

  # Fetch player details to get ages
  ids <- unique(season.full$player_id)
  players <- asa_client$get_players(leagues = "mls", ids = ids)

  # Join player details and calculate ages
  season.full <- season.full %>%
    dplyr::left_join(
      # get player birthday, join by player_id
      players %>%
        dplyr::select(player_id, player_name, birth_date),
      by = c("player_id")
    ) %>%
    dplyr::mutate(
      # Create cutoff date for age at the start of each season (first game generally)
      age_cutoff = lubridate::ymd(paste0(season, "-02-15")),
      player_name = player_name,
      # Convert birth_date to Date type
      birth_date = lubridate::ymd(birth_date),

      # Calculate age
      age = lubridate::interval(birth_date, age_cutoff) %>%
        lubridate::as.period("years") %>%
        lubridate::year()
    ) %>%
    # Drop unnecessary columns
    dplyr::select(-age_cutoff)

  # Create final dataset with selected columns
  mls.final <- season.full %>%
    dplyr::select(
      season, player_id, player_name, team_id,
      position_group, player_quality_score, goals_above_replacement_per_96,
      position_value, age, pct.season.played, minutes_played
    )

  # Save to data-raw
  write.csv(mls.final, "data-raw/mls_final.csv", row.names = FALSE)

  # Prepare output
  if (!is.null(output_path)) {
    # Create directory if it doesn't exist
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE)
    }
    write.csv(mls.final, file.path(output_path, "mls_tidy.csv"), row.names = FALSE)
    message("MLS data saved to ", file.path(output_path, "mls_tidy.csv"))
    return(invisible(file.path(output_path, "mls_tidy.csv")))
  } else {
    return(mls.final)
  }
}
