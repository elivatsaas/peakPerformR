#' Build Combined Sports Analytics Dataset
#'
#' @description
#' Creates a combined dataset of player performance across multiple sports leagues,
#' standardizing player values and metrics to allow for cross-sport comparisons.
#' By default, uses pre-packaged data included with the package. When use_cached=FALSE,
#' rebuilds from source data for each sport.
#'
#' @param output_path Character string. Optional path to save output CSV file.
#' @param use_cached Logical. Whether to use cached data (TRUE) or rebuild from APIs (FALSE).
#' @param nfl_value Symbol. The metric to use for NFL player value. Default is fpts.game
#' @param nhl_value Symbol. The metric to use for NHL player value. Default is position_value.
#' @param pwhl_value Symbol. The metric to use for PWHL player value. Default is position_value.
#' @param mlb_value Symbol. The metric to use for MLB player value. Default is total_war.
#' @param mls_value Symbol. The metric to use for MLS player value. Default is player_quality_score.
#' @param nwsl_value Symbol. The metric to use for NWSL player value. Default is player_quality_score.
#' @param nba_value Symbol. The metric to use for NBA player value. Default is pie.game.
#' @param wnba_value Symbol. The metric to use for WNBA player value. Default is pie.game.
#' @param chess_value Symbol. The metric to use for chess player value. Default is max_rating.
#' @param ... Additional arguments passed to individual build functions.
#'
#' @return A data frame containing standardized player data across all sports, or
#'         the path to the saved CSV file if output_path is specified.
#'
#' @importFrom dplyr filter mutate select rename group_by ungroup arrange left_join inner_join bind_rows ntile distinct sym case_when row_number n across everything
#' @importFrom stringr str_to_lower str_replace_all
#' @importFrom stats sd weighted.mean
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # Get pre-packaged combined data
#' all_sports <- build_all_sports()
#'
#' # Rebuild data for all sports and customize value metrics
#' all_sports_custom <- build_all_sports(
#'   use_cached = FALSE,
#'   nfl_value = position_value_2,
#'   mlb_value = war_batting
#' )
#'
#' # Save to specific location
#' build_all_sports(output_path = "path/to/save/directory")
#' }
#'
#' @export
build_all_sports <- function(output_path = NULL,
                             use_cached = TRUE,
                             nfl_value = fpts.game,
                             nhl_value = position_value,
                             pwhl_value = position_value,
                             mlb_value = total_war,
                             mls_value = player_quality_score,
                             nwsl_value = player_quality_score,
                             nba_value = pie.game,
                             wnba_value = pie.game,
                             chess_value = max_rating,
                             ...) {

  # Check required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it with install.packages('dplyr').")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required. Please install it with install.packages('stringr').")
  }

  # First try package data if use_cached is TRUE
  if(use_cached) {
    all_sports_data <- tryCatch({
      # Try to load package data
      all_sports_tidy <- NULL
      data("all_sports_tidy", envir = environment())
      all_sports_tidy
    }, error = function(e) {
      # If loading package data fails, try to load from CSV
      csv_path <- system.file("extdata", "all_sports_final.csv", package = "yourpackage")
      if (csv_path == "") {
        warning("Could not find cached combined sports data. Attempting to rebuild from source data.")
        return(NULL)
      }
      tryCatch({
        read.csv(csv_path)
      }, error = function(e2) {
        warning("Could not read CSV file. Attempting to rebuild from source data.")
        return(NULL)
      })
    })

    # If we successfully loaded the data, either return it or save it
    if (!is.null(all_sports_data)) {
      if (!is.null(output_path)) {
        # Create directory if it doesn't exist
        if (!dir.exists(output_path)) {
          dir.create(output_path, recursive = TRUE)
        }
        write.csv(all_sports_data, file.path(output_path, "all_sports_tidy.csv"), row.names = FALSE)
        message("Combined sports data saved to ", file.path(output_path, "all_sports_tidy.csv"))
        return(invisible(file.path(output_path, "all_sports_tidy.csv")))
      } else {
        return(all_sports_data)
      }
    }
  }

  # If we get here, either use_cached was FALSE or we couldn't load cached data
  message("Building combined sports data from source data. This may take time...")

  # Create data-raw directory if it doesn't exist
  if (!dir.exists("data-raw")) {
    dir.create("data-raw", recursive = TRUE)
  }

  # Function to load data from package or csv
  load_sport_data <- function(sport) {
    # First try to load from package
    var_name <- paste0(sport, "_tidy")

    sport_data <- tryCatch({
      # Try to get data from package environment
      get(var_name, envir = .GlobalEnv)
    }, error = function(e) {
      # Try to load from package data
      df <- NULL
      tryCatch({
        data(list = var_name, envir = environment())
        df <- get(var_name, envir = environment())
      }, error = function(e2) {
        # If not available in package, try CSV
        csv_path <- file.path("data-raw", paste0(sport, "_final.csv"))
        if (file.exists(csv_path)) {
          df <- read.csv(csv_path)
        } else {
          # Try the inst/extdata directory
          csv_path <- system.file("extdata", paste0(sport, "_final.csv"), package = "yourpackage")
          if (csv_path != "") {
            df <- read.csv(csv_path)
          } else {
            # If failed, try to rebuild
            build_func <- get(paste0("build_", sport), mode = "function")
            message(paste("Loading", sport, "data failed. Attempting to rebuild..."))
            df <- build_func(use_cached = FALSE, ...)
          }
        }
      })
      return(df)
    })

    if (is.null(sport_data)) {
      stop(paste("Failed to load data for", sport))
    }

    return(sport_data)
  }

  # Helper function to slugify names for standardization
  slugify <- function(name) {
    name %>%
      stringr::str_to_lower() %>%      # Convert to lowercase
      stringr::str_replace_all(" ", "-") %>%  # Replace spaces with hyphens
      stringr::str_replace_all("[^a-z0-9-]", "") # Remove non-alphanumeric characters except hyphens
  }

  # Load all sports datasets
  message("Loading sport datasets...")

  tryCatch({
    nfl <- load_sport_data("nfl")
    nhl <- load_sport_data("nhl")
    pwhl <- load_sport_data("pwhl")
    mlb <- load_sport_data("mlb")
    mls <- load_sport_data("mls")
    nwsl <- load_sport_data("nwsl")
    nba <- load_sport_data("nba")
    wnba <- load_sport_data("wnba")
    chess <- load_sport_data("chess")

    # Convert the value parameters to strings for dplyr operations
    nfl_value_str <- deparse(substitute(nfl_value))
    nhl_value_str <- deparse(substitute(nhl_value))
    pwhl_value_str <- deparse(substitute(pwhl_value))
    mlb_value_str <- deparse(substitute(mlb_value))
    mls_value_str <- deparse(substitute(mls_value))
    nwsl_value_str <- deparse(substitute(nwsl_value))
    nba_value_str <- deparse(substitute(nba_value))
    wnba_value_str <- deparse(substitute(wnba_value))
    chess_value_str <- deparse(substitute(chess_value))

    message("Standardizing MLB data...")
    # Standardize MLB data
    mlb_clean <- mlb %>%
      dplyr::rename(
        player_id = playerid,
        position = player_type,
        player_name = PlayerName,
        season = Season,
        age = Age,
        value = !!dplyr::sym(mlb_value_str),
        games_played = total_games
      ) %>%
      dplyr::select(player_id, player_name, season, age, position, value, pct.season.played, games_played) %>%
      dplyr::mutate(
        sport = "MLB",
        season = as.integer(season),
        slug = slugify(player_name),
        player_id = as.character(player_id)  # ensure character type
      )

    message("Standardizing NHL data...")
    # Standardize NHL data
    nhl_clean <- nhl %>%
      dplyr::mutate(
        sport = "NHL",
        player_name = player_full_name,
        season = as.integer(season),
        slug = slugify(player_full_name),
        player_id = as.character(player_id),
        value = !!dplyr::sym(nhl_value_str),
      )  %>%
      dplyr::select(player_id, player_name, slug, season, age, position, value, pct.season.played, games_played, sport)

    message("Standardizing PWHL data...")
    # Standardize PWHL data
    pwhl_clean <- pwhl %>%
      dplyr::mutate(
        sport = "PWHL",
        season = as.integer(season),
        player_id= as.character(player_id),
        slug = slugify(player_name),
        value = !!dplyr::sym(pwhl_value_str),
      )  %>%
      dplyr::select(player_id, player_name, slug, season, age, position, value, pct.season.played, games_played, sport)

    message("Standardizing NFL data...")
    # Standardize NFL data
    nfl_clean <- nfl %>%
      dplyr::filter(!is.na(age)) %>%
      dplyr::mutate(
        sport = "NFL",
        position = position_group,
        season = as.integer(season),
        player_name = display_name,
        slug = slugify(player_name),
        player_id = as.character(player_id),
        value = !!dplyr::sym(nfl_value_str),
        pct.season.played = ifelse(is.null(avg_snap_pct), pct.season.played, avg_snap_pct),
      ) %>%
      dplyr::select(player_id, player_name, slug, season, age, position, value, pct.season.played, games_played, sport)

    message("Standardizing MLS data...")
    # Standardize MLS data
    mls_clean <- mls %>%
      dplyr::mutate(
        sport = "MLS",
        slug = slugify(player_name),
        season = as.integer(season),
        player_id = as.character(player_id),
        value = !!dplyr::sym(mls_value_str),
        position = position_group,
        games_played = pct.season.played * 30
      )  %>%
      dplyr::select(player_id, player_name, slug, season, age, position, value, pct.season.played, games_played, sport)

    message("Standardizing NWSL data...")
    # Standardize NWSL data
    nwsl_clean <- nwsl %>%
      dplyr::mutate(
        sport = "NWSL",
        season = as.integer(season),
        slug = slugify(player_name),
        player_id = as.character(player_id),
        value = !!dplyr::sym(nwsl_value_str),
        position = position_group,
        games_played = pct.season.played * 26
      )  %>%
      dplyr::select(player_id, slug, player_name, season, age, position, value, pct.season.played, games_played, sport)

    message("Standardizing NBA data...")
    # Standardize NBA data
    nba_clean <- nba %>%
      dplyr::mutate(
        sport = "NBA",
        season = as.integer(season),
        player_name = athlete_display_name,
        slug = slugify(player_name),
        player_id = as.character(athlete_id),
        value = !!dplyr::sym(nba_value_str),
        pct.season.played = pct_season_played
      )  %>%
      dplyr::select(player_id, slug, player_name, position, season, age, value, pct.season.played, games_played, sport)

    message("Standardizing WNBA data...")
    # Standardize WNBA data
    wnba_clean <- wnba %>%
      dplyr::mutate(
        sport = "WNBA",
        season = as.integer(season),
        player_name = athlete_display_name,
        slug = slugify(player_name),
        player_id = as.character(athlete_id),
        value = !!dplyr::sym(wnba_value_str),
        pct.season.played = pct_season_played
      )  %>%
      dplyr::select(player_id, slug, player_name, position, season, age, value, pct.season.played, games_played, sport)

    message("Standardizing Chess data...")
    # Standardize Chess data
    chess_clean <- chess %>%
      dplyr::mutate(
        sport = "CHESS",
        season = as.integer(year),
        position = title,
        player_name = name,
        slug = slugify(player_name),
        player_id = as.character(username),
        value = !!dplyr::sym(chess_value_str),
      )  %>%
      dplyr::select(player_id, slug, games_played, player_name, position, gender, season, age, value, sport)

    message("Combining all sports data...")
    # Now bind the rows
    all_sports <- dplyr::bind_rows(mlb_clean, nhl_clean, pwhl_clean, nfl_clean, mls_clean, nwsl_clean, nba_clean, wnba_clean, chess_clean)

    # Cap percentage at 1
    all_sports <- all_sports %>%
      dplyr::mutate(pct.season.played = ifelse(pct.season.played > 1, 1, pct.season.played))

    # Join back to original data and classify sports and genders
    all_sports <- all_sports %>%
      dplyr::mutate(
        league = sport,
        sport = dplyr::case_when(
          league %in% c("MLB")          ~ "Baseball",
          league %in% c("NHL", "PWHL")  ~ "Hockey",
          league %in% c("NFL")          ~ "Football",
          league %in% c("MLS", "NWSL")  ~ "Soccer",
          league %in% c("NBA", "WNBA")  ~ "Basketball",
          league %in% c("CHESS")        ~ "Chess",
          TRUE                          ~ as.character(league)  # Fallback
        ),
        gender = dplyr::case_when(
          league %in% c("PWHL", "NWSL", "WNBA") ~ "F",
          league %in% c("CHESS") ~ gender,
          TRUE ~ "M")
      )

    message("Creating unique player IDs...")
    # Function to create unique player IDs
    create_unique_player_ids <- function(df) {
      # Create a dataframe of distinct player identities
      distinct_players <- df %>%
        # Select only the columns we need for identification
        dplyr::select(sport, league, player_name, player_id, slug) %>%
        dplyr::distinct() %>%
        # Group by sport, league, and player name
        dplyr::group_by(sport, league, player_name) %>%
        # Simply number the players sequentially within each group
        dplyr::mutate(
          id_suffix = sprintf("%02d", dplyr::row_number() - 1),
          unique_id = paste(league, slug, id_suffix, sep = "-")
        ) %>%
        dplyr::ungroup() %>%
        # Only keep what we need to join back
        dplyr::select(sport, league, player_name, player_id, unique_id)

      # Join the unique IDs back to the original data frame
      df %>%
        dplyr::left_join(distinct_players, by = c("sport", "league", "player_name", "player_id"))
    }

    # Apply unique IDs
    all_sports <- create_unique_player_ids(all_sports)

    message("Processing chess data separately...")
    # Handle chess data separately
    chess <- all_sports %>%
      dplyr::filter(league == "CHESS") %>%
      dplyr::mutate(
        player_value = value
      )

    # Split chess by gender
    chess <- chess %>%
      dplyr::mutate(
        league = ifelse(
          gender == "M", "CHESS_M", "CHESS_F"
        )
      )

    message("Calculating player values...")
    # Process non-chess sports
    all_sports <- all_sports %>%
      dplyr::filter(league != "CHESS") %>%
      dplyr::group_by(league, position) %>%
      dplyr::mutate(
        availability_percentile = dplyr::ntile(games_played, 100),
        player_value = value * (availability_percentile/100)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-availability_percentile)

    # Recombine all sports data
    all_sports <- dplyr::bind_rows(all_sports, chess)

    message("Calculating career statistics...")
    # Calculate career statistics
    all_sports <- all_sports %>%
      dplyr::rename(id = unique_id) %>%
      dplyr::group_by(id) %>%
      dplyr::arrange(season) %>%
      dplyr::mutate(
        career_games = cumsum(games_played),
        career_seasons = dplyr::row_number(),
        career_mean = stats::weighted.mean(
          player_value,
          w = season / max(season),
          na.rm = TRUE
        )
      ) %>%
      dplyr::ungroup()

    message("Calculating z-scores...")
    # Calculate z-scores for each league and position
    all_sports <- all_sports %>%
      dplyr::group_by(league, position) %>%
      dplyr::filter(dplyr::n() >= 5) %>%
      dplyr::mutate(
        z_score = (player_value - mean(player_value, na.rm = TRUE)) / stats::sd(player_value, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()

    # Save the combined data
    write.csv(all_sports, "data-raw/all_sports_final.csv", row.names = FALSE)

    # Also save to inst/extdata if it exists
    if (dir.exists("inst/extdata")) {
      write.csv(all_sports, "inst/extdata/all_sports_final.csv", row.names = FALSE)
    }

    message("Combined sports data successfully built.")

    # If output_path is specified, save there as well
    if (!is.null(output_path)) {
      # Create directory if it doesn't exist
      if (!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE)
      }
      write.csv(all_sports, file.path(output_path, "all_sports_tidy.csv"), row.names = FALSE)
      message("Combined sports data saved to ", file.path(output_path, "all_sports_tidy.csv"))
      return(invisible(file.path(output_path, "all_sports_tidy.csv")))
    } else {
      return(all_sports)
    }
  }, error = function(e) {
    stop(paste("Error building combined sports data:", e$message))
  })
}
