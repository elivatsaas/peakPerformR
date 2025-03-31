#' Identify Player Prime Years
#'
#' @description
#' Identifies the prime years in a player's career based on performance data.
#' This function can work with either predicted trajectory data (from spline models)
#' or actual player season data, determining prime years as a period where performance
#' remains above a specified threshold of peak performance.
#'
#' @param data A data frame containing player performance data.
#'             For method="predicted": Must include columns 'id', 'age', 'predicted_value', 'games_played', 'league'
#'             For method="actual": Must include columns 'id', 'age', 'player_value', 'games_played', 'league', 'season'
#' @param method Character string. The method to use for identifying prime years.
#'               Options are "predicted" (using player trajectory predictions) or
#'               "actual" (using actual season data). Default is "predicted".
#' @param threshold_pct Numeric. The percentage of peak performance to consider as prime years.
#'                     Default is 70.
#' @param games_pct_threshold Numeric. The percentile threshold for games played to consider
#'                           in prime calculations. Default is 40.
#' @param min_seasons Integer. For method="actual", the minimum number of seasons required
#'                    to calculate a prime. Default is 5.
#'
#' @return A data frame containing prime years information for each player:
#'   \item{id}{Player identifier}
#'   \item{league}{Player's league}
#'   \item{max_value_age}{Age at peak performance}
#'   \item{start_age}{Age when prime years begin}
#'   \item{end_age}{Age when prime years end}
#'   \item{prime_duration}{Duration of prime in years}
#'   \item{threshold_value}{The performance threshold used}
#'   \item{threshold_pct}{The percentage threshold specified}
#'   \item{skip_before_used}{Whether a skip was used before peak due to low games played}
#'   \item{skip_after_used}{Whether a skip was used after peak due to low games played}
#'
#' @examples
#' \dontrun{
#' # With predicted trajectory data
#' player_trajectories <- readr::read_csv("player_spline_predictions.csv")
#' player_joins <- player_trajectories |>
#'   dplyr::left_join(sports |> dplyr::select(id, league, position, age, games_played), by = c("id", "age"))
#' player_prime_pct <- identify_prime(player_joins, method = "predicted", threshold_pct = 80)
#'
#' # With actual player data
#' sports <- build_all_sports(nfl_value = fpts.game)
#' player_prime_actual <- identify_prime(sports, method = "actual", threshold_pct = 80, min_seasons = 5)
#' }
#'
#' @export
identify_prime <- function(data,
                           method = c("predicted", "actual"),
                           threshold_pct = 70,
                           games_pct_threshold = 40,
                           min_seasons = 5) {
  # Match method argument
  method <- match.arg(method)

  # Convert threshold to decimal
  threshold_decimal <- threshold_pct / 100

  # Ungroup if grouped
  if(dplyr::is.grouped_df(data)) {
    data <- dplyr::ungroup(data)
  }

  # Calculate games played threshold by league
  games_thresholds <- data |>
    dplyr::group_by(league) |>
    dplyr::summarize(games_threshold = stats::quantile(games_played, games_pct_threshold/100, na.rm = TRUE),
                     .groups = "drop")

  # Initialize results dataframe
  prime_results <- data.frame()

  # Get player IDs to process based on method
  player_ids <- if (method == "actual") {
    # For actual data, filter by minimum seasons
    player_seasons <- data |>
      dplyr::group_by(id) |>
      dplyr::summarize(
        num_seasons = dplyr::n_distinct(season),
        .groups = "drop"
      )
    unique(player_seasons$id[player_seasons$num_seasons >= min_seasons])
  } else {
    # For predicted data, use all unique IDs
    unique(data$id)
  }

  # Process each player
  for (pid in player_ids) {
    # Get player data sorted by age
    player_data <- data |>
      dplyr::filter(id == pid) |>
      dplyr::arrange(age)

    # Apply appropriate filters based on method
    if (method == "predicted") {
      player_data <- player_data |>
        dplyr::filter(!is.na(predicted_value) & !is.na(age))

      # Skip if insufficient data
      if(nrow(player_data) < 2) next

      # Set value column for later use
      value_col <- "predicted_value"
    } else {
      player_data <- player_data |>
        dplyr::filter(!is.na(player_value) & !is.na(age))

      # Skip if insufficient data
      if(nrow(player_data) < min_seasons) next

      # Set value column for later use
      value_col <- "player_value"
    }

    # Get player metadata
    player_league <- player_data$league[1]

    # Get games threshold for this player - with error handling
    games_threshold_row <- games_thresholds |>
      dplyr::filter(league == player_league)

    # Skip player if we can't find a threshold for their league
    if(nrow(games_threshold_row) == 0) {
      message("No games threshold found for player ", pid, " (", player_league, ")")
      next
    }

    games_threshold <- games_threshold_row$games_threshold

    # Find peak performance
    max_idx <- which.max(player_data[[value_col]])
    max_value <- player_data[[value_col]][max_idx]
    max_age <- player_data$age[max_idx]

    # Calculate threshold value
    min_value <- min(player_data[[value_col]])
    value_range <- max_value - min_value
    threshold_value <- min_value + (value_range * threshold_decimal)

    # Find prime start (working backwards from peak)
    start_idx <- max_idx
    skip_used_before <- FALSE

    if (max_idx > 1) {
      i <- max_idx - 1
      while (i >= 1) {
        # Add safety check for NA values in games_played
        current_games <- player_data$games_played[i]
        if (is.na(current_games)) current_games <- 0

        if (player_data[[value_col]][i] >= threshold_value) {
          # Value above threshold - include in prime
          start_idx <- i
          i <- i - 1
        } else if (!skip_used_before && !is.na(games_threshold) && current_games <= games_threshold) {
          # Value below threshold but low games played - use skip
          skip_used_before <- TRUE
          i <- i - 1
        } else {
          # Can't extend prime further
          break
        }
      }
    }

    # Find prime end (working forwards from peak)
    end_idx <- max_idx
    skip_used_after <- FALSE

    if (max_idx < nrow(player_data)) {
      i <- max_idx + 1
      while (i <= nrow(player_data)) {
        # Add safety check for NA values in games_played
        current_games <- player_data$games_played[i]
        if (is.na(current_games)) current_games <- 0

        if (player_data[[value_col]][i] >= threshold_value) {
          # Value above threshold - include in prime
          end_idx <- i
          i <- i + 1
        } else if (!skip_used_after && !is.na(games_threshold) && current_games <= games_threshold) {
          # Value below threshold but low games played - use skip
          skip_used_after <- TRUE
          i <- i + 1
        } else {
          # Can't extend prime further
          break
        }
      }
    }

    # Record results
    prime_results <- dplyr::bind_rows(prime_results,
                                      data.frame(
                                        id = pid,
                                        league = player_league,
                                        max_value_age = max_age,
                                        start_age = player_data$age[start_idx],
                                        end_age = player_data$age[end_idx],
                                        prime_duration = player_data$age[end_idx] - player_data$age[start_idx] + 1,
                                        threshold_value = threshold_value,
                                        threshold_pct = threshold_pct,
                                        skip_before_used = skip_used_before,
                                        skip_after_used = skip_used_after
                                      ))
  }

  return(prime_results)
}
