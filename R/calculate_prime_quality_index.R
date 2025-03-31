#' Calculate Prime Quality Index (PQI)
#'
#' @description
#' Calculates the Prime Quality Index (PQI) for players based on their prime years performance.
#' This function handles both pre-aggregated prime metrics and per-season data with an 'in_prime' flag.
#'
#' @param player_data A data frame containing player performance data.
#'                    Either pre-aggregated with 'prime_seasons', 'prime_avg_tier', etc.,
#'                    or per-season data with 'in_prime' flag.
#' @param nfl_by_position Logical. Whether to use position-specific normalization for NFL players.
#'                        Default is TRUE.
#' @param exclude_positions Character vector. Positions to exclude from analysis.
#'                          Default is c("OL").
#' @param tier_method Character. Method to use for tier assignment. Options are "percentile" or "cluster".
#'                    Default is "percentile".
#' @param save_output Logical. Whether to save the output to a CSV file. Default is FALSE.
#' @param output_file Character. File name for saving output if save_output is TRUE.
#'                    Default is "pqi_scores.csv".
#'
#' @details
#' Prime tiers can be defined using two different methods:
#'
#' When tier_method = "percentile", tiers are defined by percentile ranks within each league:
#' \itemize{
#'   \item Hall of Fame: Top 5 percentile (95th percentile and above)
#'   \item Elite Player: 75th to 95th percentile
#'   \item Great Starter: 45th to 75th percentile
#'   \item Starter: 15th to 45th percentile
#'   \item Backup: Bottom 15 percentile
#' }
#'
#' When tier_method = "cluster", tiers are defined by CLARA clustering (k=5) of PQI scores
#' within each league, with clusters ordered from highest to lowest performance and labeled
#' accordingly. CLARA is a clustering method designed for large datasets.
#'
#' @return A data frame with calculated PQI scores and tiers for each player.
#'
#' @examples
#' \dontrun{
#' # Calculate PQI scores with percentile-based tiers (default)
#' pqi_percentile <- calculate_prime_quality_index(player_data)
#'
#' # Calculate PQI scores with cluster-based tiers
#' pqi_cluster <- calculate_prime_quality_index(player_data, tier_method = "cluster")
#' }
#'
#' @export
calculate_prime_quality_index <- function(player_data,
                                          nfl_by_position = TRUE,
                                          exclude_positions = c("OL"),
                                          tier_method = "percentile",
                                          save_output = FALSE,
                                          output_file = "pqi_scores.csv") {

  # Check if cluster package is available if using cluster method
  if (tier_method == "cluster" && !requireNamespace("cluster", quietly = TRUE)) {
    stop("The 'cluster' package is required for tier_method='cluster'. Please install it with install.packages('cluster').")
  }

  # Validate tier_method parameter
  if (!tier_method %in% c("percentile", "cluster")) {
    stop("tier_method must be either 'percentile' or 'cluster'")
  }

  # Check for required columns and data format
  has_in_prime <- "in_prime" %in% colnames(player_data)
  has_prime_avg_tier <- "prime_avg_tier" %in% colnames(player_data)
  has_prime_avg_value <- "prime_avg_value" %in% colnames(player_data)
  has_prime_peak_value <- "prime_peak_value" %in% colnames(player_data)
  has_prime_seasons <- "prime_seasons" %in% colnames(player_data)

  has_pre_aggregated <- all(c("prime_seasons", "prime_avg_tier",
                              "prime_avg_value", "prime_peak_value") %in% colnames(player_data))

  # If data is in per-season format, aggregate the prime metrics
  if (!has_pre_aggregated && has_in_prime) {
    message("Error: Your data is in per-season format but missing required tier and value columns")
    message("Please run process_player_primes() on your data first to calculate prime metrics")
    stop("Required prime metrics missing. Run process_player_primes() first.")
  } else if (!has_pre_aggregated) {
    stop("Required columns missing. Need pre-aggregated prime metrics: ",
         "prime_seasons, prime_avg_tier, prime_avg_value, prime_peak_value")
  }

  # Filter out excluded positions
  if (length(exclude_positions) > 0) {
    player_data <- player_data |>
      dplyr::filter(!position %in% exclude_positions)
  }

  # Ensure we're working with distinct player records
  player_data <- player_data |>
    dplyr::group_by(id) |>
    dplyr::distinct(id, .keep_all = TRUE) |>
    dplyr::ungroup()

  # Calculate PQI by league and position
  calculate_pqi_by_league_position <- function(data) {
    # First calculate the raw PQI score for everyone
    pqi_raw <- data |>
      dplyr::mutate(
        # Calculate for all players, assigning a small base value to those without prime seasons
        pqi_raw = dplyr::case_when(
          !is.na(prime_seasons) & prime_seasons > 0 ~
            prime_avg_tier * log(prime_seasons + 1) * prime_avg_value * log(prime_seasons + 1) * (1 + (prime_peak_value * 0.5)),
          TRUE ~ 0.01  # Assign a very small positive value instead of NA
        )
      )

    # Calculate min/max by group separately
    group_stats <- pqi_raw |>
      dplyr::group_by(league, position) |>
      dplyr::summarize(
        min_score = min(pqi_raw, na.rm = TRUE),
        max_score = max(pqi_raw, na.rm = TRUE),
        .groups = "drop"
      )

    # Join the stats and calculate the normalized scores
    pqi_scores <- pqi_raw |>
      dplyr::left_join(group_stats, by = c("league", "position")) |>
      dplyr::mutate(
        # Use case_when instead of if/else
        pqi_score = dplyr::case_when(
          min_score == max_score ~ 50, # If all scores identical
          TRUE ~ (pqi_raw - min_score) / (max_score - min_score) * 100
        )
      ) |>
      # Remove the working columns
      dplyr::select(-min_score, -max_score)

    return(pqi_scores)
  }

  # Calculate PQI by league only
  calculate_pqi_by_league <- function(data) {
    # First calculate the raw PQI score for everyone
    pqi_raw <- data |>
      dplyr::mutate(
        # Calculate for all players, assigning a small base value to those without prime seasons
        pqi_raw = dplyr::case_when(
          !is.na(prime_seasons) & prime_seasons > 0 ~
            prime_avg_tier * log(prime_seasons + 1) * prime_avg_value * log(prime_seasons + 1) * (1 + (prime_peak_value * 0.5)),
          TRUE ~ 0.01  # Assign a very small positive value instead of NA
        )
      )

    # Calculate min/max by group separately
    group_stats <- pqi_raw |>
      dplyr::group_by(league) |>
      dplyr::summarize(
        min_score = min(pqi_raw, na.rm = TRUE),
        max_score = max(pqi_raw, na.rm = TRUE),
        .groups = "drop"
      )

    # Join the stats and calculate the normalized scores
    pqi_scores <- pqi_raw |>
      dplyr::left_join(group_stats, by = c("league")) |>
      dplyr::mutate(
        # Use case_when instead of if/else
        pqi_score = dplyr::case_when(
          min_score == max_score ~ 50, # If all scores identical
          TRUE ~ (pqi_raw - min_score) / (max_score - min_score) * 100
        )
      ) |>
      # Remove the working columns
      dplyr::select(-min_score, -max_score)

    return(pqi_scores)
  }

  # Calculate both sets of PQI scores
  pqi_scores_league_position <- calculate_pqi_by_league_position(player_data)
  pqi_scores_league <- calculate_pqi_by_league(player_data)

  # Combine the two sets of scores
  pqi <- pqi_scores_league |>
    dplyr::rename(pqi_score_league = pqi_score) |>
    dplyr::left_join(pqi_scores_league_position |>
                       dplyr::select(id, pqi_score) |>
                       dplyr::rename(pqi_score_league_position = pqi_score),
                     by = "id")

  # Select which PQI to use based on league
  pqi <- pqi |>
    dplyr::mutate(
      pqi_selected = dplyr::case_when(
        nfl_by_position & league %in% "NFL" ~ pqi_score_league_position,
        TRUE ~ pqi_score_league
      )
    )

  # Apply either percentile-based or cluster-based tier assignment
  if (tier_method == "percentile") {
    # Percentile-based tier assignment (original method)
    pqi <- pqi |>
      # Calculate percentile rank and tiers based on the selected PQI within each league
      dplyr::group_by(league) |>
      dplyr::mutate(
        percentile_rank = dplyr::percent_rank(pqi_selected),
        prime_tier = dplyr::case_when(
          percentile_rank >= 0.95 ~ "Hall of Fame",  # Top 5% (95th percentile and above)
          percentile_rank >= 0.75 ~ "Elite Player",  # 75th to 95th percentile
          percentile_rank >= 0.45 ~ "Great Starter", # 45th to 75th percentile
          percentile_rank >= 0.15 ~ "Starter",       # 15th to 45th percentile
          TRUE ~ "Backup"                            # Bottom 15%
        )
      ) |>
      dplyr::ungroup()
  } else {
    # Cluster-based tier assignment using CLARA
    # Process each league separately for clustering
    leagues <- unique(pqi$league)
    pqi_list <- list()

    for (league_name in leagues) {
      league_data <- pqi |> dplyr::filter(league == league_name)

      # Calculate the sample size for CLARA (minimum 20, maximum 100 or half the data size)
      sample_size <- min(max(20, nrow(league_data) / 10), min(100, nrow(league_data) / 2))

      # Skip leagues with too few players for meaningful clustering
      min_players_for_clara <- 15  # CLARA needs a reasonable number of observations
      if (nrow(league_data) < min_players_for_clara) {
        # Not enough data for clustering, use percentile ranking instead
        league_data <- league_data |>
          dplyr::mutate(
            percentile_rank = dplyr::percent_rank(pqi_selected),
            prime_tier = dplyr::case_when(
              percentile_rank >= 0.95 ~ "Hall of Fame",
              percentile_rank >= 0.75 ~ "Elite Player",
              percentile_rank >= 0.45 ~ "Great Starter",
              percentile_rank >= 0.15 ~ "Starter",
              TRUE ~ "Backup"
            )
          )
        pqi_list[[league_name]] <- league_data
        next
      }

      # Handle leagues with enough data for clustering
      set.seed(123) # For reproducibility

      # Create input matrix for clustering (just the pqi_selected column)
      cluster_data <- as.matrix(league_data$pqi_selected)

      # Perform CLARA clustering (k=5)
      # Adjust the samples parameter based on data size
      tryCatch({
        clara_result <- cluster::clara(
          cluster_data,
          k = 5,
          samples = max(5, min(50, floor(nrow(league_data)/10))),
          pamLike = TRUE
        )

        # Add cluster assignments to the data
        league_data$cluster <- clara_result$clustering

        # Calculate mean PQI score for each cluster and order them
        cluster_means <- data.frame(
          cluster = 1:5,
          mean_pqi = tapply(league_data$pqi_selected, league_data$cluster, mean)
        )

        # Order clusters from highest to lowest mean PQI
        cluster_means <- cluster_means[order(cluster_means$mean_pqi, decreasing = TRUE), ]

        # Create mapping from original cluster numbers to tier ranks (1 = best, 5 = worst)
        cluster_mapping <- data.frame(
          cluster = cluster_means$cluster,
          tier_rank = 1:5
        )

        # Join mapping to get tier ranks for each player
        league_data <- league_data |>
          dplyr::left_join(cluster_mapping, by = "cluster") |>
          dplyr::mutate(
            # Assign tier labels based on tier rankings
            prime_tier = dplyr::case_when(
              tier_rank == 1 ~ "Hall of Fame",
              tier_rank == 2 ~ "Elite Player",
              tier_rank == 3 ~ "Great Starter",
              tier_rank == 4 ~ "Starter",
              tier_rank == 5 ~ "Backup"
            )
          ) |>
          dplyr::select(-cluster, -tier_rank) # Remove temporary columns
      }, error = function(e) {
        # If CLARA fails, fall back to percentile method
        message("CLARA clustering failed for league ", league_name, ": ", conditionMessage(e))
        league_data <- league_data |>
          dplyr::mutate(
            percentile_rank = dplyr::percent_rank(pqi_selected),
            prime_tier = dplyr::case_when(
              percentile_rank >= 0.95 ~ "Hall of Fame",
              percentile_rank >= 0.75 ~ "Elite Player",
              percentile_rank >= 0.45 ~ "Great Starter",
              percentile_rank >= 0.15 ~ "Starter",
              TRUE ~ "Backup"
            )
          )
      })

      pqi_list[[league_name]] <- league_data
    }

    # Recombine the league data
    pqi <- do.call(rbind, pqi_list)
  }

  # Save output if requested
  if(save_output) {
    utils::write.csv(pqi, output_file)
  }

  message("PQI calculation complete for ", nrow(pqi), " players using ", tier_method, " tier assignment.")
  return(pqi)
}
