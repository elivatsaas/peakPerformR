#' Calculate Prime Quality Index (PQI)
#'
#' @description
#' Calculates the Prime Quality Index (PQI) for players based on their prime years performance
#' with a simplified formula that doesn't include consistency factors. The PQI score is directly
#' expressed as a percentile (0-100) within the appropriate comparison group.
#'
#' @param player_data A data frame containing player performance data.
#'                    Must include columns with prime metrics.
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
#' @return A data frame with calculated PQI scores and tiers for each player.
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
  has_prime_avg_tier <- "prime_avg_tier" %in% colnames(player_data)
  has_prime_avg_value <- "prime_avg_value" %in% colnames(player_data)
  has_prime_peak_value <- "prime_peak_value" %in% colnames(player_data)
  has_prime_seasons <- "prime_seasons" %in% colnames(player_data)

  has_pre_aggregated <- all(c("prime_seasons", "prime_avg_tier",
                              "prime_avg_value", "prime_peak_value") %in% colnames(player_data))

  # If data is in per-season format, or missing required columns, stop with helpful message
  if (!has_pre_aggregated) {
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

  # Calculate raw PQI for all players
  player_data <- player_data |>
    dplyr::mutate(
      # Ensure minimum values for safety in calculations
      prime_avg_value_safe = pmax(prime_avg_value, 0.01),

      # Simplified formula without consistency factor
      pqi_raw = dplyr::case_when(
        !is.na(prime_seasons) & prime_seasons > 0 ~
          prime_avg_tier *
          log(prime_seasons + 1) *
          prime_avg_value_safe *
          (1 + (prime_peak_value * 0.3)),
        TRUE ~ 0.01  # Assign a very small positive value instead of NA
      )
    ) |>
    dplyr::select(-prime_avg_value_safe) # Clean up temporary column

  # Instead of calculating min/max normalized scores, directly calculate percentile ranks
  # within the appropriate grouping
  pqi <- player_data |>
    # Use split-apply-combine approach with the correct grouping
    dplyr::group_split(league) |>
    purrr::map_dfr(function(league_data) {
      league_name <- unique(league_data$league)

      # Determine if this league should use position-specific normalization
      use_position_grouping <- nfl_by_position && league_name == "NFL"

      if (use_position_grouping) {
        # Group by both league and position for NFL
        league_data |>
          dplyr::group_by(league, position) |>
          dplyr::mutate(
            # Calculate percentile rank directly as the score (multiplied by 100)
            pqi_score = dplyr::percent_rank(pqi_raw) * 100
          ) |>
          dplyr::ungroup()
      } else {
        # Group by league only for other leagues
        league_data |>
          dplyr::group_by(league) |>
          dplyr::mutate(
            # Calculate percentile rank directly as the score (multiplied by 100)
            pqi_score = dplyr::percent_rank(pqi_raw) * 100
          ) |>
          dplyr::ungroup()
      }
    })

  # Apply either percentile-based or cluster-based tier assignment
  if (tier_method == "percentile") {
    # Since pqi_score is already a percentile, we can use it directly for tier assignment
    pqi <- pqi |>
      dplyr::mutate(
        prime_tier = dplyr::case_when(
          pqi_score >= 95 ~ "Hall of Fame",  # Top 5% (95th percentile and above)
          pqi_score >= 75 ~ "Elite Player",  # 75th to 95th percentile
          pqi_score >= 45 ~ "Great Starter", # 45th to 75th percentile
          pqi_score >= 15 ~ "Starter",       # 15th to 45th percentile
          TRUE ~ "Backup"                    # Bottom 15%
        )
      )
  } else {
    # Cluster-based tier assignment using CLARA
    # Process each league separately for clustering
    leagues <- unique(pqi$league)
    pqi_list <- list()

    for (league_name in leagues) {
      league_data <- pqi |> dplyr::filter(league == league_name)

      # Skip leagues with too few players for meaningful clustering
      min_players_for_clara <- 15  # CLARA needs a reasonable number of observations
      if (nrow(league_data) < min_players_for_clara) {
        # Not enough data for clustering, use percentile directly
        league_data <- league_data |>
          dplyr::mutate(
            prime_tier = dplyr::case_when(
              pqi_score >= 95 ~ "Hall of Fame",
              pqi_score >= 75 ~ "Elite Player",
              pqi_score >= 45 ~ "Great Starter",
              pqi_score >= 15 ~ "Starter",
              TRUE ~ "Backup"
            )
          )
        pqi_list[[league_name]] <- league_data
        next
      }

      # Handle leagues with enough data for clustering
      set.seed(123) # For reproducibility

      # Create input matrix for clustering
      cluster_data <- as.matrix(league_data$pqi_score)

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
          mean_pqi = tapply(league_data$pqi_score, league_data$cluster, mean)
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
            prime_tier = dplyr::case_when(
              pqi_score >= 95 ~ "Hall of Fame",
              pqi_score >= 75 ~ "Elite Player",
              pqi_score >= 45 ~ "Great Starter",
              pqi_score >= 15 ~ "Starter",
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

  message("PQI calculation complete for ", nrow(pqi), " players using ", tier_method, " tier assignment.",
          " PQI scores represent direct percentile ranks (0-100).")
  return(pqi)
}
