#' Create Player Performance Dataset
#'
#' @description
#' Combines player sports data, prime years information, and performance clusters
#' into a comprehensive dataset for player career analysis.
#'
#' @param sports_data A data frame containing player sports data.
#'                   Must include 'id' and 'age' columns.
#' @param prime_data A data frame containing player prime years information.
#'                  Must include 'id', 'start_age', 'end_age', and 'max_value_age' columns.
#' @param cluster_data A data frame containing player performance clusters.
#'                    Must include 'id', 'age', 'performance_tier', 'scaled_value', and 'cluster' columns.
#' @param prime_file Character. Optional file path to load prime data from.
#' @param cluster_file Character. Optional file path to load cluster data from.
#' @param tier_mapping A named vector mapping performance tier names to numeric scores.
#'                    Default maps the standard five tiers to values 5-1.
#'
#' @return A data frame with combined player data including prime indicators and performance tiers.
#'
#' @examples
#' \dontrun{
#' # Load data from files
#' sports <- build_all_sports()
#' player_prime <- readr::read_csv("player_primes_pct.csv")
#' cluster_data <- readr::read_csv("season_clusters.csv")
#'
#' # Combine the data
#' combined_data <- create_player_performance_dataset(
#'   sports_data = sports,
#'   prime_data = player_prime,
#'   cluster_data = cluster_data
#' )
#'
#' # Alternatively, specify file paths directly
#' combined_data <- create_player_performance_dataset(
#'   sports_data = sports,
#'   prime_file = "player_primes_pct_3min_8max_cubic.csv",
#'   cluster_file = "season_clusters_3min_8max_cubic.csv"
#' )
#' }
#'
#' @export
create_player_performance_dataset <- function(sports_data,
                                              prime_data = NULL,
                                              cluster_data = NULL,
                                              prime_file = NULL,
                                              cluster_file = NULL,
                                              tier_mapping = c("Elite" = 5,
                                                               "Great" = 4,
                                                               "Average" = 3,
                                                               "Below Average" = 2,
                                                               "Replacement Level" = 1)) {

  # Load prime data from file if provided
  if (is.null(prime_data) && !is.null(prime_file)) {
    prime_data <- readr::read_csv(prime_file)
  }

  # Load cluster data from file if provided
  if (is.null(cluster_data) && !is.null(cluster_file)) {
    cluster_data <- readr::read_csv(cluster_file)
  }

  # Validate inputs
  if (is.null(prime_data)) {
    stop("Prime data is required. Please provide either prime_data or prime_file.")
  }

  if (is.null(cluster_data)) {
    stop("Cluster data is required. Please provide either cluster_data or cluster_file.")
  }

  # Create the combined dataset
  final_dataset <- sports_data |>
    # Join with player primes to determine if each season was during prime years
    dplyr::inner_join(
      prime_data |>
        dplyr::select(id, start_age, end_age, max_value_age, prime_duration, threshold_value),
      by = "id"
    ) |>
    # Add flags for prime analysis
    dplyr::mutate(
      in_prime = ifelse(!is.na(start_age) & age >= start_age & age <= end_age, TRUE, FALSE),
      is_peak_age = (age == max_value_age),
      years_from_peak = ifelse(!is.na(max_value_age), age - max_value_age, NA)
    ) |>
    # Join with clustered data to add performance tier
    dplyr::inner_join(
      cluster_data |> dplyr::select(id, age, performance_tier, scaled_value, cluster),
      by = c("id", "age")
    ) |>
    dplyr::filter(!is.na(performance_tier))

  # Apply tier scoring
  final_dataset <- final_dataset |>
    dplyr::mutate(
      tier_score = dplyr::case_when(
        performance_tier %in% names(tier_mapping) ~ tier_mapping[performance_tier],
        TRUE ~ NA_real_
      )
    )

  return(final_dataset)
}
