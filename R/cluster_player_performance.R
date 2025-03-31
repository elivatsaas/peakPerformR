#' Cluster Player Performance
#'
#' @description
#' Clusters player performance based on predicted trajectory data,
#' assigning players to performance tiers based on their scaled values.
#'
#' @param player_trajectories A data frame containing player predicted trajectories.
#'                           Must include 'id', 'age', and 'predicted_value' columns.
#' @param sports_data A data frame containing player sports data.
#'                   Must include 'id', 'age', 'league', 'position', 'sport', and 'season' columns.
#' @param max_debut_age Maximum age for player debut to be included in analysis. Default is 23.
#' @param min_age Minimum age for player data to be included. Default is 18.
#' @param num_clusters Number of clusters to create. Default is 5.
#' @param seed Random seed for reproducibility. Default is 123.
#' @param clara_samples Number of samples for CLARA clustering. Default is 5000.
#' @param plot_distributions Logical. Whether to plot distributions of scaled values and clusters. Default is FALSE.
#' @param print_summaries Logical. Whether to print summary statistics of the clusters. Default is FALSE.
#' @param save_output Logical. Whether to save the output to a CSV file. Default is FALSE.
#' @param output_file Character. File name for saving output if save_output is TRUE. Default is "season_clusters.csv".
#'
#' @return A data frame with player data, scaled values, and assigned performance tiers.
#' @importFrom dplyr lag lead
#' @examples
#' \dontrun{
#' # Load data
#' sports <- build_all_sports()
#' player_trajectories <- readr::read_csv("player_spline_predictions.csv")
#'
#' # Run clustering
#' result <- cluster_player_performance(
#'   player_trajectories = player_trajectories,
#'   sports_data = sports,
#'   plot_distributions = TRUE,
#'   print_summaries = TRUE
#' )
#'
#' # View results
#' head(result)
#' }
#'
#' @export
cluster_player_performance <- function(player_trajectories,
                                       sports_data,
                                       max_debut_age = 23,
                                       min_age = 18,
                                       num_clusters = 5,
                                       seed = 123,
                                       clara_samples = 5000,
                                       plot_distributions = FALSE,
                                       print_summaries = FALSE,
                                       save_output = FALSE,
                                       output_file = "season_clusters.csv") {

  # Calculate debut ages
  debut_ages <- sports_data |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      debut_age = age[which.min(season)]
    ) |>
    dplyr::ungroup()

  # Filter eligible players
  eligible_players <- debut_ages |>
    dplyr::filter(debut_age <= max_debut_age) |>
    dplyr::select(id)

  # Filter sports data
  filtered_sports <- sports_data |>
    dplyr::inner_join(eligible_players, by = "id") |>
    dplyr::filter(age >= min_age)

  # Join trajectory and sports data
  joined_data <- player_trajectories |>
    dplyr::left_join(
      filtered_sports |> dplyr::select(id, age, league, position, sport),
      by = c("id", "age")
    ) |>
    dplyr::filter(!is.na(league))

  if(print_summaries) {
    print(paste("Number of records after joining:", nrow(joined_data)))
    print(paste("Number of players:", length(unique(joined_data$id))))
  }

  # Scale the predicted values within each league-position group
  scaled_data <- joined_data |>
    dplyr::group_by(league, position) |>
    dplyr::mutate(
      group_mean = mean(predicted_value, na.rm = TRUE),
      group_sd = stats::sd(predicted_value, na.rm = TRUE),
      # Handle the case where sd = 0 (all values the same)
      scaled_value = dplyr::if_else(
        group_sd > 0,
        (predicted_value - group_mean) / group_sd,
        0
      )
    ) |>
    dplyr::ungroup()

  # Plot distribution if requested
  if(plot_distributions) {
    hist_plot <- ggplot2::ggplot(scaled_data, ggplot2::aes(x = scaled_value)) +
      ggplot2::geom_histogram(bins = 50, fill = "steelblue", color = "black") +
      ggplot2::labs(title = "Distribution of Scaled Performance Values",
                    x = "Scaled Value (Z-score)",
                    y = "Count") +
      ggplot2::theme_minimal()

    print(hist_plot)
  }

  # Prepare data for clustering
  scaled_matrix <- scaled_data |>
    dplyr::select(scaled_value) |>
    as.matrix()

  # Set seed for reproducibility
  set.seed(seed)

  # Perform CLARA clustering
  clara_result <- cluster::clara(scaled_matrix, k = num_clusters, samples = clara_samples, pamLike = TRUE)

  # Add cluster assignments to the data
  clustered_data <- scaled_data |>
    dplyr::mutate(cluster = clara_result$clustering)

  # Calculate cluster centers
  cluster_centers <- clustered_data |>
    dplyr::group_by(cluster) |>
    dplyr::summarize(centroid = mean(scaled_value, na.rm = TRUE))

  # Create performance tier mapping based on number of clusters
  if(num_clusters == 5) {
    tier_names <- c("Elite", "Great", "Average", "Below Average", "Replacement Level")
  } else if(num_clusters == 3) {
    tier_names <- c("Above Average", "Average", "Below Average")
  } else if(num_clusters == 4) {
    tier_names <- c("Elite", "Above Average", "Below Average", "Replacement Level")
  } else {
    # For other numbers of clusters, create numeric tiers
    tier_names <- paste("Tier", 1:num_clusters)
  }

  # Ensure we have the right number of tier names
  if(length(tier_names) != num_clusters) {
    tier_names <- paste("Tier", 1:num_clusters)
  }

  # Sort cluster centers and assign tier names
  cluster_centroids <- cluster_centers |>
    dplyr::arrange(dplyr::desc(centroid)) |>
    dplyr::mutate(performance_tier = tier_names)

  # Map clusters to performance tiers
  clustered_data <- clustered_data |>
    dplyr::left_join(
      cluster_centroids |> dplyr::select(cluster, performance_tier),
      by = "cluster"
    )

  # Print summaries if requested
  if(print_summaries) {
    tier_distribution <- clustered_data |>
      dplyr::group_by(performance_tier) |>
      dplyr::summarise(
        count = dplyr::n(),
        percentage = dplyr::n() / nrow(clustered_data) * 100,
        mean_scaled_value = mean(scaled_value, na.rm = TRUE),
        min_scaled_value = min(scaled_value, na.rm = TRUE),
        max_scaled_value = max(scaled_value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(match(performance_tier, tier_names))

    print("Tier Distribution:")
    print(tier_distribution)

    sport_league_distribution <- clustered_data |>
      dplyr::group_by(sport, league, performance_tier) |>
      dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
      dplyr::arrange(sport, league, match(performance_tier, tier_names))

    print("Sport/League Distribution:")
    print(sport_league_distribution)

    tier_boundaries <- cluster_centroids |>
      dplyr::arrange(centroid) |>
      dplyr::mutate(
        lower_bound = tidyr::lag(centroid, default = min(clustered_data$scaled_value, na.rm = TRUE) - 0.1),
        upper_bound = tidyr::lead(centroid, default = max(clustered_data$scaled_value, na.rm = TRUE) + 0.1)
      ) |>
      dplyr::mutate(
        boundary = (upper_bound + centroid) / 2
      ) |>
      dplyr::select(performance_tier, boundary) |>
      dplyr::arrange(match(performance_tier, tier_names))

    print("Tier Boundaries (in terms of scaled values):")
    print(tier_boundaries)
  }

  # Plot clustering visualization if requested
  if(plot_distributions) {
    # Create color palette with appropriate length
    if(num_clusters <= 5) {
      color_values <- c("#1a9850", "#66bd63", "#fee08b", "#fc8d59", "#d73027")[1:num_clusters]
      names(color_values) <- tier_names
    } else {
      # For more clusters, use a colorRampPalette
      color_values <- grDevices::colorRampPalette(c("#1a9850", "#fee08b", "#d73027"))(num_clusters)
      names(color_values) <- tier_names
    }

    cluster_plot <- ggplot2::ggplot(clustered_data, ggplot2::aes(x = age, y = predicted_value, color = performance_tier)) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::facet_wrap(~ league, scales = "free_y") +
      ggplot2::scale_color_manual(values = color_values) +
      ggplot2::labs(title = "Player Performance Tiers by League",
                    subtitle = "Based on Predicted Values Scaled Within League-Position Groups",
                    x = "Age", y = "Predicted Value",
                    color = "Performance Tier") +
      ggplot2::theme_minimal()

    print(cluster_plot)
  }

  # Create final dataset
  final_data <- clustered_data |>
    dplyr::select(
      id, age, predicted_value, league, position, sport,
      scaled_value, performance_tier, cluster
    ) |>
    dplyr::arrange(id, age)

  # Save output if requested
  if(save_output) {
    readr::write_csv(final_data, output_file)
    if(print_summaries) {
      print(paste("Results saved to", output_file))
    }
  }

  return(final_data)
}
