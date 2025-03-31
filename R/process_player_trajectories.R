#' Process Player Data and Generate Career Trajectories
#'
#' @description
#' Takes player performance data, fits spline models to each player's career,
#' and generates predicted career trajectories. This function handles the
#' entire workflow from data preparation to trajectory prediction.
#'
#' @param player_data A data frame containing player performance data.
#'                   Must include 'id', 'age', 'player_value', and 'player_name' columns.
#' @param min_knots Minimum number of knots for spline fits. Default is 3.
#' @param max_knots Maximum number of knots for spline fits. Default is 8.
#' @param filter_expr An optional expression to filter the input data.
#'
#' @return A list containing:
#'   \item{models}{A tibble with fitted models and metadata for each player}
#'   \item{trajectories}{A data frame with predicted trajectories for all players}
#'   \item{knot_info}{A summary of the knot distribution}
#'
#' @importFrom dplyr group_by filter select rename first
#' @importFrom purrr map map_chr map_lgl map_dbl pmap
#' @importFrom tidyr nest unnest
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # Process all MLB players
#' mlb_results <- process_player_trajectories(
#'   all_sports_tidy %>% filter(sport == "Baseball")
#' )
#'
#' # Plot trajectories
#' plot(mlb_results$trajectories$age, mlb_results$trajectories$predicted_value)
#' }
#'
#' @export
process_player_trajectories <- function(player_data, min_knots = 3, max_knots = 8, filter_expr = NULL) {
  # Apply filtering if provided
  filtered_data <- player_data
  if (!is.null(filter_expr)) {
    filtered_data <- eval(substitute(dplyr::filter(player_data, filter_expr)))
  }

  # Process player data with tracking of fit information
  player_models_with_info <- filtered_data %>%
    dplyr::group_by(id) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      # Store the player name for later reference
      player_name = purrr::map_chr(data, ~ dplyr::first(.x$player_name)),
      # Try to fit the model
      model_info = purrr::map(data, ~ suppressMessages(suppressWarnings(fit_player_splines(.x, min_knots = min_knots, max_knots = max_knots)))),
      # Track whether fitting was successful
      fit_success = !purrr::map_lgl(model_info, is.null),
      # Extract the model object only for successful fits
      model = purrr::map(model_info, ~ if(!is.null(.x)) .x$model else NULL),
      # Extract the number of knots for successful fits
      knots = purrr::map_dbl(model_info, ~ if(!is.null(.x)) .x$knots else NA),
      # Extract the fitting method for successful fits
      fit_method = purrr::map_chr(model_info, ~ if(!is.null(.x)) .x$method else "failed")
    )

  # Print summary statistics
  message(paste("Number of players with successful spline models:", sum(player_models_with_info$fit_success)))
  message(paste("Percentage of successful fits:", round(mean(player_models_with_info$fit_success) * 100, 1), "%"))

  # Create a dataframe of knot information for visualization
  knot_info <- player_models_with_info %>%
    dplyr::select(id, player_name, knots, fit_method, fit_success)

  message("Summary of knots distribution:")
  #print(summary(knot_info$knots))

  # Generate player trajectories
  player_trajectories <- player_models_with_info %>%
    dplyr::mutate(
      min_age = purrr::map_dbl(data, ~ min(.x$age)),
      max_age = purrr::map_dbl(data, ~ max(.x$age)),
      trajectory = purrr::pmap(
        list(model, id, min_age, max_age),
        predict_career_trajectory
      )
    ) %>%
    dplyr::select(id, trajectory) %>%
    tidyr::unnest(trajectory, names_sep = "_")

  # Rename columns for clarity
  player_trajectories <- player_trajectories %>%
    dplyr::rename(predicted_value = trajectory_predicted_value,
                  age = trajectory_age)

  # Return results as a list
  return(list(
    models = player_models_with_info,
    trajectories = player_trajectories,
    knot_info = knot_info
  ))
}
