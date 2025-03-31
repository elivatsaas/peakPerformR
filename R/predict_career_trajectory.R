#' Predict Player Career Trajectory
#'
#' @description
#' Uses a fitted spline model to predict a player's performance trajectory
#' across a range of ages. This function creates a data frame of predicted
#' values for each integer age in the range.
#'
#' @param model A fitted smooth.spline model from fit_player_spline()
#' @param id The unique player identifier
#' @param min_age The minimum age to predict from
#' @param max_age The maximum age to predict to
#'
#' @return A data frame with predicted values at each integer age in the range
#'
#' @export
predict_career_trajectory <- function(model, id, min_age, max_age) {
  # Handle NULL models
  if (is.null(model)) {
    return(NULL)
  }

  # Create integer age sequence
  ages <- min_age:max_age

  # Predict values at each age
  predictions <- stats::predict(model, ages)

  # Create data frame of predictions
  trajectory <- data.frame(
    id = rep(id, length(ages)),
    age = ages,
    predicted_value = predictions$y
  )

  return(trajectory)
}
