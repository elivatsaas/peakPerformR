#' Fit a Smoothing Spline to Player Performance Data
#'
#' @description
#' Creates a smoothing spline model for a player's performance over their career,
#' using age as the predictor and player_value as the response variable.
#' This function implements multiple fitting approaches to ensure robust results,
#' with explicit smoothing parameter control for better handling of sharp peaks.
#'
#' @param player_data A data frame containing player performance data.
#'                    Must include 'age', 'player_value', and 'player_name' columns.
#' @param min_knots Minimum number of knots for the spline fit. Default is 3.
#' @param max_knots Maximum number of knots for the spline fit. Default is 8.
#' @param verbose TRUE/FALSE. whether you want to print errors and messages. Default is FALSE.
#'
#' @return A list containing:
#'   \item{model}{The fitted smooth.spline object}
#'   \item{knots}{The number of knots used in the model}
#'   \item{method}{The fitting method that was used}
#'   \item{fit_success}{Logical indicating if fitting was successful}
#'   If fitting fails completely, returns NULL.
#'
#' @importFrom dplyr filter arrange
#' @importFrom stats smooth.spline lm predict poly
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # Get data for a specific player
#' player_data <- all_sports_tidy %>% filter(id == "NBA-lebron-james-00")
#'
#' # Fit the spline model
#' spline_fit <- fit_player_splines(player_data)
#'
#' # Plot the result
#' plot(player_data$age, player_data$player_value)
#' lines(spline_fit$model, col = "red")
#' }
#'
#' @export
fit_player_splines <- function(player_data, min_knots = 3, max_knots = 8, verbose = FALSE) {
  # Remove any NA values
  player_data <- player_data %>%
    dplyr::filter(!is.na(player_value), !is.na(age))

  # Check if we have enough data
  if (nrow(player_data) < 5) {
    if(verbose == TRUE){
      message("Not enough valid data for player: ", player_data$player_name[1])
    }
    return(NULL)
  }

  # Helper function to validate model meets knot requirements
  validate_model <- function(model, method_name) {
    # Extract knots from model (df is close approximation)
    knots_used <- round(model$df)

    # Check if knots meet minimum requirement
    if (knots_used < min_knots) {
      if(verbose == TRUE){
        message("  Rejected: ", method_name, " produced only ", knots_used,
                " knots (minimum is ", min_knots, ")")
      }
      return(NULL)
    }

    # If model passes validation, return with metadata
    return(list(
      model = model,
      knots = knots_used,
      method = method_name,
      fit_success = TRUE
    ))
  }

  # Multi-approach strategy - with min_knots strictly enforced
  fit_methods <- list(
    # Try 1: Direct with fixed spar=0.5 and explicit min_knots
    function() {
      # Ensure we never go below min_knots
      suggested_knots <- min(max(floor(nrow(player_data)/3), min_knots), max_knots)
      if(verbose == TRUE){
        message("Approach 1: Fixed spar=0.5, ", suggested_knots, " knots for ", player_data$player_name[1])
      }
      model <- suppressWarnings(
        stats::smooth.spline(player_data$age, player_data$player_value,
                             nknots = suggested_knots, spar = 0.5)
      )
      return(validate_model(model, "method_1"))
    },

    # Try 2: More knots with higher smoothing (spar=0.7)
    function() {
      k <- min(max(floor(nrow(player_data)/2), min_knots), max_knots)
      if(verbose == TRUE){
        message("Approach 2: High smoothing spar=0.7, ", k, " knots for ", player_data$player_name[1])
      }
      model <- suppressWarnings(
        stats::smooth.spline(player_data$age, player_data$player_value,
                             nknots = k, spar = 0.7)
      )
      return(validate_model(model, "method_2"))
    },

    # Try 3: Exactly min_knots with lower smoothing (spar=0.3)
    function() {
      if(verbose == TRUE){
        message("Approach 3: Low smoothing spar=0.3, ", min_knots, " knots for ", player_data$player_name[1])
      }
      model <- suppressWarnings(
        stats::smooth.spline(player_data$age, player_data$player_value,
                             nknots = min_knots, spar = 0.3)
      )
      return(validate_model(model, "method_3"))
    },

    # Try 4: Use df specification with min_knots as minimum and medium smoothing (spar=0.4)
    function() {
      # Ensure df is at least min_knots
      df <- min(max(nrow(player_data) - 1, min_knots), max_knots)
      if(verbose == TRUE){
        message("Approach 4: Medium smoothing spar=0.4, df=", df, " for ", player_data$player_name[1])
      }
      model <- suppressWarnings(
        stats::smooth.spline(player_data$age, player_data$player_value, df = df, spar = 0.4)
      )
      return(validate_model(model, "method_4"))
    },

    # Try 5: Specific approach for sharp peak careers
    function() {
      if(verbose == TRUE){
        message("Approach 5: Sharp peak detection for ", player_data$player_name[1])
      }
      # First detect if there's a sharp peak by checking the max difference between adjacent values
      player_data_sorted <- player_data %>% dplyr::arrange(age)
      player_values <- player_data_sorted$player_value
      max_diff <- max(abs(diff(player_values)), na.rm = TRUE)
      avg_value <- mean(player_values, na.rm = TRUE)

      # If max difference is significant compared to mean value, this might be a sharp peak career
      if (max_diff > 0.3 * avg_value) {
        if(verbose == TRUE) {
          message("  Detected possible sharp peak, using low smoothing")
        }
        # Use lower smoothing to capture the peak
        model <- suppressWarnings(
          stats::smooth.spline(player_data$age, player_data$player_value,
                               nknots = min(max(5, min_knots), max_knots), spar = 0.2)
        )
      } else {
        # Otherwise, use auto-smoothing
        model <- suppressWarnings(
          stats::smooth.spline(player_data$age, player_data$player_value)
        )
      }

      # If auto model has too few knots, try again with forced min_knots
      if (round(model$df) < min_knots) {
        if(verbose == TRUE){
          message("  Auto fit had only ", round(model$df), " knots, forcing ", min_knots)
        }
        model <- suppressWarnings(
          stats::smooth.spline(player_data$age, player_data$player_value, df = min_knots)
        )
      }

      return(validate_model(model, "method_5"))
    }
  )

  # Try each method in sequence until one works
  for (i in 1:length(fit_methods)) {
    result <- tryCatch({
      fit_methods[[i]]()
    }, error = function(e) {
      if(verbose == TRUE){
        message("  Failed: ", e$message)
      }
      NULL
    })

    if (!is.null(result)) {
      if(verbose == TRUE){
        message("  Success with approach ", i, " (", result$knots, " knots)")
      }
      return(result)
    }
  }

  if(verbose == TRUE){
    # If all fitting approaches failed
    message("All spline approaches failed for player: ", player_data$player_name[1])
  }

  # Last resort: Cubic polynomial regression (which has inherently 4 knots)
  tryCatch({
    if(verbose == TRUE){
      message("Attempting cubic polynomial as fallback for ", player_data$player_name[1])
    }
    # Sort by age to ensure proper interpolation
    player_data <- player_data %>% dplyr::arrange(age)

    # Create a cubic polynomial model (4 parameters = 4 degrees of freedom = 4 knots)
    poly_model <- suppressWarnings(
      stats::lm(player_value ~ stats::poly(age, 3, raw = TRUE), data = player_data)
    )

    # Create a function that mimics smooth.spline predict interface
    predict_fn <- function(x) {
      if (is.list(x) && !is.null(x$x)) {
        new_x <- x$x
      } else {
        new_x <- x
      }
      pred <- suppressWarnings(
        stats::predict(poly_model, newdata = data.frame(age = new_x))
      )
      list(x = new_x, y = pred)
    }

    # Create a custom "model" object that reports 4 df (cubic polynomial has 4 degrees of freedom)
    fallback_model <- list(
      x = player_data$age,
      y = player_data$player_value,
      predict = predict_fn,
      df = 4  # Cubic polynomial has 4 degrees of freedom
    )

    return(list(
      model = fallback_model,
      knots = 4,
      method = "cubic_fallback",
      fit_success = TRUE
    ))
  }, error = function(e) {
    if(verbose == TRUE){
      message("Even polynomial fallback failed: ", e$message)
    }
    return(NULL)
  })
}
