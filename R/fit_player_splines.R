#' Fit a Smoothing Spline to Player Performance Data 
#'
#' @description
#' Creates a smoothing spline model... Version 11 performs the 'Entry Peak Rule'
#' check IMMEDIATELY after a candidate spline fit passes the [min, max] df constraint,
#' by generating predictions within the fitting loop. Only fits passing BOTH df
#' AND the entry peak rule are considered for selection based on RSS or preference order.
#' This ensures the rule is strictly enforced before any model selection occurs.
#' Uses the same conditional logic (Targeted DF Search for peaks, sequential for others)
#' and fallbacks (df-valid-only, then Method 4) as v10.
#'
#' @param player_data A data frame... (as before)
#' @param min_knots Minimum degrees of freedom... Default 3.
#' @param max_knots Maximum degrees of freedom... Default 8.
#' @param peak_ratio_threshold Numeric threshold peak detection... Default 3.0.
#' @param sharp_peak_method_weight Numeric weight multiplier... Default 5.
#' @param fallback_method4_spar Numeric spar value for the final Method 4 fallback. Default 0.4.
#' @param verbose TRUE/FALSE... Default FALSE.
#'
#' @return A list containing: ... (as before)
#'
#' @importFrom dplyr filter arrange summarise lag lead mutate if_else first last n desc pull slice_max slice_min lag group_by ungroup row_number case_when distinct between
#' @importFrom stats smooth.spline lm predict poly sd quantile weighted.mean residuals
#' @importFrom magrittr %>%
#' @importFrom tidyr drop_na
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Assume all_sports_tidy is loaded
#' drose_data <- all_sports_tidy %>% filter(grepl("errick Rose", player_name, ignore.case = TRUE))
#'
#' # Define the function below in your session first!
#' spline_fit_drose <- fit_player_splines(drose_data, verbose = TRUE) # Call the function
#'
#' # Plot D-Rose - check method. It should NOT be one failing the entry peak rule unless that's the ONLY df-valid option.
#' if (!is.null(spline_fit_drose) && spline_fit_drose$fit_success) {
#'    # ... plotting code ...
#' }
#' }
#' @export
fit_player_splines <- function(player_data, min_knots = 3, max_knots = 8,
                               peak_ratio_threshold = 3.0,
                               sharp_peak_method_weight = 5,
                               fallback_method4_spar = 0.4,
                               verbose = FALSE) {

  player_name <- if (nrow(player_data) > 0 && "player_name" %in% names(player_data)) player_data$player_name[1] else "Unknown Player"

  # --- Initial Data Cleaning and Checks ---
  player_data_orig <- player_data %>% dplyr::filter(!is.na(player_value), !is.na(age)) %>% dplyr::arrange(age)
  n_obs <- nrow(player_data_orig)
  if (n_obs < 5) { if(verbose) message("Not enough valid data points..."); return(NULL) }
  min_knots <- min(min_knots, n_obs); min_knots <- max(min_knots, 2)
  max_knots <- min(max_knots, n_obs); max_knots <- max(max_knots, min_knots)
  if(min_knots > max_knots) { if(verbose) message("Infeasible final df range..."); return(NULL) }

  # --- Stricter Sharp Peak Detection Logic (Ratio-Based) ---
  is_sharp_peak_career <- FALSE; peak_info <- NULL
  peak_detection_data <- player_data_orig %>% tidyr::drop_na(player_value)
  if(nrow(peak_detection_data) >= 5) {
    peak_info <- peak_detection_data %>% dplyr::slice_max(order_by = .data$player_value, n = 1, with_ties = FALSE)
    # (rest of detection logic as in v10...)
    if (nrow(peak_info) == 1 && !is.na(peak_info$player_value) && !is.na(peak_info$age)) { peak_val <- peak_info$player_value; peak_age <- peak_info$age; avg_non_peak <- NA_real_; non_peak_data <- peak_detection_data %>% dplyr::filter(.data$age != peak_age); if (nrow(non_peak_data) > 0) { avg_non_peak <- mean(non_peak_data$player_value, na.rm = TRUE) }; if (!is.na(avg_non_peak) && avg_non_peak > 1e-6) { if ((peak_val / avg_non_peak) >= peak_ratio_threshold) { is_sharp_peak_career <- TRUE; if(verbose) message("  Sharp peak DETECTED...") } else { if(verbose) { message("  Sharp peak NOT detected...") } } } else { if(verbose) message("  Sharp peak NOT detected...") } } else { if(verbose) message("  Could not uniquely identify peak...") }
  } else { if(verbose) message("  Not enough non-NA data points...") }

  # --- Helper Function: Check Entry Year Peak Rule ---
  # (Keep helper function exactly as in v9/v10)
  check_entry_peak_rule <- function(model_to_check, original_data, peak_info_for_rule) { # Pass peak_info
    # ... (function code as in v9/v10, using peak_info_for_rule for actual peak) ...
    if (is.null(model_to_check) || !inherits(model_to_check, "smooth.spline") || nrow(original_data) < 1 || is.null(peak_info_for_rule)) return(FALSE); entry_age <- dplyr::first(original_data$age); actual_peak_age <- peak_info_for_rule$age; pred_ages <- sort(unique(original_data$age)); if(length(pred_ages) < 2) return(FALSE); predictions <- tryCatch({ stats::predict(model_to_check, x = pred_ages) }, error = function(e){ NULL }); if(is.null(predictions) || is.null(predictions$y) || length(predictions$y) != length(pred_ages)) { if(verbose) message("    Rule Check Warning: Prediction failed."); return(FALSE) }; predicted_traj <- dplyr::tibble(age = predictions$x, predicted_value = predictions$y); predicted_peak <- predicted_traj %>% dplyr::filter(!is.na(predicted_value)) %>% dplyr::slice_max(order_by = predicted_value, n = 1, with_ties = FALSE); predicted_peak_age <- if(nrow(predicted_peak) == 1) predicted_peak$age else NA; if(is.na(predicted_peak_age) || is.na(actual_peak_age)) { if(verbose) message("    Rule Check Note: Cannot determine peak ages..."); return(TRUE) }; violates_rule <- (predicted_peak_age == entry_age) && (actual_peak_age != entry_age); passes_rule <- !violates_rule; if(verbose && violates_rule) message("    Rule Check Failed: Predicted peak at entry age (", entry_age, ") but actual peak was at age ", actual_peak_age, "."); if(verbose && passes_rule && !is.na(predicted_peak_age) && !is.na(actual_peak_age)) message("    Rule Check Passed."); return(passes_rule)
  }


  selected_fit_details <- NULL # Final selected fit passing BOTH rules
  best_fit_failed_peak_rule <- NULL # Best fit passing only df rule [3,8]
  min_rss_failed_peak_rule <- Inf

  # --- Conditional Fitting Logic ---
  if (is_sharp_peak_career) {
    # --- Strategy for Sharp Peaks: Iterate through valid DF values (WEIGHTED) ---
    if(verbose) message("-> Applying Targeted DF Search (Weighted) for Sharp Peak...")
    min_rss_sharp_valid <- Inf; best_sharp_fit_valid <- NULL; best_sharp_df_valid <- NA
    min_rss_sharp_peak_fail <- Inf; best_sharp_fit_peak_fail <- NULL; best_sharp_df_peak_fail <- NA

    peak_age_for_weighting <- peak_info$age
    weights <- rep(1, nrow(player_data_orig)); if(length(peak_age_for_weighting) == 1 && !is.na(peak_age_for_weighting)){ weights <- ifelse(dplyr::between(player_data_orig$age, peak_age_for_weighting - 1, peak_age_for_weighting + 1), sharp_peak_method_weight, 1) }

    for (target_df in min_knots:max_knots) {
      if(verbose) message("  Trying Sharp Peak Method with target df = ", target_df)
      if (target_df < 2 || target_df > n_obs) { next }

      model_fit_attempt <- tryCatch({ suppressWarnings(stats::smooth.spline(player_data_orig$age, player_data_orig$player_value, df = target_df, w = weights)) }, error = function(e) { NULL })

      if (!is.null(model_fit_attempt) && inherits(model_fit_attempt, "smooth.spline") && !is.null(model_fit_attempt$df)) {
        actual_df <- round(model_fit_attempt$df)
        # 1. Check DF validity
        if (actual_df >= min_knots && actual_df <= max_knots) {
          # 2. *** Check Entry Peak Rule IMMEDIATELY ***
          passes_peak_rule <- check_entry_peak_rule(model_fit_attempt, player_data_orig, peak_info)
          rss <- sum(weights * (stats::residuals(model_fit_attempt)^2))

          if(passes_peak_rule){
            if(verbose) message("    Fit valid (df=", actual_df, ", Peak Rule OK). Weighted RSS = ", rss)
            if (rss < min_rss_sharp_valid) { min_rss_sharp_valid <- rss; best_sharp_fit_valid <- model_fit_attempt; best_sharp_df_valid <- actual_df; if(verbose) message("      * New best fully valid fit...") }
          } else { # Failed peak rule but passed DF check
            if(verbose) message("    Fit valid (df=", actual_df, ") but FAILED Peak Rule. Weighted RSS = ", rss)
            if (rss < min_rss_sharp_peak_fail) { min_rss_sharp_peak_fail <- rss; best_sharp_fit_peak_fail <- model_fit_attempt; best_sharp_df_peak_fail <- actual_df; if(verbose) message("      * New best fit (failing peak rule)...") }
          }
        } else { if(verbose) message("    Rejected: Actual df=", actual_df, " outside validation range.") }
      } else { if(verbose) message("    Failed fitting target df = ", target_df)}
    } # End loop

    # Prioritize fully valid fit
    if (!is.null(best_sharp_fit_valid)) {
      selected_fit_details <- list( model = best_sharp_fit_valid, knots = best_sharp_df_valid, method = "sharp_peak_best_valid_peak_df", fit_success = TRUE )
      if(verbose) message("  Selected Method for Sharp Peak: sharp_peak_best_valid_peak_df (df = ", best_sharp_df_valid, ")")
    } else if (!is.null(best_sharp_fit_peak_fail)) {
      # Store the best one that failed peak rule if no fully valid one found
      best_fit_failed_peak_rule <- list( model = best_sharp_fit_peak_fail, knots = best_sharp_df_peak_fail, method = "sharp_peak_best_df_weighted" )
      min_rss_failed_peak_rule <- min_rss_sharp_peak_fail
      if(verbose) message("  Sharp Peak: No fully valid fit found. Best fit failing peak rule has df = ", best_sharp_df_peak_fail)
    } else { if(verbose) message("  Sharp Peak Targeted DF Search failed...") }

  } else {
    # --- Strategy for Non-Sharp Peaks: Try GCV -> Wiggly -> Fallback DF Search ---
    if(verbose) message("-> Applying Standard Multi-Method Fitting...")
    gcv_result <- NULL; wiggly_result <- NULL; fallback_search_result <- NULL
    gcv_failed_peak <- NULL; wiggly_failed_peak <- NULL; fallback_failed_peak <- NULL
    min_rss_fallback_peak_fail <- Inf; best_fallback_fit_peak_fail <- NULL; best_fallback_df_peak_fail <- NA

    # 1. Try GCV
    if(verbose) message("  Trying Method 2 (Balanced/Default GCV)")
    gcv_fit <- tryCatch({ suppressWarnings(stats::smooth.spline(player_data_orig$age, player_data_orig$player_value)) }, error = function(e){ NULL })
    if (!is.null(gcv_fit) && inherits(gcv_fit, "smooth.spline") && !is.null(gcv_fit$df)){
      gcv_df <- round(gcv_fit$df)
      if (gcv_df >= min_knots && gcv_df <= max_knots) {
        if (check_entry_peak_rule(gcv_fit, player_data_orig, peak_info)) { # Check peak rule
          gcv_result <- list(model=gcv_fit, knots=gcv_df, method="balanced_gcv")
        } else { gcv_failed_peak <- list(model=gcv_fit, knots=gcv_df, method="balanced_gcv") }
      } else { if(verbose) message("    Rejected GCV: df out of range") }
    } else { if(verbose) message("    Failed GCV.") }

    # 2. Try Wiggly (only if GCV was not fully valid)
    if (is.null(gcv_result)) {
      if(verbose) message("  Trying Method 3 (Wiggly, spar=0.3)")
      wiggly_fit <- tryCatch({ suppressWarnings(stats::smooth.spline(player_data_orig$age, player_data_orig$player_value, spar = 0.3)) }, error=function(e){NULL})
      if (!is.null(wiggly_fit) && inherits(wiggly_fit, "smooth.spline") && !is.null(wiggly_fit$df)){
        wiggly_df <- round(wiggly_fit$df)
        if (wiggly_df >= min_knots && wiggly_df <= max_knots) {
          if (check_entry_peak_rule(wiggly_fit, player_data_orig, peak_info)) { # Check peak rule
            wiggly_result <- list(model=wiggly_fit, knots=wiggly_df, method="wiggly_low_spar")
          } else { wiggly_failed_peak <- list(model=wiggly_fit, knots=wiggly_df, method="wiggly_low_spar") }
        } else { if(verbose) message("    Rejected Wiggly: df out of range") }
      } else { if(verbose) message("    Failed Wiggly.") }
    }

    # 3. Try Fallback DF Search (only if GCV and Wiggly were not fully valid)
    if (is.null(gcv_result) && is.null(wiggly_result)) {
      if(verbose) message("  Applying Fallback DF Search (Unweighted)...")
      min_rss_fallback_valid <- Inf; best_fallback_fit_valid <- NULL; best_fallback_df_valid <- NA
      temp_min_rss_fb_peak_fail <- Inf; temp_best_fb_fit_peak_fail <- NULL; temp_best_fb_df_peak_fail <- NA

      for (target_df in min_knots:max_knots) {
        if(verbose) message("    Trying Fallback with target df = ", target_df)
        if (target_df < 2 || target_df > n_obs) { next }
        model_fit_attempt <- tryCatch({ suppressWarnings(stats::smooth.spline(player_data_orig$age, player_data_orig$player_value, df = target_df)) }, error = function(e) { NULL })
        if (!is.null(model_fit_attempt) && inherits(model_fit_attempt, "smooth.spline") && !is.null(model_fit_attempt$df)) {
          actual_df <- round(model_fit_attempt$df)
          if (actual_df >= min_knots && actual_df <= max_knots) {
            passes_peak_rule <- check_entry_peak_rule(model_fit_attempt, player_data_orig, peak_info) # Check peak rule
            rss <- sum(stats::residuals(model_fit_attempt)^2)
            if(passes_peak_rule){
              if(verbose) message("      Fallback fit valid (df=", actual_df, ", Peak Rule OK)..."); if (rss < min_rss_fallback_valid) { min_rss_fallback_valid <- rss; best_fallback_fit_valid <- model_fit_attempt; best_fallback_df_valid <- actual_df; if(verbose) message("        * New best fully valid fallback fit...") }
            } else {
              if(verbose) message("      Fallback fit valid (df=", actual_df, ") but FAILED Peak Rule..."); if (rss < temp_min_rss_fb_peak_fail) { temp_min_rss_fb_peak_fail <- rss; temp_best_fb_fit_peak_fail <- model_fit_attempt; temp_best_fb_df_peak_fail <- actual_df; if(verbose) message("        * New best fallback fit (failing peak rule)...") }
            }
          } else { if(verbose) message("      Rejected Fallback: Actual df out of range.") }
        } else { if(verbose) message("      Failed fitting fallback df=", target_df) }
      } # End fallback loop

      if (!is.null(best_fallback_fit_valid)) {
        fallback_search_result <- list( model = best_fallback_fit_valid, knots = best_fallback_df_valid, method = "fallback_best_valid_peak_df" )
        if(verbose) message("  Selected Method via Fallback Search: fallback_best_valid_peak_df (df = ", best_fallback_df_valid, ")")
      } else if (!is.null(temp_best_fb_fit_peak_fail)) {
        # Store the best one that failed peak rule from this specific search
        fallback_failed_peak <- list( model = temp_best_fb_fit_peak_fail, knots = temp_best_fb_df_peak_fail, method = "fallback_best_df" )
        # Update overall best failing fit if this one is better
        if(temp_min_rss_fb_peak_fail < min_rss_failed_peak_rule) { best_fit_failed_peak_rule <- fallback_failed_peak; min_rss_failed_peak_rule <- temp_min_rss_fb_peak_fail }
        if(verbose) message("  Fallback Search: No fully valid fit found. Best fit failing peak rule has df = ", temp_best_fb_df_peak_fail)
      } else { if(verbose) message("  Fallback DF Search failed...") }
    }

    # --- Selection Logic for Non-Sharp Peaks (Prioritize fully valid) ---
    if (!is.null(gcv_result)) {
      selected_fit_details <- list( model = gcv_result$model, knots = gcv_result$knots, method = gcv_result$method, fit_success = TRUE )
      if(verbose) message("  Selected Method: balanced_gcv")
    } else if (!is.null(wiggly_result)) {
      selected_fit_details <- list( model = wiggly_result$model, knots = wiggly_result$knots, method = wiggly_result$method, fit_success = TRUE )
      if(verbose) message("  Selected Method: wiggly_low_spar")
    } else if (!is.null(fallback_search_result)) {
      selected_fit_details <- list( model = fallback_search_result$model, knots = fallback_search_result$knots, method = fallback_search_result$method, fit_success = TRUE )
      # message already printed
    } else {
      # *** Intermediate Fallback Selection (If no fully valid NON-SHARP PEAK fit found) ***
      if (is.null(selected_fit_details)) {
        if(verbose) message("  No fully valid spline fit found from standard methods. Checking fits that failed peak rule...")
        # Choose based on original preference order among those stored
        if (!is.null(gcv_failed_peak) && (is.null(best_fit_failed_peak_rule) || gcv_failed_peak$knots <= best_fit_failed_peak_rule$knots)) { best_fit_failed_peak_rule <- gcv_failed_peak }
        if (!is.null(wiggly_failed_peak) && (is.null(best_fit_failed_peak_rule) || wiggly_failed_peak$knots <= best_fit_failed_peak_rule$knots)) { best_fit_failed_peak_rule <- wiggly_failed_peak }
        if (!is.null(fallback_failed_peak)) { best_fit_failed_peak_rule <- fallback_failed_peak }
        # Note: best_fit_failed_peak_rule might have been set already by the sharp peak path if that was detected but failed validation fully.
      }
    }
  } # End conditional fitting logic

  # --- Intermediate Fallback (Select best failing peak rule fit if no fully valid found) ---
  if (is.null(selected_fit_details) && !is.null(best_fit_failed_peak_rule)) {
    selected_fit_details <- list( model = best_fit_failed_peak_rule$model, knots = best_fit_failed_peak_rule$knots, method = best_fit_failed_peak_rule$method, fit_success = TRUE )
    if(verbose) message("  Selected Method (Fallback): ", best_fit_failed_peak_rule$method, " (df=", best_fit_failed_peak_rule$knots, ", Failed Peak Rule)")
  }


  # --- Return Selected Fit or Try FINAL Fallback (Original Method 4) ---
  if (!is.null(selected_fit_details)) {
    return(selected_fit_details)
  } else {
    # --- FINAL Fallback: Original Method 4 Logic ---
    if(verbose) message("Attempting FINAL fallback: Original Method 4 Logic...")
    target_df_final <- min(max(n_obs - 1, min_knots), max_knots)
    if (target_df_final >= min_knots && target_df_final <= max_knots && target_df_final >= 2 && target_df_final <= n_obs) {
      if(verbose) message("  Final fallback target df = ", target_df_final, ", spar = ", fallback_method4_spar)
      final_fallback_fit <- tryCatch({ suppressWarnings(stats::smooth.spline(player_data_orig$age, player_data_orig$player_value, df = target_df_final, spar = fallback_method4_spar)) }, error = function(e) { NULL })
      if (!is.null(final_fallback_fit) && inherits(final_fallback_fit, "smooth.spline") && !is.null(final_fallback_fit$df)) {
        final_knots_used <- round(final_fallback_fit$df)
        if(final_knots_used >= min_knots && final_knots_used <= max_knots){
          # Optionally check peak rule even here, but maybe less critical
          # passes_peak_rule_final <- check_entry_peak_rule(final_fallback_fit, player_data_orig, peak_info)
          # if (!passes_peak_rule_final) { message("Warning: Final fallback failed peak rule check.") }
          if(verbose) message("  Final fallback successful (Method 4 logic). df = ", final_knots_used)
          return(list( model = final_fallback_fit, knots = final_knots_used, method = "fallback_original_method4", fit_success = TRUE ))
        } else { if(verbose) message("  Final fallback failed validation: Actual df=", final_knots_used) }
      } else { if(verbose) message("  Final fallback fit failed.") }
    } else { if(verbose) message("  Cannot attempt final fallback: Target df ", target_df_final, " invalid.") }
    # If final fallback also fails, return NULL
    if(verbose) message("All fitting methods including final fallback failed.")
    return(NULL)
  }
}
