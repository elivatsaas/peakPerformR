# plumber.R - Production Ready API Endpoint (vFINAL_fixpath_robust_json_root_manual - Optimized Sequential)
# This version uses manual JSON serialization for the root endpoint to fix the 'Error creating serializer' issue.

# --- Load Required Libraries ---
# Ensure these are installed in the deployment environment
# install.packages(c("plumber", "jsonlite", "dplyr", "tidyr", "readr", "purrr", "here", "peakPerformR"))

suppressPackageStartupMessages({
  library(plumber)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(peakPerformR) # Your custom package - **Reminder: Needs INTERNAL FIX for 'argument is of length zero' error**
  library(here)
})
message("Required packages loaded. Custom package 'peakPerformR' loaded.")

# --- Load Pre-Computed Base Data (Load ONCE at API startup) ---
.API_ENV <- new.env(parent = emptyenv()) # Dedicated environment for API data

message("Loading pre-computed base data...")
tryCatch({
  # --- Define Path to Pre-computed Data using here() ---
  data_dir <- NULL
  if (requireNamespace("here", quietly = TRUE)) {
    # Try to find the project root and construct the path
    project_root <- tryCatch(here::here(), error = function(e) NULL)
    if (!is.null(project_root)) {
      data_dir <- file.path(project_root, "plumber", "data")
      message("Using here() package. Project root detected at: ", project_root)
      message("Attempting to load base data from directory: ", normalizePath(data_dir, mustWork = FALSE))
    } else {
      message("here() loaded, but could not determine project root. Falling back to relative path.")
    }
  }

  if (is.null(data_dir)) {
    # Fallback if 'here' is not available or fails
    data_dir <- file.path("plumber", "data")
    message("Using relative path from CWD (", getwd(), "): ", data_dir)
    message("Attempting to load base data from directory: ", normalizePath(data_dir, mustWork = FALSE))
  }

  sports_filtered_path <- file.path(data_dir, "precalc_sports_filtered.rds")
  trajectories_path <- file.path(data_dir, "precalc_player_trajectories.rds")
  clusters_path <- file.path(data_dir, "precalc_cluster_data.rds")

  required_files <- c(sports_filtered_path, trajectories_path, clusters_path)
  files_exist <- file.exists(required_files)

  if(!all(files_exist)) {
    missing_files <- required_files[!files_exist]
    # Try to normalize path for clearer error message
    normalized_data_dir <- tryCatch(normalizePath(data_dir, mustWork = FALSE), error=function(e) data_dir)
    stop(paste("CRITICAL ERROR: One or more pre-calculated data files not found:",
               paste(basename(missing_files), collapse=", "),
               "\nPlease ensure files exist in the expected directory:", normalized_data_dir,
               "\nRun the pre-computation script if necessary."))
  }

  # Load data quickly from RDS files
  .API_ENV$SPORTS_FILTERED <- readRDS(sports_filtered_path)
  .API_ENV$PLAYER_TRAJECTORIES <- readRDS(trajectories_path)
  .API_ENV$CLUSTER_DATA <- readRDS(clusters_path)

  # Basic validation of loaded data
  stopifnot(
    "`SPORTS_FILTERED` did not load as a non-empty data frame." = is.data.frame(.API_ENV$SPORTS_FILTERED) && nrow(.API_ENV$SPORTS_FILTERED) > 0,
    "`PLAYER_TRAJECTORIES` did not load as a non-empty data frame." = is.data.frame(.API_ENV$PLAYER_TRAJECTORIES) && nrow(.API_ENV$PLAYER_TRAJECTORIES) > 0,
    "`CLUSTER_DATA` did not load as a non-empty data frame." = is.data.frame(.API_ENV$CLUSTER_DATA) && nrow(.API_ENV$CLUSTER_DATA) > 0
  )

  message("Base data loaded and validated successfully.")

}, error = function(e) {
  message("CRITICAL ERROR during base data loading: ")
  message(conditionMessage(e))
  # Stop the plumber process if essential base data fails
  stop("API cannot start due to failure in loading base data. Check logs, paths, and pre-computation script output.")
})


# --- Plumber Filters ---
#* Log incoming requests
#* @filter logger
function(req){
  cat(as.character(Sys.time()), "-", req$REQUEST_METHOD, req$PATH_INFO, "-", req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

# --- API Endpoints ---

#* Recalculate Primes, PQI, CQI based on thresholds
#* @param req The request object
#* @param res The response object
#* @post /recalculate
#* @serializer contentType list(type="application/json") # Manual serialization below
function(req, res) {

  # Default parameter values & initialize results structure
  default_threshold_pct <- 70.0
  default_games_pct_threshold <- 100.0
  results <- list( success = FALSE, message = "Initialization error.", parameters = list(), rawPrimes = list(), splinePrimes = list(), pqi = list(), cqi = list(), fullData = list() )

  # Extract and validate parameters
  params <- tryCatch({
    # Handle empty body gracefully
    if (is.null(req$postBody) || req$postBody == "") '{}' else req$postBody
  } |> jsonlite::fromJSON(simplifyDataFrame = FALSE),
  error = function(e) {
    message("API Warning: Failed to parse request body JSON. Using defaults. Error: ", conditionMessage(e))
    list() # Return empty list on parse error
  })

  threshold_pct <- as.numeric(params$thresholdPct %||% default_threshold_pct)
  games_pct_threshold <- as.numeric(params$gamesPctThreshold %||% default_games_pct_threshold)
  current_params <- list(thresholdPct = threshold_pct, gamesPctThreshold = games_pct_threshold)
  results$parameters <- current_params # Store parameters early for error responses

  # Input validation checks
  param_errors <- character(0) # Vector to hold multiple errors
  if (length(threshold_pct) != 1 || is.na(threshold_pct) || !is.numeric(threshold_pct) || threshold_pct < 0 || threshold_pct > 100) {
    param_errors <- c(param_errors, "Invalid or missing 'thresholdPct' (must be numeric between 0 and 100).")
  }
  if (length(games_pct_threshold) != 1 || is.na(games_pct_threshold) || !is.numeric(games_pct_threshold) || games_pct_threshold < 0 || games_pct_threshold > 100) {
    param_errors <- c(param_errors, "Invalid or missing 'gamesPctThreshold' (must be numeric between 0 and 100).")
  }

  if (length(param_errors) > 0) {
    res$status <- 400 # Bad Request
    results$message <- paste(param_errors, collapse = " ")
    message("API Error: Invalid input parameters: ", results$message)
    # Manually serialize because Plumber serializer might not run on early exit
    res$body <- jsonlite::toJSON(results, auto_unbox = TRUE, na = "null")
    return(res) # Return the response object directly
  }

  message(sprintf("API: Received valid request: thresholdPct=%.1f, gamesPctThreshold=%.1f", threshold_pct, games_pct_threshold))
  api_start_time <- Sys.time()

  # --- Perform Calculations SEQUENTIALLY with Error Handling ---
  calculation_result <- tryCatch({
    # Access pre-loaded base data from dedicated environment
    base_sports_data <- .API_ENV$SPORTS_FILTERED
    base_trajectory_data <- .API_ENV$PLAYER_TRAJECTORIES
    base_cluster_data <- .API_ENV$CLUSTER_DATA

    # --- Step 1: Prime Identification ---
    message("API: Preparing data for spline prime identification...")
    spline_join_data <- base_sports_data |>
      dplyr::select(id, league, position, age, games_played) |>
      dplyr::distinct(id, age, .keep_all = TRUE)
    stopifnot("Spline join data preparation failed unexpectedly." = is.data.frame(spline_join_data))
    spline_input_data <- base_trajectory_data |>
      dplyr::left_join(spline_join_data, by = c("id", "age"))
    stopifnot("Joining trajectory and sports data for spline input failed." = is.data.frame(spline_input_data))

    message("API: Identifying spline primes (calling peakPerformR::identify_prime)...")
    # ** POTENTIAL ERROR LOCATION (needs fix inside peakPerformR) **
    new_spline_primes_df <- peakPerformR::identify_prime(
      spline_input_data, method = "predicted",
      threshold_pct = threshold_pct, games_pct_threshold = games_pct_threshold
    )
    stopifnot("peakPerformR::identify_prime (method='predicted') failed to return a data frame." = is.data.frame(new_spline_primes_df))
    message("API: Spline prime identification call complete.")

    message("API: Identifying raw primes (calling peakPerformR::identify_prime)...")
    # ** POTENTIAL ERROR LOCATION (needs fix inside peakPerformR) **
    new_raw_primes_df <- peakPerformR::identify_prime(
      base_sports_data, method = "actual",
      threshold_pct = threshold_pct, games_pct_threshold = games_pct_threshold
    )
    stopifnot("peakPerformR::identify_prime (method='actual') failed to return a data frame." = is.data.frame(new_raw_primes_df))
    message("API: Raw prime identification call complete.")

    # --- Step 2: Dependent Calculations ---
    message("API: Creating performance dataset (calling peakPerformR::create_player_performance_dataset)...")
    temp_player_data <- peakPerformR::create_player_performance_dataset(
      sports_data = base_sports_data, prime_data = new_spline_primes_df,
      cluster_data = base_cluster_data
    )
    stopifnot("peakPerformR::create_player_performance_dataset failed to return a data frame." = is.data.frame(temp_player_data))

    message("API: Processing prime metrics (calling peakPerformR::process_player_primes)...")
    temp_processed_data <- peakPerformR::process_player_primes(
      temp_player_data, base_sports_data
    )
    stopifnot("peakPerformR::process_player_primes failed to return a data frame." = is.data.frame(temp_processed_data))

    message("API: Updating full data with 'in_prime' flag (based on raw primes)...")
    updated_full_data_df <- base_sports_data # Start with a fresh copy
    # Check if raw primes data is usable before attempting the join/update
    if (nrow(new_raw_primes_df) > 0 && all(c("id", "start_age", "end_age") %in% names(new_raw_primes_df))) {
      # Using a join is generally more efficient and robust than mapply for large data
      primes_to_join <- new_raw_primes_df %>%
        dplyr::select(id, prime_start_age = start_age, prime_end_age = end_age) %>%
        # Handle potential multiple prime periods per player if identify_prime could return that
        # Assuming one prime period for now based on original mapply logic
        dplyr::distinct(id, .keep_all = TRUE)

      updated_full_data_df <- updated_full_data_df %>%
        dplyr::left_join(primes_to_join, by = "id") %>%
        dplyr::mutate(
          in_prime = !is.na(prime_start_age) & !is.na(prime_end_age) & !is.na(age) &
            age >= prime_start_age & age <= prime_end_age
        ) %>%
        dplyr::select(-prime_start_age, -prime_end_age) # Remove temporary join columns
      message("API: 'in_prime' flag updated using left join.")

    } else {
      message("API Warning: Raw primes data is empty or missing required columns (id, start_age, end_age). Setting 'in_prime' to FALSE for all rows.")
      updated_full_data_df <- updated_full_data_df |> dplyr::mutate(in_prime = FALSE)
    }
    stopifnot("Updating 'in_prime' flag failed to produce a data frame." = is.data.frame(updated_full_data_df))


    # --- Step 3: PQI Calculation ---
    message("API: Calculating PQI (calling peakPerformR::calculate_prime_quality_index)...")
    # ** POTENTIAL ERROR LOCATION (needs fix inside peakPerformR) **
    pqi_calculated_df <- peakPerformR::calculate_prime_quality_index(
      temp_processed_data,
      nfl_by_position = TRUE,
      tier_method = "percentile"
    )
    stopifnot("peakPerformR::calculate_prime_quality_index failed to return a data frame." = is.data.frame(pqi_calculated_df))
    # Safely rename/select columns only if they exist
    if (nrow(pqi_calculated_df) > 0 && all(c("prime_tier", "pqi_score") %in% names(pqi_calculated_df))) {
      pqi_calculated_df <- pqi_calculated_df |>
        dplyr::mutate(selected_tier = prime_tier, pqi_selected = pqi_score) |>
        dplyr::select(-prime_tier, -pqi_score)
    } else {
      message("API Warning: PQI result empty or missing expected columns ('prime_tier', 'pqi_score'). Skipping rename.")
      # Ensure expected columns exist even if empty, add if missing
      if (!"selected_tier" %in% names(pqi_calculated_df)) pqi_calculated_df$selected_tier <- NA
      if (!"pqi_selected" %in% names(pqi_calculated_df)) pqi_calculated_df$pqi_selected <- NA_real_
    }


    # --- Step 4: CQI Calculation ---
    message("API: Preparing data for CQI calculation...")
    # Select minimal necessary columns to reduce join complexity/memory
    base_traj_minimal <- base_trajectory_data %>% dplyr::select(id, age, predicted_value)
    processed_minimal <- temp_processed_data %>% dplyr::select(id, career_avg_tier) %>% dplyr::distinct(id, .keep_all=TRUE)

    # Ensure required columns for join exist in updated_full_data_df
    stopifnot("Required columns 'id', 'age' missing from data before CQI joins." = all(c("id", "age") %in% names(updated_full_data_df)))

    data_for_cqi <- updated_full_data_df |>
      dplyr::left_join(base_traj_minimal, by = c("id","age")) |> # Check relationship if multiple predictions per age exist
      dplyr::left_join(processed_minimal, by = "id") # Assumes one career_avg_tier per id

    stopifnot("Data preparation for CQI via joins failed." = is.data.frame(data_for_cqi))

    message("API: Calculating CQI (calling peakPerformR::calculate_career_quality_index)...")
    # ** POTENTIAL ERROR LOCATION (needs fix inside peakPerformR) **
    cqi_calculated_df <- peakPerformR::calculate_career_quality_index(
      player_data = data_for_cqi,
      nfl_by_position = FALSE,
      tier_method = "percentile",
      exclude_positions = c("OL", "SPEC"),
      min_seasons = 5
    )
    stopifnot("peakPerformR::calculate_career_quality_index failed to return a data frame." = is.data.frame(cqi_calculated_df))
    # Safely rename/select columns only if they exist
    if (nrow(cqi_calculated_df) > 0 && all(c("career_tier", "cqi_score") %in% names(cqi_calculated_df))) {
      cqi_calculated_df <- cqi_calculated_df |>
        dplyr::mutate(selected_tier = career_tier, cqi_selected = cqi_score) |>
        dplyr::select(-career_tier, -cqi_score)
    } else {
      message("API Warning: CQI result empty or missing expected columns ('career_tier', 'cqi_score'). Skipping rename.")
      # Ensure expected columns exist even if empty, add if missing
      if (!"selected_tier" %in% names(cqi_calculated_df)) cqi_calculated_df$selected_tier <- NA
      if (!"cqi_selected" %in% names(cqi_calculated_df)) cqi_calculated_df$cqi_selected <- NA_real_
    }
    message("API: PQI/CQI calculation calls complete.")

    # --- Step 5: Final Filtering (Consistency between PQI/CQI) ---
    message("API: Filtering PQI and CQI results for consistency...")
    pqi_final_df <- pqi_calculated_df # Default to unfiltered
    cqi_final_df <- cqi_calculated_df # Default to unfiltered
    req_cols <- c("id", "position")
    # Check if filtering is possible and makes sense
    can_filter <- nrow(pqi_calculated_df) > 0 && nrow(cqi_calculated_df) > 0 &&
      all(req_cols %in% names(pqi_calculated_df)) && all(req_cols %in% names(cqi_calculated_df))

    if (can_filter) {
      # Ensure position column exists before filtering on it
      pqi_filtered <- pqi_calculated_df %>% dplyr::filter(!is.na(position) & position != "SPEC")
      cqi_filtered <- cqi_calculated_df %>% dplyr::filter(!is.na(position) & position != "SPEC")

      common_ids <- intersect(pqi_filtered$id, cqi_filtered$id)

      pqi_final_df <- pqi_filtered %>% dplyr::filter(id %in% common_ids)
      cqi_final_df <- cqi_filtered %>% dplyr::filter(id %in% common_ids)
      message(sprintf("API: Filtering applied. Final PQI rows: %d, Final CQI rows: %d", nrow(pqi_final_df), nrow(cqi_final_df)))
    } else {
      message("API Warning: Skipping final PQI/CQI filtering step because input dataframes are empty, missing required columns ('id', 'position'), or filtering is not applicable.")
    }

    api_end_time <- Sys.time()
    message(sprintf("API: Calculation sequence successful. Duration: %.2f seconds", difftime(api_end_time, api_start_time, units = "secs")))

    # --- Return successful results list ---
    list(
      success = TRUE,
      message = "Recalculation successful.",
      parameters = current_params, # Include parameters in the success response
      rawPrimes = new_raw_primes_df,
      splinePrimes = new_spline_primes_df,
      pqi = pqi_final_df,
      cqi = cqi_final_df,
      fullData = updated_full_data_df
    )

  }, error = function(e) { # Catch errors from ANY step inside the tryCatch
    # Log the error clearly on the server side
    error_message <- paste("API ERROR during calculation:", conditionMessage(e))
    message(error_message)
    # Capture and log traceback if possible
    tb <- tryCatch(traceback(), error = function(e_tb) "Traceback not available")
    message("Traceback:\n", paste(capture.output(tb), collapse = "\n"))

    # Return the error object itself, it will be handled below
    return(e)
  }) # End main tryCatch block

  # --- Prepare Final Response ---
  final_response_list <- list()

  if (inherits(calculation_result, "error")) {
    # Set HTTP status to 500 Internal Server Error
    res$status <- 500
    # Populate the pre-defined error structure
    results$success <- FALSE
    # Provide a user-friendly message, but log the detailed one
    results$message <- paste("Internal server error during calculation. Check server logs for details. Error context:", calculation_result$message)
    # Clear data fields in error response
    results$rawPrimes <- list()
    results$splinePrimes <- list()
    results$pqi <- list()
    results$cqi <- list()
    results$fullData <- list()
    final_response_list <- results

  } else {
    # Calculation was successful, use the list returned by the tryCatch block
    final_response_list <- calculation_result
    # Status defaults to 200 OK
  }

  # Final check: Ensure the result is a list suitable for JSON conversion
  if (!is.list(final_response_list) || is.null(names(final_response_list))) {
    message("API CRITICAL Error: Final result structure is invalid before JSON serialization.");
    res$status <- 500
    final_response_list <- list(
      success = FALSE,
      message = "Internal server error: Invalid response structure generated.",
      parameters = current_params, # Still include parameters if available
      rawPrimes = list(), splinePrimes = list(), pqi = list(), cqi = list(), fullData = list()
    )
  }

  # --- Manual JSON Serialization with Final Error Check ---
  response_body <- tryCatch({
    jsonlite::toJSON(final_response_list, auto_unbox = TRUE, na = "null", pretty = FALSE)
  }, error = function(e) {
    error_msg_json <- "Internal server error: Failed to serialize results to JSON."
    message("API CRITICAL Error: ", error_msg_json, " JSON Error: ", conditionMessage(e));
    res$status <- 500 # Ensure status is 500 if serialization fails
    # Manually create a minimal error JSON string
    sprintf('{"success": false, "message": "%s", "parameters": %s}',
            error_msg_json,
            jsonlite::toJSON(current_params, auto_unbox = TRUE, na = "null") # Include params if possible
    )
  })

  message("API: Sending response (Status: ", res$status %||% 200, ").") # Log final status
  res$body <- response_body # Assign JSON string to the response body
  return(res) # Return the response object
}


#* API Status / Health Check
#* @get /
#* @serializer contentType list(type="application/json") # Use contentType for manual serialization
function(res){ # Ensure 'res' parameter is present
  data_loaded <- exists(".API_ENV", envir = .GlobalEnv) &&
    inherits(.API_ENV$SPORTS_FILTERED, "data.frame") &&
    inherits(.API_ENV$PLAYER_TRAJECTORIES, "data.frame") &&
    inherits(.API_ENV$CLUSTER_DATA, "data.frame") &&
    nrow(.API_ENV$SPORTS_FILTERED) > 0

  response_list <- list() # Initialize response list

  if (data_loaded) {
    res$status <- 200 # Explicitly set OK status
    response_list <- list(
      status = "OK",
      message = "API is running and base data is loaded.",
      endpoints = list(
        recalculate = list(method = "POST", path = "/recalculate", description = "Recalculates metrics based on thresholds.")
      )
    )
  } else {
    res$status <- 503 # Service Unavailable
    response_list <- list(
      status = "Error",
      message = "API is running BUT critical base data failed to load or is invalid. Service may be impaired. Check server logs."
    )
  }

  # Manually serialize the list to JSON and assign to body
  res$body <- jsonlite::toJSON(response_list, auto_unbox = TRUE, na = "null")

  # Return the response object
  return(res)
}


# --- Plumber Run/Deploy Reminders ---
# Local:
# pr <- plumber::plumb("plumber/plumber.R")
# pr$run(host = "0.0.0.0", port = 8000, swagger = TRUE) # swagger=TRUE enables /__docs__/ UI

# Deploy:
#
# rsconnect::deployApp(
#   appDir = "plumber",
#   appName = "peakperformr-api",
#   account = "elivatsaas",
#   server = "shinyapps.io",
#   appMode = "api"
# )
