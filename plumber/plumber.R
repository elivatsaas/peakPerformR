# plumber.R - Production Ready API Endpoint
# v2023-04-06 - Simplified paths, robust checks, runtime directive

# --- Load Required Libraries ---
# Ensure these are installed in the deployment environment via renv
suppressPackageStartupMessages({
  library(plumber)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(peakPerformR) # Your custom package - **Reminder: Needs INTERNAL FIX for 'argument is of length zero' error**
  # library(here) # Removed dependency for simpler deployment pathing
})
message("Required packages loaded. Custom package 'peakPerformR' loaded.")

# --- Utility Functions ---
# Helper for safe defaulting (returns NULL if var is NULL, otherwise default)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# --- Load Pre-Computed Base Data (Load ONCE at API startup) ---
.API_ENV <- new.env(parent = emptyenv()) # Dedicated environment for API data

message("Loading pre-computed base data...")
tryCatch({
  # --- Define Path to Pre-computed Data RELATIVE TO SCRIPT LOCATION ---
  # Assumes deployment structure places this script in 'plumber/' and data in 'plumber/data/'
  # When deploying appDir = "plumber", the working dir on the server for this script
  # should be the 'plumber' directory itself.
  data_dir <- "data"
  message("Using relative path presumed from script location: ", data_dir)
  normalized_base_path <- tryCatch(normalizePath(getwd(), mustWork = FALSE), error=function(e) getwd())
  message("Attempting to load base data relative to CWD: ", normalized_base_path)

  sports_filtered_path <- file.path(data_dir, "precalc_sports_filtered.rds")
  trajectories_path <- file.path(data_dir, "precalc_player_trajectories.rds")
  clusters_path <- file.path(data_dir, "precalc_cluster_data.rds")

  required_files <- c(sports_filtered_path, trajectories_path, clusters_path)
  files_exist <- file.exists(required_files)

  if(!all(files_exist)) {
    missing_files <- required_files[!files_exist]
    normalized_data_dir_path <- tryCatch(normalizePath(file.path(normalized_base_path, data_dir), mustWork = FALSE), error=function(e) file.path(normalized_base_path, data_dir))
    stop(paste("CRITICAL ERROR: One or more pre-calculated data files not found:",
               paste(basename(missing_files), collapse=", "),
               "\nExpected location relative to CWD:", normalized_data_dir_path,
               "\nCurrent working directory reported as:", normalized_base_path,
               "\nPlease ensure files exist and check deployment structure.",
               "\nRun the pre-computation script if necessary."))
  }

  # Load data quickly from RDS files
  .API_ENV$SPORTS_FILTERED <- readRDS(sports_filtered_path)
  .API_ENV$PLAYER_TRAJECTORIES <- readRDS(trajectories_path)
  .API_ENV$CLUSTER_DATA <- readRDS(clusters_path)

  # Basic validation of loaded data
  stopifnot(
    "`SPORTS_FILTERED` did not load as a non-empty data frame." = inherits(.API_ENV$SPORTS_FILTERED, "data.frame") && nrow(.API_ENV$SPORTS_FILTERED) > 0,
    "`PLAYER_TRAJECTORIES` did not load as a non-empty data frame." = inherits(.API_ENV$PLAYER_TRAJECTORIES, "data.frame") && nrow(.API_ENV$PLAYER_TRAJECTORIES) > 0,
    "`CLUSTER_DATA` did not load as a non-empty data frame." = inherits(.API_ENV$CLUSTER_DATA, "data.frame") && nrow(.API_ENV$CLUSTER_DATA) > 0
  )

  message("Base data loaded and validated successfully.")

}, error = function(e) {
  message("CRITICAL ERROR during base data loading: ")
  message(conditionMessage(e))
  # Capture the error to prevent API from starting completely broken
  .API_ENV$LOAD_ERROR <- e
  # Stop the plumber process if essential base data fails? Or let health check report it?
  # Let health check report it for now, but log clearly.
  message("API WILL LIKELY FAIL: Base data loading failed. Check logs, paths, and pre-computation script.")
  # stop("API cannot start due to failure in loading base data.") # Alternative: Hard stop
})


# --- Plumber Filters ---
#* Log incoming requests
#* @filter logger
function(req){
  start_time <- Sys.time()
  cat(format(start_time), "-", req$REQUEST_METHOD, req$PATH_INFO, "-", req$HTTP_USER_AGENT %||% "Unknown", "@", req$REMOTE_ADDR %||% "Unknown", "\n")

  # Add handler to log response status and duration
  plumber::forward()
  # Code here runs *after* the endpoint finishes
  end_time <- Sys.time()
  duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 3)
  status <- req$pr$res$status %||% 200 # Get status from response object if available
  cat(format(end_time), "-", req$REQUEST_METHOD, req$PATH_INFO, "- Status:", status, "- Duration:", duration, "s\n")
}

# --- API Endpoints ---

#* Recalculate Primes, PQI, CQI based on thresholds
#* @param req The request object
#* @param res The response object
#* @post /recalculate
#* @serializer contentType list(type="application/json") # Manual serialization below
function(req, res) {
  endpoint_start_time <- Sys.time()
  message("API: /recalculate endpoint invoked.")

  # Check if base data loaded correctly
  if (exists("LOAD_ERROR", envir = .API_ENV)) {
    res$status <- 503 # Service Unavailable
    err_msg <- paste("Service Unavailable: Critical base data failed to load during API startup. Please check server logs. Original error:",
                     conditionMessage(.API_ENV$LOAD_ERROR))
    message("API Error: /recalculate cannot proceed due to data load failure.")
    res$body <- jsonlite::toJSON(list(success = FALSE, message = err_msg, parameters = list()), auto_unbox = TRUE, na = "null")
    return(res)
  }

  # Default parameter values & initialize results structure
  default_threshold_pct <- 70.0
  default_games_pct_threshold <- 100.0
  # Initialize results structure clearly
  results <- list(
    success = FALSE,
    message = "Initialization error.",
    parameters = list(),
    dataSummary = list(
      rawPrimesCount = NA_integer_,
      splinePrimesCount = NA_integer_,
      pqiCount = NA_integer_,
      cqiCount = NA_integer_,
      fullDataRows = NA_integer_
    ),
    # Keep data fields separate for clarity, initially empty lists
    rawPrimes = list(),
    splinePrimes = list(),
    pqi = list(),
    cqi = list(),
    fullData = list() # Consider omitting fullData by default unless requested? For now, include.
  )

  # Extract and validate parameters
  params <- tryCatch({
    if (is.null(req$postBody) || req$postBody == "") {
      message("API Info: Empty request body received. Using default parameters.")
      '{}' # Return empty JSON string if body is null/empty
    } else {
      req$postBody
    }
  } |> jsonlite::fromJSON(simplifyDataFrame = FALSE),
  error = function(e) {
    message("API Warning: Failed to parse request body JSON. Using defaults. Error: ", conditionMessage(e))
    list() # Return empty list on parse error
  })

  threshold_pct <- as.numeric(params$thresholdPct %||% default_threshold_pct)
  games_pct_threshold <- as.numeric(params$gamesPctThreshold %||% default_games_pct_threshold)
  current_params <- list(thresholdPct = threshold_pct, gamesPctThreshold = games_pct_threshold)
  results$parameters <- current_params # Store parameters early

  # Input validation checks
  param_errors <- character(0)
  if (length(threshold_pct) != 1 || is.na(threshold_pct) || !is.numeric(threshold_pct) || threshold_pct < 0 || threshold_pct > 100) {
    param_errors <- c(param_errors, "Invalid or missing 'thresholdPct' (must be numeric between 0 and 100).")
  }
  if (length(games_pct_threshold) != 1 || is.na(games_pct_threshold) || !is.numeric(games_pct_threshold) || games_pct_threshold < 0 || games_pct_threshold > 100) {
    param_errors <- c(param_errors, "Invalid or missing 'gamesPctThreshold' (must be numeric between 0 and 100).")
  }

  if (length(param_errors) > 0) {
    res$status <- 400 # Bad Request
    results$message <- paste("Input validation failed:", paste(param_errors, collapse = " "))
    message("API Error: Invalid input parameters: ", results$message)
    res$body <- jsonlite::toJSON(results, auto_unbox = TRUE, na = "null") # Use the initialized structure
    return(res)
  }

  message(sprintf("API: Received valid request: thresholdPct=%.1f, gamesPctThreshold=%.1f", threshold_pct, games_pct_threshold))
  calculation_start_time <- Sys.time()

  # --- Perform Calculations SEQUENTIALLY with Error Handling ---
  calculation_result <- tryCatch({
    # Access pre-loaded base data from dedicated environment
    base_sports_data <- .API_ENV$SPORTS_FILTERED
    base_trajectory_data <- .API_ENV$PLAYER_TRAJECTORIES
    base_cluster_data <- .API_ENV$CLUSTER_DATA
    message("API: Base data retrieved from API environment.")

    # --- Step 1: Prime Identification ---
    message("API: Starting Step 1: Prime Identification...")
    # Spline Primes
    spline_join_data <- base_sports_data |>
      dplyr::select(id, league, position, age, games_played) |>
      dplyr::distinct(id, age, .keep_all = TRUE) # Ensure uniqueness per player-age
    stopifnot("Spline join data prep failed (expected data frame)." = is.data.frame(spline_join_data))
    spline_input_data <- base_trajectory_data |>
      dplyr::left_join(spline_join_data, by = c("id", "age"))
    stopifnot("Joining trajectory/sports data for spline input failed." = is.data.frame(spline_input_data))

    message("API: Identifying spline primes (peakPerformR::identify_prime)...")
    new_spline_primes_df <- peakPerformR::identify_prime(
      spline_input_data, method = "predicted",
      threshold_pct = threshold_pct, games_pct_threshold = games_pct_threshold
    )
    stopifnot("peakPerformR::identify_prime (method='predicted') failed to return a data frame." = is.data.frame(new_spline_primes_df))
    message(sprintf("API: Spline prime identification complete. Found %d prime periods.", nrow(new_spline_primes_df)))

    # Raw Primes
    message("API: Identifying raw primes (peakPerformR::identify_prime)...")
    new_raw_primes_df <- peakPerformR::identify_prime(
      base_sports_data, method = "actual",
      threshold_pct = threshold_pct, games_pct_threshold = games_pct_threshold
    )
    stopifnot("peakPerformR::identify_prime (method='actual') failed to return a data frame." = is.data.frame(new_raw_primes_df))
    message(sprintf("API: Raw prime identification complete. Found %d prime periods.", nrow(new_raw_primes_df)))

    # --- Step 2: Dependent Calculations (Performance Dataset & Processing) ---
    message("API: Starting Step 2: Dependent Calculations...")
    message("API: Creating performance dataset (peakPerformR::create_player_performance_dataset)...")
    temp_player_data <- peakPerformR::create_player_performance_dataset(
      sports_data = base_sports_data, prime_data = new_spline_primes_df, # Using SPLINE primes here
      cluster_data = base_cluster_data
    )
    stopifnot("peakPerformR::create_player_performance_dataset failed." = is.data.frame(temp_player_data))
    message("API: Performance dataset created.")

    message("API: Processing prime metrics (peakPerformR::process_player_primes)...")
    temp_processed_data <- peakPerformR::process_player_primes(
      temp_player_data, base_sports_data
    )
    stopifnot("peakPerformR::process_player_primes failed." = is.data.frame(temp_processed_data))
    message("API: Prime metrics processed.")

    # Update 'in_prime' flag using RAW primes
    message("API: Updating full data with 'in_prime' flag (using raw primes)...")
    updated_full_data_df <- base_sports_data # Start fresh
    if (nrow(new_raw_primes_df) > 0 && all(c("id", "start_age", "end_age") %in% names(new_raw_primes_df))) {
      primes_to_join <- new_raw_primes_df %>%
        dplyr::select(id, prime_start_age = start_age, prime_end_age = end_age) %>%
        dplyr::distinct(id, .keep_all = TRUE) # Assuming one prime interval per player for flag

      updated_full_data_df <- updated_full_data_df %>%
        dplyr::left_join(primes_to_join, by = "id") %>%
        dplyr::mutate(
          in_prime = !is.na(prime_start_age) & !is.na(prime_end_age) & !is.na(age) &
            age >= prime_start_age & age <= prime_end_age
        ) %>%
        dplyr::select(-prime_start_age, -prime_end_age) # Clean up
      message(sprintf("API: 'in_prime' flag updated using left join. %d rows marked as in_prime.", sum(updated_full_data_df$in_prime, na.rm=TRUE)))
    } else {
      message("API Warning: Raw primes data empty or missing columns. Setting 'in_prime' to FALSE for all.")
      updated_full_data_df <- updated_full_data_df |> dplyr::mutate(in_prime = FALSE)
    }
    stopifnot("Updating 'in_prime' flag failed." = is.data.frame(updated_full_data_df))

    # --- Step 3: PQI Calculation ---
    message("API: Starting Step 3: PQI Calculation...")
    message("API: Calculating PQI (peakPerformR::calculate_prime_quality_index)...")
    pqi_calculated_df <- peakPerformR::calculate_prime_quality_index(
      temp_processed_data, # Uses processed data derived from SPLINE primes
      nfl_by_position = TRUE,
      tier_method = "percentile"
    )
    stopifnot("peakPerformR::calculate_prime_quality_index failed." = is.data.frame(pqi_calculated_df))
    # Rename safely
    if (nrow(pqi_calculated_df) > 0 && all(c("prime_tier", "pqi_score") %in% names(pqi_calculated_df))) {
      pqi_calculated_df <- pqi_calculated_df |>
        dplyr::rename(selected_tier = prime_tier, pqi_selected = pqi_score)
      message(sprintf("API: PQI calculation complete. %d PQI records generated.", nrow(pqi_calculated_df)))
    } else {
      message("API Warning: PQI result empty or missing expected columns. Adding empty columns if needed.")
      if (!"selected_tier" %in% names(pqi_calculated_df)) pqi_calculated_df$selected_tier <- NA_character_
      if (!"pqi_selected" %in% names(pqi_calculated_df)) pqi_calculated_df$pqi_selected <- NA_real_
    }

    # --- Step 4: CQI Calculation ---
    message("API: Starting Step 4: CQI Calculation...")
    # Prepare data minimally for CQI
    base_traj_minimal <- base_trajectory_data %>% dplyr::select(id, age, predicted_value) %>% distinct(id, age, .keep_all = TRUE) # Ensure unique
    processed_minimal <- temp_processed_data %>% dplyr::select(id, career_avg_tier) %>% dplyr::distinct(id, .keep_all=TRUE)
    stopifnot("Required cols missing before CQI joins." = all(c("id", "age") %in% names(updated_full_data_df)))

    data_for_cqi <- updated_full_data_df |>
      dplyr::left_join(base_traj_minimal, by = c("id","age")) |>
      dplyr::left_join(processed_minimal, by = "id")
    stopifnot("Data preparation for CQI via joins failed." = is.data.frame(data_for_cqi))
    message("API: Data prepared for CQI calculation.")

    message("API: Calculating CQI (peakPerformR::calculate_career_quality_index)...")
    cqi_calculated_df <- peakPerformR::calculate_career_quality_index(
      player_data = data_for_cqi,
      nfl_by_position = FALSE,
      tier_method = "percentile",
      exclude_positions = c("OL", "SPEC"),
      min_seasons = 5
    )
    stopifnot("peakPerformR::calculate_career_quality_index failed." = is.data.frame(cqi_calculated_df))
    # Rename safely
    if (nrow(cqi_calculated_df) > 0 && all(c("career_tier", "cqi_score") %in% names(cqi_calculated_df))) {
      cqi_calculated_df <- cqi_calculated_df |>
        dplyr::rename(selected_tier = career_tier, cqi_selected = cqi_score)
      message(sprintf("API: CQI calculation complete. %d CQI records generated.", nrow(cqi_calculated_df)))
    } else {
      message("API Warning: CQI result empty or missing expected columns. Adding empty columns if needed.")
      if (!"selected_tier" %in% names(cqi_calculated_df)) cqi_calculated_df$selected_tier <- NA_character_
      if (!"cqi_selected" %in% names(cqi_calculated_df)) cqi_calculated_df$cqi_selected <- NA_real_
    }

    # --- Step 5: Final Filtering (Consistency between PQI/CQI) ---
    message("API: Starting Step 5: Final Filtering...")
    pqi_final_df <- pqi_calculated_df
    cqi_final_df <- cqi_calculated_df
    req_cols <- c("id", "position")

    can_filter <- nrow(pqi_calculated_df) > 0 && nrow(cqi_calculated_df) > 0 &&
      all(req_cols %in% names(pqi_calculated_df)) && all(req_cols %in% names(cqi_calculated_df))

    if (can_filter) {
      pqi_filtered <- pqi_calculated_df %>% dplyr::filter(!is.na(position) & position != "SPEC")
      cqi_filtered <- cqi_calculated_df %>% dplyr::filter(!is.na(position) & position != "SPEC")
      common_ids <- intersect(pqi_filtered$id, cqi_filtered$id)
      pqi_final_df <- pqi_filtered %>% dplyr::filter(id %in% common_ids)
      cqi_final_df <- cqi_filtered %>% dplyr::filter(id %in% common_ids)
      message(sprintf("API: Final filtering applied. Final PQI rows: %d, Final CQI rows: %d", nrow(pqi_final_df), nrow(cqi_final_df)))
    } else {
      message("API Warning: Skipping final PQI/CQI filtering as inputs are empty, missing columns, or filtering is not applicable.")
    }

    calculation_end_time <- Sys.time()
    message(sprintf("API: Calculation sequence successful. Duration: %.2f seconds", difftime(calculation_end_time, calculation_start_time, units = "secs")))

    # --- Prepare successful results list ---
    list(
      success = TRUE,
      message = "Recalculation successful.",
      parameters = current_params,
      dataSummary = list(
        rawPrimesCount = nrow(new_raw_primes_df),
        splinePrimesCount = nrow(new_spline_primes_df),
        pqiCount = nrow(pqi_final_df),
        cqiCount = nrow(cqi_final_df),
        fullDataRows = nrow(updated_full_data_df)
      ),
      rawPrimes = new_raw_primes_df,
      splinePrimes = new_spline_primes_df,
      pqi = pqi_final_df,
      cqi = cqi_final_df,
      fullData = updated_full_data_df
    )

  }, error = function(e) { # Catch errors from ANY step inside the tryCatch
    error_message <- paste("API ERROR during calculation:", conditionMessage(e))
    message(error_message)
    # Capture traceback if possible
    tb <- tryCatch(rlang::trace_back(), error = function(e_tb) "Traceback not available")
    message("Traceback:\n", paste(capture.output(print(tb)), collapse = "\n"))
    # Return the error object itself
    return(e)
  }) # End main tryCatch block

  # --- Prepare Final Response ---
  final_response_list <- list()

  if (inherits(calculation_result, "error")) {
    res$status <- 500 # Internal Server Error
    # Use the initialized structure for consistency
    results$success <- FALSE
    # Provide user-friendly message, log the detailed one
    results$message <- paste("Internal server error during calculation. Check server logs for details. Error context:", calculation_result$message)
    # Ensure data fields are empty lists (or appropriate type)
    results$rawPrimes <- list()
    results$splinePrimes <- list()
    results$pqi <- list()
    results$cqi <- list()
    results$fullData <- list()
    # dataSummary might be partially filled or NA - leave as is from init
    final_response_list <- results

  } else {
    # Calculation was successful, use the list returned by the tryCatch block
    final_response_list <- calculation_result
    res$status <- 200 # OK
  }

  # Final check: Ensure the result is a list suitable for JSON conversion
  if (!is.list(final_response_list) || is.null(names(final_response_list))) {
    message("API CRITICAL Error: Final result structure is invalid before JSON serialization.");
    res$status <- 500
    # Revert to the basic error structure
    final_response_list <- list(
      success = FALSE,
      message = "Internal server error: Invalid response structure generated.",
      parameters = current_params, # Still include parameters if available
      dataSummary = list(), # Keep consistent keys even if empty
      rawPrimes = list(), splinePrimes = list(), pqi = list(), cqi = list(), fullData = list()
    )
  }

  # --- Manual JSON Serialization with Final Error Check ---
  response_body <- tryCatch({
    # Use pretty=FALSE for production (smaller payload)
    jsonlite::toJSON(final_response_list, auto_unbox = TRUE, na = "null", digits = NA, pretty = FALSE)
  }, error = function(e) {
    error_msg_json <- "Internal server error: Failed to serialize results to JSON."
    message("API CRITICAL Error: ", error_msg_json, " JSON Error: ", conditionMessage(e));
    res$status <- 500 # Ensure status is 500 if serialization fails
    # Manually create a minimal error JSON string
    sprintf('{"success": false, "message": "%s", "parameters": %s, "dataSummary": {}, "rawPrimes": [], "splinePrimes": [], "pqi": [], "cqi": [], "fullData": []}',
            error_msg_json,
            jsonlite::toJSON(current_params, auto_unbox = TRUE, na = "null") # Include params if possible
    )
  })

  message(sprintf("API: /recalculate completed. Status: %d. Total duration: %.2f seconds",
                  res$status %||% 200,
                  difftime(Sys.time(), endpoint_start_time, units = "secs")))

  res$body <- response_body
  return(res)
}


#* API Status / Health Check
#* @get /
#* @serializer contentType list(type="application/json") # Manual serialization
function(req, res){ # Add req parameter
  message("API: / (health check) endpoint invoked.")
  response_list <- list()

  # Check for the initial load error first
  if (exists("LOAD_ERROR", envir = .API_ENV)) {
    res$status <- 503 # Service Unavailable
    response_list <- list(
      status = "Error",
      message = paste("API is running BUT critical base data failed to load during startup. Service is impaired. Check server logs. Error:",
                      conditionMessage(.API_ENV$LOAD_ERROR)),
      data_loaded = FALSE
    )
  } else {
    # Check if data objects exist and are valid within the environment
    data_check <- tryCatch({
      stopifnot(
        "SPORTS_FILTERED missing or not a data frame" = exists("SPORTS_FILTERED", envir = .API_ENV) && inherits(.API_ENV$SPORTS_FILTERED, "data.frame") && nrow(.API_ENV$SPORTS_FILTERED) > 0,
        "PLAYER_TRAJECTORIES missing or not a data frame" = exists("PLAYER_TRAJECTORIES", envir = .API_ENV) && inherits(.API_ENV$PLAYER_TRAJECTORIES, "data.frame") && nrow(.API_ENV$PLAYER_TRAJECTORIES) > 0,
        "CLUSTER_DATA missing or not a data frame" = exists("CLUSTER_DATA", envir = .API_ENV) && inherits(.API_ENV$CLUSTER_DATA, "data.frame") && nrow(.API_ENV$CLUSTER_DATA) > 0
      )
      TRUE # Return TRUE if all checks pass
    }, error = function(e) {
      message("API Health Check Warning: Data validation failed. ", conditionMessage(e))
      FALSE # Return FALSE if any check fails
    })

    if (data_check) {
      res$status <- 200 # OK
      response_list <- list(
        status = "OK",
        message = "API is running and base data is loaded and validated.",
        data_loaded = TRUE,
        endpoints = list(
          status = list(method = "GET", path = "/", description = "API health check."),
          recalculate = list(method = "POST", path = "/recalculate", description = "Recalculates metrics based on thresholds (thresholdPct, gamesPctThreshold).")
        )
      )
      message("API Health Check: OK.")
    } else {
      res$status <- 503 # Service Unavailable (data is essential)
      response_list <- list(
        status = "Error",
        message = "API is running BUT base data is missing, invalid, or empty. Service is impaired. Check server logs.",
        data_loaded = FALSE
      )
      message("API Health Check: Failed (Data Invalid/Missing).")
    }
  }

  # Manually serialize the list to JSON
  res$body <- jsonlite::toJSON(response_list, auto_unbox = TRUE, na = "null", pretty = FALSE)
  message("API: / (health check) completed.")
  return(res)
}

# --- Plumber Run/Deploy Instructions ---
# Local Development:
# pr <- plumber::plumb("plumber/plumber.R")
# pr$run(host = "0.0.0.0", port = 8000, swagger = TRUE)

# Deployment to shinyapps.io (using renv):
# 1. Ensure renv is active (`renv::status()`) and lockfile (`renv.lock`) is up-to-date (`renv::snapshot()`)
#    - Make sure peakPerformR is installed from GitHub (`renv::install("elivatsaas/peakPerformR")`)
#    - Ensure plumber is in the lockfile (`renv::install("plumber")`)
# 2. Run the deployment command from the project root directory:
#
# rsconnect::deployApp(
#   appDir = "plumber",         # Directory containing plumber.R and data/ subdirectory
#   appPrimaryDoc = "plumber.R", # Explicitly declare the entrypoint file
#   appName = "peakperformr-api",
#   account = "elivatsaas",     # Replace with your account name
#   server = "shinyapps.io",
#   forceUpdate = TRUE
#    appMode = "api"           # Usually not needed if '#* @runtime plumber' is present
# )

