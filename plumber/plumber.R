# plumber.R - Production Ready API Endpoint with CORS
# v2023-04-08 - Refined for Production Deployment

# --- Load Required Libraries ---
# Assumes these are managed by renv for reproducible builds.
# Ensure renv.lock reflects these packages and peakPerformR installed from GitHub.
suppressPackageStartupMessages({
  library(plumber)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(peakPerformR) # Your custom package - Ensure internal errors are resolved
})
message("INFO: Required packages loaded. Custom package 'peakPerformR' loaded.")

# --- Utility Functions ---
# Safe default operator: returns y if x is NULL, otherwise x
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# --- Load Pre-Computed Base Data (Load ONCE at API startup) ---
# Using a dedicated environment avoids polluting the global environment.
.API_ENV <- new.env(parent = emptyenv())

message("INFO: Loading pre-computed base data...")
tryCatch({
  # Define path relative to the script's expected location within the app directory.
  # Assumes Dockerfile WORKDIR is /app and this script is at /app/plumber.R,
  # with data copied to /app/data/.
  data_dir <- "data"
  base_path <- getwd() # In container, this should be /app
  message("INFO: Base path for data loading: ", base_path)
  message("INFO: Relative data directory: ", data_dir)

  # Construct full paths
  sports_filtered_path <- file.path(data_dir, "precalc_sports_filtered.rds")
  trajectories_path <- file.path(data_dir, "precalc_player_trajectories.rds")
  clusters_path <- file.path(data_dir, "precalc_cluster_data.rds")

  required_files <- c(sports_filtered_path, trajectories_path, clusters_path)
  files_exist <- file.exists(required_files)

  # Critical check: Stop if essential data files are missing
  if(!all(files_exist)) {
    missing_files <- required_files[!files_exist]
    abs_data_dir_path <- tryCatch(normalizePath(file.path(base_path, data_dir), mustWork = FALSE), error=function(e) file.path(base_path, data_dir))
    stop(paste("CRITICAL ERROR: One or more pre-calculated data files not found:",
               paste(basename(missing_files), collapse=", "),
               "\nExpected location relative to CWD:", abs_data_dir_path,
               "\nCurrent working directory reported as:", base_path,
               "\nPlease ensure files exist in the 'data' subdirectory within the deployment context."))
  }

  # Load data into the dedicated environment
  .API_ENV$SPORTS_FILTERED <- readRDS(sports_filtered_path)
  .API_ENV$PLAYER_TRAJECTORIES <- readRDS(trajectories_path)
  .API_ENV$CLUSTER_DATA <- readRDS(clusters_path)

  # Basic validation of loaded data structure and content
  stopifnot(
    "`SPORTS_FILTERED` must load as a non-empty data frame." = inherits(.API_ENV$SPORTS_FILTERED, "data.frame") && nrow(.API_ENV$SPORTS_FILTERED) > 0,
    "`PLAYER_TRAJECTORIES` must load as a non-empty data frame." = inherits(.API_ENV$PLAYER_TRAJECTORIES, "data.frame") && nrow(.API_ENV$PLAYER_TRAJECTORIES) > 0,
    "`CLUSTER_DATA` must load as a non-empty data frame." = inherits(.API_ENV$CLUSTER_DATA, "data.frame") && nrow(.API_ENV$CLUSTER_DATA) > 0
  )

  message("INFO: Base data loaded and validated successfully.")

}, error = function(e) {
  # Log critical error and store it for health checks
  message("CRITICAL ERROR during base data loading: ")
  message(conditionMessage(e))
  .API_ENV$LOAD_ERROR <- e
  message("ERROR: API service will be impaired due to failure in loading base data. Check paths, file integrity, and pre-computation steps.")
  # Note: The API will still start, but endpoints checking .API_ENV$LOAD_ERROR will fail.
})


# --- Plumber Filters ---
# Filters run in order for each request before the endpoint handler.

#* CORS handler - Essential for Browser-Based Clients (e.g., React)
#* Configures Cross-Origin Resource Sharing headers.
#* @filter cors
function(req, res) {
  # Production Best Practice: Control allowed origins via an environment variable.
  # Set 'ALLOWED_ORIGIN' in your Azure Container App environment configuration.
  # Example value: "https://your-react-app.com" (NO trailing slash)
  # Default is for local development ONLY. For production, ensure the env var is set.
  allowed_origin <- Sys.getenv("ALLOWED_ORIGIN", "http://localhost:3000")
  if (allowed_origin == "http://localhost:3000" && Sys.getenv("R_ENV", "development") == "production") {
    message("WARN: ALLOWED_ORIGIN environment variable not set in production environment, defaulting to localhost:3000. This is likely incorrect for production use.")
  }

  # Always set the Allow-Origin header for the configured origin.
  res$setHeader("Access-Control-Allow-Origin", allowed_origin)

  # Handle browser "preflight" OPTIONS requests.
  if (req$REQUEST_METHOD == "OPTIONS") {
    # Specify allowed HTTP methods and headers for requests from the allowed origin.
    res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS") # Adjust if needed
    res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization") # Add others if needed (e.g., custom headers)
    # Optional: Allow credentials (cookies, auth tokens). Only use if origin isn't "*".
    # res$setHeader("Access-Control-Allow-Credentials", "true")
    # Optional: Cache preflight response (seconds). Reduces OPTIONS requests.
    # res$setHeader("Access-Control-Max-Age", "86400") # 24 hours

    res$status <- 204 # 204 No Content is common for OPTIONS success
    # Return empty body immediately - do not forward OPTIONS requests further.
    return(list())
  } else {
    # For non-OPTIONS requests (GET, POST, etc.), pass control to the next filter or endpoint.
    plumber::forward()
  }
}

#* Request Logger
#* Logs basic details about incoming requests and their responses.
#* @filter logger
function(req, res){ # Ensure 'res' is available if needed for response logging later
  start_time <- Sys.time()
  # Log request details
  cat(format(start_time), "- REQ:", req$REQUEST_METHOD, req$PATH_INFO, "-",
      "FROM:", req$REMOTE_ADDR %||% "Unknown", "-",
      "AGENT:", substr(req$HTTP_USER_AGENT %||% "Unknown", 1, 50), # Limit agent length
      "\n")

  # Pass control down the chain (to other filters or the endpoint)
  plumber::forward()

  # Code here runs *after* the endpoint handler finishes
  end_time <- Sys.time()
  duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 3)
  # Safely get status code from the response object
  status_code <- tryCatch(res$status, error = function(e) NULL) %||% 200 # Default to 200 if unset

  # Log response details
  cat(format(end_time), "- RES:", req$REQUEST_METHOD, req$PATH_INFO, "-",
      "STATUS:", status_code, "-",
      "DURATION:", duration, "s\n")
}

# --- API Endpoints ---

#* Recalculate Primes, PQI, CQI based on thresholds
#* Expects a JSON body with optional 'thresholdPct' and 'gamesPctThreshold'.
#* @param req The plumber request object
#* @param res The plumber response object
#* @post /recalculate
#* @serializer contentType list(type="application/json") # Specify response type (manual serialization below)
function(req, res) {
  endpoint_start_time <- Sys.time()
  message("INFO: /recalculate endpoint invoked.")

  # --- Initial Health Check ---
  if (exists("LOAD_ERROR", envir = .API_ENV)) {
    res$status <- 503 # Service Unavailable
    err_msg <- paste("Service Unavailable: Critical base data failed to load during API startup. Check server logs. Original error:",
                     conditionMessage(.API_ENV$LOAD_ERROR))
    message("ERROR: /recalculate cannot proceed due to data load failure.")
    res$body <- jsonlite::toJSON(list(success = FALSE, message = err_msg, parameters = list()), auto_unbox = TRUE, na = "null")
    return(res)
  }

  # --- Defaults and Parameter Handling ---
  default_threshold_pct <- 70.0
  default_games_pct_threshold <- 100.0
  results <- list( # Initialize consistent response structure
    success = FALSE, message = "Initialization error.", parameters = list(),
    dataSummary = list(rawPrimesCount = NA_integer_, splinePrimesCount = NA_integer_, pqiCount = NA_integer_, cqiCount = NA_integer_, fullDataRows = NA_integer_),
    rawPrimes = list(), splinePrimes = list(), pqi = list(), cqi = list(), fullData = list()
  )

  params <- tryCatch({ # Safely parse JSON body
    body <- req$postBody %||% "{}" # Handle NULL body
    if (body == "") body <- "{}"
    jsonlite::fromJSON(body, simplifyDataFrame = FALSE)
  }, error = function(e) {
    message("WARN: Failed to parse request body JSON. Using defaults. Error: ", conditionMessage(e))
    list() # Return empty list on parse error
  })

  # Extract parameters with defaults
  threshold_pct <- as.numeric(params$thresholdPct %||% default_threshold_pct)
  games_pct_threshold <- as.numeric(params$gamesPctThreshold %||% default_games_pct_threshold)
  current_params <- list(thresholdPct = threshold_pct, gamesPctThreshold = games_pct_threshold)
  results$parameters <- current_params # Record parameters used

  # --- Input Validation ---
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
    message("ERROR: Invalid input parameters: ", results$message)
    res$body <- jsonlite::toJSON(results, auto_unbox = TRUE, na = "null") # Use initialized structure
    return(res)
  }

  message(sprintf("INFO: Received valid request: thresholdPct=%.1f, gamesPctThreshold=%.1f", threshold_pct, games_pct_threshold))
  calculation_start_time <- Sys.time()

  # --- Core Calculation Logic (Wrapped in Error Handling) ---
  calculation_result <- tryCatch({
    # Access pre-loaded base data safely from the dedicated environment
    base_sports_data <- .API_ENV$SPORTS_FILTERED
    base_trajectory_data <- .API_ENV$PLAYER_TRAJECTORIES
    base_cluster_data <- .API_ENV$CLUSTER_DATA
    message("INFO: Calculation Step 0: Base data retrieved.")

    # --- Step 1: Prime Identification ---
    message("INFO: Calculation Step 1: Identifying Primes...")
    spline_join_data <- base_sports_data %>% select(id, league, position, age, games_played) %>% distinct(id, age, .keep_all = TRUE)
    spline_input_data <- base_trajectory_data %>% left_join(spline_join_data, by = c("id", "age"))
    stopifnot("Data prep for spline input failed." = is.data.frame(spline_input_data))

    new_spline_primes_df <- peakPerformR::identify_prime(spline_input_data, method = "predicted", threshold_pct = threshold_pct, games_pct_threshold = games_pct_threshold)
    stopifnot("Spline prime identification failed." = is.data.frame(new_spline_primes_df))
    message(sprintf("INFO: -> Spline primes identified: %d rows", nrow(new_spline_primes_df)))

    new_raw_primes_df <- peakPerformR::identify_prime(base_sports_data, method = "actual", threshold_pct = threshold_pct, games_pct_threshold = games_pct_threshold)
    stopifnot("Raw prime identification failed." = is.data.frame(new_raw_primes_df))
    message(sprintf("INFO: -> Raw primes identified: %d rows", nrow(new_raw_primes_df)))

    # --- Step 2: Dependent Calculations (Performance Dataset & Processing) ---
    message("INFO: Calculation Step 2: Creating Performance Dataset & Processing Primes...")
    temp_player_data <- peakPerformR::create_player_performance_dataset(sports_data = base_sports_data, prime_data = new_spline_primes_df, cluster_data = base_cluster_data)
    stopifnot("Create performance dataset failed." = is.data.frame(temp_player_data))

    temp_processed_data <- peakPerformR::process_player_primes(temp_player_data, base_sports_data)
    stopifnot("Process player primes failed." = is.data.frame(temp_processed_data))

    # Update 'in_prime' flag using RAW primes for the full dataset output
    updated_full_data_df <- base_sports_data # Start fresh
    if (nrow(new_raw_primes_df) > 0 && all(c("id", "start_age", "end_age") %in% names(new_raw_primes_df))) {
      primes_to_join <- new_raw_primes_df %>% select(id, prime_start_age = start_age, prime_end_age = end_age) %>% distinct(id, .keep_all = TRUE)
      updated_full_data_df <- updated_full_data_df %>%
        left_join(primes_to_join, by = "id") %>%
        mutate(in_prime = !is.na(prime_start_age) & !is.na(prime_end_age) & !is.na(age) & age >= prime_start_age & age <= prime_end_age) %>%
        select(-prime_start_age, -prime_end_age)
    } else {
      message("WARN: Raw primes data empty or missing columns. Setting 'in_prime' to FALSE for all in fullData.")
      updated_full_data_df <- updated_full_data_df %>% mutate(in_prime = FALSE)
    }
    stopifnot("Updating 'in_prime' flag failed." = is.data.frame(updated_full_data_df))
    message(sprintf("INFO: -> Performance data processed, full data 'in_prime' flag updated (%d T / %d F)",
                    sum(updated_full_data_df$in_prime, na.rm=TRUE), sum(!updated_full_data_df$in_prime, na.rm=TRUE)))

    # --- Step 3: PQI Calculation ---
    message("INFO: Calculation Step 3: Calculating PQI...")
    pqi_calculated_df <- peakPerformR::calculate_prime_quality_index(temp_processed_data, nfl_by_position = TRUE, tier_method = "percentile")
    stopifnot("PQI calculation failed." = is.data.frame(pqi_calculated_df))
    # Rename safely
    if (nrow(pqi_calculated_df) > 0 && all(c("prime_tier", "pqi_score") %in% names(pqi_calculated_df))) {
      pqi_calculated_df <- pqi_calculated_df %>% rename(selected_tier = prime_tier, pqi_selected = pqi_score)
    } else {
      message("WARN: PQI result empty or missing expected columns.")
      if (!"selected_tier" %in% names(pqi_calculated_df)) pqi_calculated_df$selected_tier <- NA_character_
      if (!"pqi_selected" %in% names(pqi_calculated_df)) pqi_calculated_df$pqi_selected <- NA_real_
    }
    message(sprintf("INFO: -> PQI calculation complete: %d rows", nrow(pqi_calculated_df)))

    # --- Step 4: CQI Calculation ---
    message("INFO: Calculation Step 4: Calculating CQI...")
    base_traj_minimal <- base_trajectory_data %>% select(id, age, predicted_value) %>% distinct(id, age, .keep_all = TRUE)
    processed_minimal <- temp_processed_data %>% select(id, career_avg_tier) %>% distinct(id, .keep_all=TRUE)
    data_for_cqi <- updated_full_data_df %>% left_join(base_traj_minimal, by = c("id","age")) %>% left_join(processed_minimal, by = "id")
    stopifnot("Data preparation for CQI failed." = is.data.frame(data_for_cqi))

    cqi_calculated_df <- peakPerformR::calculate_career_quality_index(data_for_cqi, nfl_by_position = FALSE, tier_method = "percentile", exclude_positions = c("OL", "SPEC"), min_seasons = 5)
    stopifnot("CQI calculation failed." = is.data.frame(cqi_calculated_df))
    # Rename safely
    if (nrow(cqi_calculated_df) > 0 && all(c("career_tier", "cqi_score") %in% names(cqi_calculated_df))) {
      cqi_calculated_df <- cqi_calculated_df %>% rename(selected_tier = career_tier, cqi_selected = cqi_score)
    } else {
      message("WARN: CQI result empty or missing expected columns.")
      if (!"selected_tier" %in% names(cqi_calculated_df)) cqi_calculated_df$selected_tier <- NA_character_
      if (!"cqi_selected" %in% names(cqi_calculated_df)) cqi_calculated_df$cqi_selected <- NA_real_
    }
    message(sprintf("INFO: -> CQI calculation complete: %d rows", nrow(cqi_calculated_df)))

    # --- Step 5: Final Filtering (Consistency) ---
    message("INFO: Calculation Step 5: Applying Final Filtering...")
    pqi_final_df <- pqi_calculated_df
    cqi_final_df <- cqi_calculated_df
    if (nrow(pqi_calculated_df) > 0 && nrow(cqi_calculated_df) > 0 && all(c("id", "position") %in% names(pqi_calculated_df)) && all(c("id", "position") %in% names(cqi_calculated_df))) {
      pqi_filtered <- pqi_calculated_df %>% filter(!is.na(position) & position != "SPEC")
      cqi_filtered <- cqi_calculated_df %>% filter(!is.na(position) & position != "SPEC")
      common_ids <- intersect(pqi_filtered$id, cqi_filtered$id)
      pqi_final_df <- pqi_filtered %>% filter(id %in% common_ids)
      cqi_final_df <- cqi_filtered %>% filter(id %in% common_ids)
      message(sprintf("INFO: -> Final filtering applied. Final PQI rows: %d, Final CQI rows: %d", nrow(pqi_final_df), nrow(cqi_final_df)))
    } else {
      message("WARN: Skipping final PQI/CQI filtering due to empty inputs or missing columns.")
    }

    calculation_end_time <- Sys.time()
    message(sprintf("INFO: Calculation sequence successful. Duration: %.2f seconds", difftime(calculation_end_time, calculation_start_time, units = "secs")))

    # Return successful results list
    list(
      success = TRUE, message = "Recalculation successful.", parameters = current_params,
      dataSummary = list(rawPrimesCount = nrow(new_raw_primes_df), splinePrimesCount = nrow(new_spline_primes_df), pqiCount = nrow(pqi_final_df), cqiCount = nrow(cqi_final_df), fullDataRows = nrow(updated_full_data_df)),
      rawPrimes = new_raw_primes_df, splinePrimes = new_spline_primes_df, pqi = pqi_final_df, cqi = cqi_final_df, fullData = updated_full_data_df
    )

  }, error = function(e) { # Catch errors from ANY calculation step
    error_message <- paste("Internal Server Error during calculation:", conditionMessage(e))
    message("ERROR: ", error_message)
    # Attempt to get traceback for detailed debugging
    tb <- tryCatch(rlang::trace_back(bottom = sys.frame(1)), error = function(e_tb) "Traceback not available") # Go up one frame from error handler
    message("ERROR Traceback:\n", paste(capture.output(print(tb)), collapse = "\n"))
    # Return the error object itself to be handled below
    return(e)
  }) # End main calculation tryCatch

  # --- Prepare Final Response ---
  final_response_list <- list()
  if (inherits(calculation_result, "error")) {
    res$status <- 500 # Internal Server Error
    # Use the initialized structure, update message
    results$success <- FALSE
    # Provide user-friendly message, detailed error already logged
    results$message <- paste("Internal server error during calculation. Please check server logs or contact support. Ref:", calculation_result$message) # Include original error context briefly
    results$rawPrimes <- list(); results$splinePrimes <- list(); results$pqi <- list(); results$cqi <- list(); results$fullData <- list() # Ensure data fields are empty
    final_response_list <- results
  } else {
    # Calculation successful
    final_response_list <- calculation_result # Use the successful result list
    res$status <- 200 # OK
  }

  # Final check: Ensure the result is a list suitable for JSON conversion
  if (!is.list(final_response_list)) {
    message("CRITICAL ERROR: Final result object is not a list before JSON serialization.");
    res$status <- 500
    # Revert to a basic error structure
    final_response_list <- list( success = FALSE, message = "Internal server error: Invalid response structure generated.", parameters = current_params, dataSummary = list(), rawPrimes = list(), splinePrimes = list(), pqi = list(), cqi = list(), fullData = list())
  }

  # --- Serialize to JSON ---
  response_body <- tryCatch({
    # Use na = "null" to represent NA values as JSON null
    # Use auto_unbox = TRUE for single-element vectors -> scalars
    # Use digits = NA for full precision of numeric values
    # Use pretty = FALSE for smaller payload size in production
    jsonlite::toJSON(final_response_list, auto_unbox = TRUE, na = "null", digits = NA, pretty = FALSE)
  }, error = function(e) {
    error_msg_json <- "Internal server error: Failed to serialize results to JSON."
    message("CRITICAL ERROR: ", error_msg_json, " JSON Error: ", conditionMessage(e));
    res$status <- 500 # Ensure status reflects the serialization failure
    # Manually create a minimal valid JSON error string
    sprintf('{"success": false, "message": "%s", "parameters": %s, "dataSummary": {}, "rawPrimes": [], "splinePrimes": [], "pqi": [], "cqi": [], "fullData": []}',
            gsub('"', '\\\\"', error_msg_json), # Escape quotes in message
            jsonlite::toJSON(current_params, auto_unbox = TRUE, na = "null") %||% '{}' # Include params if possible
    )
  })

  # --- Final Logging and Return ---
  endpoint_duration <- difftime(Sys.time(), endpoint_start_time, units = "secs")
  message(sprintf("INFO: /recalculate completed. Status: %d. Duration: %.2f seconds",
                  res$status %||% 500, # Use default status if somehow unset
                  endpoint_duration))

  # Set response body and return the response object
  res$body <- response_body
  return(res)
}


#* API Status / Health Check
#* Provides basic status information about the API.
#* @get /
#* @serializer contentType list(type="application/json") # Manual serialization
function(req, res){
  message("INFO: / (health check) endpoint invoked.")
  response_list <- list()

  # Check 1: Was there a critical error during initial data loading?
  if (exists("LOAD_ERROR", envir = .API_ENV)) {
    res$status <- 503 # Service Unavailable
    response_list <- list(
      status = "ERROR",
      message = paste("API is running BUT critical base data failed to load during startup. Service is impaired. Check server logs. Startup Error:",
                      conditionMessage(.API_ENV$LOAD_ERROR)),
      data_loaded = FALSE,
      timestamp = Sys.time()
    )
    message("ERROR: Health Check Failed (Startup Data Load Error).")
  } else {
    # Check 2: Verify essential data objects exist and seem valid *now*.
    data_check_passed <- tryCatch({
      stopifnot(
        exists("SPORTS_FILTERED", envir = .API_ENV) && inherits(.API_ENV$SPORTS_FILTERED, "data.frame") && nrow(.API_ENV$SPORTS_FILTERED) > 0,
        exists("PLAYER_TRAJECTORIES", envir = .API_ENV) && inherits(.API_ENV$PLAYER_TRAJECTORIES, "data.frame") && nrow(.API_ENV$PLAYER_TRAJECTORIES) > 0,
        exists("CLUSTER_DATA", envir = .API_ENV) && inherits(.API_ENV$CLUSTER_DATA, "data.frame") && nrow(.API_ENV$CLUSTER_DATA) > 0
      )
      TRUE # All checks passed
    }, error = function(e) {
      message("WARN: Health Check - Runtime data validation failed: ", conditionMessage(e))
      FALSE # A check failed
    })

    if (data_check_passed) {
      res$status <- 200 # OK
      response_list <- list(
        status = "OK",
        message = "API is running and essential base data appears valid.",
        data_loaded = TRUE,
        timestamp = Sys.time(),
        endpoints = list(
          status = list(method = "GET", path = "/", description = "API health check."),
          recalculate = list(method = "POST", path = "/recalculate", description = "Recalculates metrics based on thresholds (thresholdPct, gamesPctThreshold).")
        )
      )
      message("INFO: Health Check OK!")
    } else {
      res$status <- 503 # Service Unavailable (Essential data missing/invalid at runtime)
      response_list <- list(
        status = "ERROR",
        message = "API is running BUT essential base data is missing, invalid, or empty at runtime. Service is impaired. Check server logs.",
        data_loaded = FALSE,
        timestamp = Sys.time()
      )
      message("ERROR: Health Check Failed (Runtime Data Invalid/Missing).")
    }
  }

  # Serialize response and return
  res$body <- jsonlite::toJSON(response_list, auto_unbox = TRUE, na = "null", pretty = FALSE)
  message("INFO: / (health check) completed.")
  return(res)
}

# --- Plumber Entrypoint Definition ---
# Defines this script as a Plumber API source. Used by plumber::plumb().
#* @plumber
function(pr) {
  # Optional: Add global error handlers or other router-level configurations here
  # pr$setErrorHandler(function(req, res, err){ ... })
  message("INFO: Plumber router object created.")
  pr # Return the router object
}

# --- Notes for Production Deployment (e.g., Azure Container Apps via Docker) ---
# 1. Dependencies: Use 'renv' to manage and lock R package versions (`renv::snapshot()`).
#    Ensure 'renv.lock' is committed and used during Docker build (e.g., `renv::restore()`).
# 2. Configuration: Set environment variables in Azure Container Apps for:
#    - `ALLOWED_ORIGIN`: URL of your frontend (e.g., "https://your-react-app.com")
#    - `R_ENV`: Set to "production" (optional, can help control behavior/logging)
#    - Any other external service credentials or settings.
# 3. Dockerfile CMD: Use the JSON format for CMD for proper signal handling:
#    `CMD ["R", "-e", "api <- plumber::plumb('plumber.R'); api$run(host='0.0.0.0', port=8000)"]`
#    (Adjust port as needed, ensure it matches EXPOSE and Azure Container App config).
# 4. Data Files: Ensure the `data/` directory is correctly copied into the Docker image
#    relative to `plumber.R` and the WORKDIR (e.g., `COPY data/ /app/data/`).
# 5. HTTPS: Handled by Azure Container Apps ingress; the R process listens on HTTP internally.
# 6. Logging: Configure Azure Container Apps to collect logs (stdout/stderr). Consider
#    structured logging (JSON format) for easier parsing in tools like Log Analytics.
# 7. Authentication/Authorization: This API is currently open. Implement appropriate security
#    (API keys, tokens via Authorization header, etc.) if the data or operations are sensitive.
#    This would likely involve adding another Plumber filter.
