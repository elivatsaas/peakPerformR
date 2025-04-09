# --- Load Required Libraries ---
suppressPackageStartupMessages({
  library(plumber)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(peakPerformR) # Your custom package
  library(rlang)      # Needed for trace_back and :=
})
message("Required packages loaded. Custom package 'peakPerformR' loaded.")

# --- Utility Functions ---
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x # Modified to handle length 0
}

# --- Load Pre-Computed Base Data (Load ONCE at API startup) ---
.API_ENV <- new.env(parent = emptyenv())

message("Loading pre-computed base data...")
tryCatch({
  data_dir <- "data"
  message("Using relative path presumed from script location: ", data_dir)
  normalized_base_path <- tryCatch(normalizePath(getwd(), mustWork = FALSE), error=function(e) getwd())
  message("Attempting to load base data relative to CWD: ", normalized_base_path)

  sports_filtered_path <- file.path(data_dir, "precalc_sports_filtered.rds")
  trajectories_path <- file.path(data_dir, "precalc_player_trajectories.rds")
  # clusters_path is still loaded but not directly used for scaled_value in this version
  clusters_path <- file.path(data_dir, "precalc_cluster_data.rds")

  required_files <- c(sports_filtered_path, trajectories_path, clusters_path)
  files_exist <- file.exists(required_files)

  if(!all(files_exist)) {
    missing_files <- required_files[!files_exist]
    normalized_data_dir_path <- tryCatch(normalizePath(file.path(normalized_base_path, data_dir), mustWork = FALSE), error=function(e) file.path(normalized_base_path, data_dir))
    stop(paste("CRITICAL ERROR: One or more pre-calculated data files not found:",
               paste(basename(missing_files), collapse=", "),
               "\nExpected location relative to CWD:", normalized_data_dir_path,
               "\nCurrent working directory reported as:", normalized_base_path))
  }

  .API_ENV$SPORTS_FILTERED <- readRDS(sports_filtered_path)
  .API_ENV$PLAYER_TRAJECTORIES <- readRDS(trajectories_path)
  .API_ENV$CLUSTER_DATA <- readRDS(clusters_path) # Still loaded for potential use elsewhere (e.g., create_player_performance_dataset)

  stopifnot(
    "`SPORTS_FILTERED` did not load as a non-empty data frame." = inherits(.API_ENV$SPORTS_FILTERED, "data.frame") && nrow(.API_ENV$SPORTS_FILTERED) > 0,
    "`PLAYER_TRAJECTORIES` did not load as a non-empty data frame." = inherits(.API_ENV$PLAYER_TRAJECTORIES, "data.frame") && nrow(.API_ENV$PLAYER_TRAJECTORIES) > 0,
    "`CLUSTER_DATA` did not load as a non-empty data frame." = inherits(.API_ENV$CLUSTER_DATA, "data.frame") && nrow(.API_ENV$CLUSTER_DATA) > 0
  )
  message("Base data loaded and validated successfully.")

}, error = function(e) {
  message("CRITICAL ERROR during base data loading: ")
  message(conditionMessage(e))
  .API_ENV$LOAD_ERROR <- e
  message("API WILL LIKELY FAIL: Base data loading failed. Check logs, paths, and pre-computation script.")
})


# --- Plumber Filters ---

#* CORS handler - IMPORTANT FOR FRONTEND INTEGRATION
#* @filter cors
function(req, res) {
  # Get allowed origin from Environment Variable
  allowed_origin <- Sys.getenv("ALLOWED_ORIGIN", "http://localhost:3000") # Default to localhost:3000 if not set

  # Set CORS headers
  res$setHeader("Access-Control-Allow-Origin", allowed_origin)
  res$setHeader("Access-Control-Allow-Credentials", "true") # Crucial for cookies/auth headers

  # Handle preflight (OPTIONS) requests
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
    res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization") # Adjust headers as needed
    res$status <- 204 # No Content often preferred for OPTIONS
    return(list()) # Respond immediately
  } else {
    plumber::forward() # Continue processing
  }
}

#* Log incoming requests
#* @filter logger
function(req){
  start_time <- Sys.time()
  cat(format(start_time), "-", req$REQUEST_METHOD, req$PATH_INFO, "-", req$HTTP_USER_AGENT %||% "Unknown", "@", req$REMOTE_ADDR %||% "Unknown", "\n")

  plumber::forward() # Pass control to the next filter or endpoint

  end_time <- Sys.time()
  duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 3)
  status_code <- tryCatch(req$pr$res$status, error = function(e) NULL) %||% 500
  cat(format(end_time), "-", req$REQUEST_METHOD, req$PATH_INFO, "- Status:", status_code, "- Duration:", duration, "s\n")
}

# --- API Endpoints ---

#* Recalculate Primes, PQI, CQI based on thresholds
#* @post /recalculate
#* @serializer contentType list(type="application/json")
function(req, res) {
  endpoint_start_time <- Sys.time()
  message("API: /recalculate endpoint invoked.")

  # --- Initial Health Check ---
  if (exists("LOAD_ERROR", envir = .API_ENV)) {
    res$status <- 503 # Service Unavailable
    err_msg <- paste("Service Unavailable: Critical base data failed to load during API startup.",
                     "Check server logs. Original error:", conditionMessage(.API_ENV$LOAD_ERROR))
    message("ERROR: /recalculate cannot proceed due to data load failure.")
    res$setHeader("Content-Type", "application/json")
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
    param_errors <- c(param_errors, "Invalid or missing 'thresholdPct' (numeric 0-100).")
  }
  if (length(games_pct_threshold) != 1 || is.na(games_pct_threshold) || !is.numeric(games_pct_threshold) || games_pct_threshold < 0 || games_pct_threshold > 100) {
    param_errors <- c(param_errors, "Invalid or missing 'gamesPctThreshold' (numeric 0-100).")
  }

  if (length(param_errors) > 0) {
    res$status <- 400 # Bad Request
    results$message <- paste("Input validation failed:", paste(param_errors, collapse = " "))
    message("ERROR: Invalid input parameters: ", results$message)
    res$setHeader("Content-Type", "application/json")
    res$body <- jsonlite::toJSON(results, auto_unbox = TRUE, na = "null")
    return(res)
  }

  message(sprintf("INFO: Received valid request: thresholdPct=%.1f, gamesPctThreshold=%.1f", threshold_pct, games_pct_threshold))
  calculation_start_time <- Sys.time()

  # --- Core Calculation Logic (Wrapped in Error Handling) ---
  calculation_result <- tryCatch({
    # Access pre-loaded base data safely from the dedicated environment
    base_sports_data <- .API_ENV$SPORTS_FILTERED
    base_trajectory_data <- .API_ENV$PLAYER_TRAJECTORIES
    base_cluster_data <- .API_ENV$CLUSTER_DATA # Used by create_player_performance_dataset
    message("INFO: Calculation Step 0: Base data retrieved.")

    # --- Step 1: Prime Identification ---
    message("INFO: Calculation Step 1: Identifying Primes...!")
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

    # --- Step 2.5: Explicitly Calculate Scaled Value for fullData ---
    message("INFO: Calculation Step 2.5: Explicitly calculating scaled_value from predicted_value...")

    # Need predicted_value (from trajectories) and league/position (from sports_data)
    traj_minimal <- base_trajectory_data %>%
      select(id, age, predicted_value) %>%
      distinct(id, age, .keep_all = TRUE)

    data_to_scale <- base_sports_data %>%
      # Select only necessary columns from sports_data for joining and grouping
      select(id, age, league, position) %>%
      distinct(id, age, .keep_all = TRUE) %>% # Ensure unique id-age combo before join
      left_join(traj_minimal, by = c("id", "age"))

    stopifnot("Failed to join trajectory data for scaling." = is.data.frame(data_to_scale))

    # Perform the Z-score scaling within league/position groups
    scaled_values_calculated <- data_to_scale |>
      # Ensure grouping columns are suitable (e.g., handle NA positions/leagues if necessary)
      filter(!is.na(league), !is.na(position)) |> # Filter out rows unusable for group scaling
      group_by(league, position) |>
      mutate(
        group_mean = mean(predicted_value, na.rm = TRUE),
        group_sd = sd(predicted_value, na.rm = TRUE),
        # Calculate Z-score, handle SD=0 or NA SD, handle NA predicted_value
        calculated_scaled_value = case_when(
            is.na(predicted_value) ~ NA_real_, # Rule 1: NA in -> NA out
            is.na(group_sd) ~ 0,          # Rule 2: If SD is NA (e.g., group size 1), Z is 0
            group_sd == 0 ~ 0,          # Rule 3: If SD is 0 (all values same), Z is 0
            TRUE ~ (predicted_value - group_mean) / group_sd # Rule 4: Calculate Z-score
        )
      ) |>
      ungroup() |>
      # Select only the key columns needed for joining back
      select(id, age, calculated_scaled_value)

    stopifnot("Scaling calculation failed." = is.data.frame(scaled_values_calculated))
    message(sprintf("INFO: -> Scaled values calculated for %d id-age records.", nrow(scaled_values_calculated)))

    # --- Update 'fullData' - Start with base data, merge calculated scaled_value ---
    message("INFO: Updating fullData dataframe...")
    updated_full_data_df <- base_sports_data # Start with the original full data

    # Remove any existing 'scaled_value' and merge the newly calculated one
    updated_full_data_df <- updated_full_data_df %>%
      select(-any_of("scaled_value")) %>% # Remove old/potentially incorrect column
      left_join(scaled_values_calculated, by = c("id", "age")) %>%
      rename(scaled_value = calculated_scaled_value) # Rename to the expected column name

    # Ensure the scaled_value column exists, even if some joins failed (assign NA)
    if (!"scaled_value" %in% names(updated_full_data_df)) {
      message("WARN: 'scaled_value' column unexpectedly missing after join. Adding NA column.")
      updated_full_data_df$scaled_value <- NA_real_
    } else {
       # Log how many rows got a non-NA scaled value
       num_scaled <- sum(!is.na(updated_full_data_df$scaled_value))
       message(sprintf("INFO: -> Merged calculated 'scaled_value'. %d non-NA values present.", num_scaled))
    }
    # --- End scaled_value update ---


    # --- Calculate flags based on new spline primes (using the updated_full_data_df)---
    message("INFO: -> Calculating flags (in_prime, etc.)...")
    if (nrow(new_spline_primes_df) > 0 &&
        all(c("id", "start_age", "end_age", "max_value_age") %in% names(new_spline_primes_df))) {

      primes_info_to_join <- new_spline_primes_df %>%
        select(id, prime_start_age = start_age, prime_end_age = end_age, prime_peak_age = max_value_age) %>%
        distinct(id, .keep_all = TRUE)

      updated_full_data_df <- updated_full_data_df %>%
        left_join(primes_info_to_join, by = "id") %>%
        # Ensure columns are numeric before calculations
        mutate(across(c(age, prime_start_age, prime_end_age, prime_peak_age), as.numeric)) %>%
        mutate(
          in_prime = !is.na(prime_start_age) & !is.na(prime_end_age) & !is.na(age) &
                       age >= prime_start_age & age <= prime_end_age,
          # Safer comparison for numeric age
          is_peak_age = !is.na(prime_peak_age) & !is.na(age) &
                         abs(age - prime_peak_age) < 1e-6,
          years_from_peak = if_else(!is.na(age) & !is.na(prime_peak_age),
                                    as.integer(round(age - prime_peak_age)),
                                    NA_integer_)
        ) %>%
        # Convert logical flags to character ("true"/"false") if needed by frontend
         mutate(
            in_prime = if_else(is.na(in_prime), "false", if_else(in_prime, "true", "false")),
            is_peak_age = if_else(is.na(is_peak_age), "false", if_else(is_peak_age, "true", "false"))
         ) %>%
        select(-prime_start_age, -prime_end_age, -prime_peak_age) # Clean up temp join columns

      message(sprintf("INFO: -> Full data flags calculated (in_prime: %d T / %d F, is_peak_age: %d T / %d F)",
                      sum(updated_full_data_df$in_prime == "true", na.rm=TRUE), sum(updated_full_data_df$in_prime == "false", na.rm=TRUE),
                      sum(updated_full_data_df$is_peak_age == "true", na.rm=TRUE), sum(updated_full_data_df$is_peak_age == "false", na.rm=TRUE)))

    } else {
      message("WARN: Spline primes data empty or missing required columns. Setting flags to FALSE/NA in fullData.")
      # Ensure flag columns exist even if primes are missing
      if (!"in_prime" %in% names(updated_full_data_df)) updated_full_data_df$in_prime <- "false"
      else updated_full_data_df$in_prime <- ifelse(is.na(updated_full_data_df$in_prime), "false", updated_full_data_df$in_prime)

      if (!"is_peak_age" %in% names(updated_full_data_df)) updated_full_data_df$is_peak_age <- "false"
      else updated_full_data_df$is_peak_age <- ifelse(is.na(updated_full_data_df$is_peak_age), "false", updated_full_data_df$is_peak_age)

      if (!"years_from_peak" %in% names(updated_full_data_df)) updated_full_data_df$years_from_peak <- NA_integer_
      else updated_full_data_df$years_from_peak[is.na(updated_full_data_df$years_from_peak)] <- NA_integer_
    }
    # --- End flag calculation ---

    # --- Final Check: Ensure ALL required columns exist before proceeding ---
    required_cols <- c("id", "player_name", "age", "league", "sport", "position",
                       "scaled_value", "in_prime", "is_peak_age", "years_from_peak",
                       "games_played") # Base required columns
    missing_cols <- required_cols[!required_cols %in% names(updated_full_data_df)]
    if(length(missing_cols) > 0) {
        # Add missing columns as NA of appropriate type
        for(col in missing_cols) {
           message(sprintf("WARN: Final Check - Adding missing required column '%s' as NA.", col))
           if (col %in% c("scaled_value", "games_played")) { updated_full_data_df[[col]] <- NA_real_ }
           else if (col == "years_from_peak") { updated_full_data_df[[col]] <- NA_integer_ }
           else if (col %in% c("in_prime", "is_peak_age")) { updated_full_data_df[[col]] <- "false" }
           else { updated_full_data_df[[col]] <- NA_character_ }
        }
        final_missing <- required_cols[!required_cols %in% names(updated_full_data_df)]
        if(length(final_missing) > 0) {
           stop(paste("FATAL: Final updated_full_data_df STILL missing required columns after adding NAs:", paste(final_missing, collapse=", ")))
        }
    }
    message("INFO: -> Final column check passed for updated_full_data_df.")
    # --- End Final Check ---


    # --- Step 3: PQI Calculation ---
    # Uses temp_processed_data which is derived indirectly from base data
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
    # Uses the updated_full_data_df which now has the calculated scaled_value
    # !!! NOTE: This section retains the code from your prompt, including the likely error source !!!
    message("INFO: Calculation Step 4: Calculating CQI...")
    base_traj_minimal <- base_trajectory_data %>% select(id, age, predicted_value) %>% distinct(id, age, .keep_all = TRUE)
    # --- !!! LIKELY ERROR SOURCE REMAINS HERE (uses career_avg_tier, likely needs different col from temp_processed_data) !!! ---
    processed_minimal <- temp_processed_data %>% select(id, career_avg_tier) %>% distinct(id, .keep_all=TRUE)
    # --- !!! END LIKELY ERROR SOURCE !!! ---
    data_for_cqi <- updated_full_data_df %>%
                        left_join(base_traj_minimal, by = c("id","age")) %>%
                        left_join(processed_minimal, by = "id")
    stopifnot("Data preparation for CQI failed (check columns)." = is.data.frame(data_for_cqi))

    # --- !!! Column name `career_avg_tier` might need changing here too if the CQI function expects something else !!! ---
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
    message("INFO: Calculation Step 5: Applying Final Filtering for PQI/CQI consistency...")
    pqi_final_df <- pqi_calculated_df
    cqi_final_df <- cqi_calculated_df
    if (nrow(pqi_calculated_df) > 0 && nrow(cqi_calculated_df) > 0 && all(c("id", "position") %in% names(pqi_calculated_df)) && all(c("id", "position") %in% names(cqi_calculated_df))) {
      pqi_filtered <- pqi_calculated_df %>% filter(!is.na(position) & position != "SPEC")
      cqi_filtered <- cqi_calculated_df %>% filter(!is.na(position) & position != "SPEC")
      common_ids <- intersect(pqi_filtered$id, cqi_filtered$id)
      pqi_final_df <- pqi_filtered %>% filter(id %in% common_ids)
      cqi_final_df <- cqi_filtered %>% filter(id %in% common_ids)
      message(sprintf("INFO: -> Final PQI/CQI filtering applied. Final PQI rows: %d, Final CQI rows: %d", nrow(pqi_final_df), nrow(cqi_final_df)))
    } else {
      message("WARN: Skipping final PQI/CQI filtering due to empty inputs or missing columns.")
    }

    calculation_end_time <- Sys.time()
    message(sprintf("INFO: Calculation sequence successful. Duration: %.2f seconds", difftime(calculation_end_time, calculation_start_time, units = "secs")))

    # Return successful results list
    list(
      success = TRUE, message = "Recalculation successful.", parameters = current_params,
      dataSummary = list(rawPrimesCount = nrow(new_raw_primes_df), splinePrimesCount = nrow(new_spline_primes_df), pqiCount = nrow(pqi_final_df), cqiCount = nrow(cqi_final_df), fullDataRows = nrow(updated_full_data_df)),
      rawPrimes = new_raw_primes_df, splinePrimes = new_spline_primes_df, pqi = pqi_final_df, cqi = cqi_final_df,
      fullData = updated_full_data_df # Return the dataframe with the *newly calculated* scaled_value
    )

  }, error = function(e) { # Catch errors from ANY calculation step
    error_message <- paste("Internal Server Error during calculation:", conditionMessage(e))
    message("ERROR: ", error_message)
    tb <- tryCatch(rlang::trace_back(bottom = sys.frame(1)), error = function(e_tb) "Traceback not available")
    message("ERROR Traceback:\n", paste(capture.output(print(tb)), collapse = "\n"))
    return(e) # Return the error object
  }) # End main calculation tryCatch

  # --- Prepare Final Response ---
  final_response_list <- list()
  if (inherits(calculation_result, "error")) {
    res$status <- 500 # Internal Server Error
    results$success <- FALSE
    results$message <- paste("Internal server error during calculation. Check logs. Ref:", calculation_result$message)
    results$rawPrimes <- list(); results$splinePrimes <- list(); results$pqi <- list(); results$cqi <- list(); results$fullData = list()
    final_response_list <- results
  } else {
    final_response_list <- calculation_result
    res$status <- 200 # OK
  }

  # Final check: Ensure the result is a list suitable for JSON conversion
  if (!is.list(final_response_list)) {
    message("CRITICAL ERROR: Final result object is not a list before JSON serialization.");
    res$status <- 500
    final_response_list <- list(success = FALSE, message = "Internal server error: Invalid response structure.", parameters = current_params, dataSummary = list(), rawPrimes = list(), splinePrimes = list(), pqi = list(), cqi = list(), fullData = list())
  }

  # --- Serialize to JSON ---
  response_body <- tryCatch({
    jsonlite::toJSON(final_response_list, auto_unbox = TRUE, na = "null", digits = NA, pretty = FALSE)
  }, error = function(e) {
    error_msg_json <- "Internal server error: Failed to serialize results to JSON."
    message("CRITICAL ERROR: ", error_msg_json, " JSON Error: ", conditionMessage(e));
    res$status <- 500
    sprintf('{"success": false, "message": "%s", "parameters": %s, "dataSummary": {}, "rawPrimes": [], "splinePrimes": [], "pqi": [], "cqi": [], "fullData": []}',
            gsub('"', '\\\\"', error_msg_json),
            jsonlite::toJSON(current_params, auto_unbox = TRUE, na = "null") %||% '{}'
    )
  })

  # --- Final Logging and Return ---
  endpoint_duration <- difftime(Sys.time(), endpoint_start_time, units = "secs")
  message(sprintf("INFO: /recalculate completed. Status: %d. Duration: %.2f seconds",
                  res$status %||% 500, endpoint_duration))

  res$setHeader("Content-Type", "application/json")
  res$body <- response_body
  return(res)
} # End /recalculate endpoint

#* API Status / Health Check
#* @get /
#* @serializer contentType list(type="application/json")
function(req, res){
  message("INFO: / (health check) endpoint invoked.")
  response_list <- list()

  # Check for the initial load error first
  if (exists("LOAD_ERROR", envir = .API_ENV)) {
    res$status <- 503 # Service Unavailable
    response_list <- list(
      status = "ERROR",
      message = paste("API is running BUT critical base data failed to load. Service impaired. Startup Error:",
                      conditionMessage(.API_ENV$LOAD_ERROR)),
      data_loaded = FALSE,
      timestamp = Sys.time()
    )
    message("ERROR: Health Check Failed (Startup Data Load Error).")
  } else {
    # Check if data objects exist and are valid
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
      message("INFO: Health Check OK.")
    } else {
      res$status <- 503 # Service Unavailable
      response_list <- list(
        status = "ERROR",
        message = "API is running BUT essential base data is missing, invalid, or empty at runtime. Service impaired.",
        data_loaded = FALSE,
        timestamp = Sys.time()
      )
      message("ERROR: Health Check Failed (Runtime Data Invalid/Missing).")
    }
  }

  # Serialize response and return
  res$setHeader("Content-Type", "application/json")
  res$body <- jsonlite::toJSON(response_list, auto_unbox = TRUE, na = "null", pretty = FALSE)
  message("INFO: / (health check) completed.")
  return(res)
} # End / endpoint

# --- Plumber Entrypoint ---
#* @plumber
function(pr) {
  message("INFO: Plumber router object created.")
  pr # Return the router object
}
