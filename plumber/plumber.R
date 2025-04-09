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
  library(cluster)    # Needed by cluster_player_performance (implicitly used if called)
  library(ggplot2)    # Needed by cluster_player_performance if plotting enabled (though not in API context)
})
message("Required packages loaded. Custom package 'peakPerformR' loaded.")

# --- Utility Functions ---
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
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
  clusters_path <- file.path(data_dir, "precalc_cluster_data.rds") # This should contain the correct scaled_value

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
  .API_ENV$CLUSTER_DATA <- readRDS(clusters_path) # Load the cluster data containing scaled_value

  stopifnot(
    "`SPORTS_FILTERED` did not load as a non-empty data frame." = inherits(.API_ENV$SPORTS_FILTERED, "data.frame") && nrow(.API_ENV$SPORTS_FILTERED) > 0,
    "`PLAYER_TRAJECTORIES` did not load as a non-empty data frame." = inherits(.API_ENV$PLAYER_TRAJECTORIES, "data.frame") && nrow(.API_ENV$PLAYER_TRAJECTORIES) > 0,
    "`CLUSTER_DATA` did not load as a non-empty data frame." = inherits(.API_ENV$CLUSTER_DATA, "data.frame") && nrow(.API_ENV$CLUSTER_DATA) > 0
  )
  # **Crucial Check**: Ensure CLUSTER_DATA has the necessary columns
  stopifnot(
      "CLUSTER_DATA must contain 'id', 'age', and 'scaled_value' columns." =
      all(c("id", "age", "scaled_value") %in% names(.API_ENV$CLUSTER_DATA))
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

  # Handle preflight (OPTIONS) requests
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
    res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization") # Adjust headers as needed by your app
    res$setHeader("Access-Control-Allow-Credentials", "true") # Often needed with frontend frameworks
    res$status <- 204 # No Content for OPTIONS preflight often preferred
    return(list()) # Respond to OPTIONS request immediately
  } else {
    # Also add Credentials header for actual requests if needed
    res$setHeader("Access-Control-Allow-Credentials", "true")
    plumber::forward() # Continue processing for non-OPTIONS requests
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
  # Safely get status code, default to 500 if unavailable (indicates early exit/error)
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
    base_cluster_data <- .API_ENV$CLUSTER_DATA # This contains the correct scaled_value
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
    # Note: create_player_performance_dataset uses base_sports_data and primes, then takes cluster_data separately
    temp_player_data <- peakPerformR::create_player_performance_dataset(sports_data = base_sports_data, prime_data = new_spline_primes_df, cluster_data = base_cluster_data)
    stopifnot("Create performance dataset failed." = is.data.frame(temp_player_data))

    temp_processed_data <- peakPerformR::process_player_primes(temp_player_data, base_sports_data)
    stopifnot("Process player primes failed." = is.data.frame(temp_processed_data))

    # --- Construct `updated_full_data_df` with CORRECT scaled_value ---
    message("INFO: Constructing/Updating fullData with correct scaled_value and flags...")
    updated_full_data_df <- base_sports_data # Start with the original full data

    # --- *** MODIFICATION START: Integrate scaled_value from CLUSTER_DATA *** ---
    message("INFO: -> Attempting to merge 'scaled_value' from CLUSTER_DATA...")
    if (exists(".API_ENV$CLUSTER_DATA") && nrow(.API_ENV$CLUSTER_DATA) > 0 &&
        all(c("id", "age", "scaled_value") %in% names(.API_ENV$CLUSTER_DATA))) {

        cluster_scaled_values <- .API_ENV$CLUSTER_DATA %>%
          # **Also bring performance_tier if it exists in cluster_data**
          select(id, age, calculated_scaled_value = scaled_value, any_of("performance_tier")) %>%
          distinct(id, age, .keep_all = TRUE) # Ensure unique id-age combo

        # Join the calculated scaled values and tier, potentially replacing existing columns
        updated_full_data_df <- updated_full_data_df %>%
          # Remove any old potentially incorrect scaled_value and tier first
          select(-any_of(c("scaled_value", "performance_tier"))) %>%
          left_join(cluster_scaled_values, by = c("id", "age")) %>%
          # Rename the correctly calculated value to 'scaled_value'
          rename(scaled_value = calculated_scaled_value)

         message(sprintf("INFO: -> 'scaled_value' (and 'performance_tier' if present) merged into fullData from CLUSTER_DATA. %d rows updated.", nrow(updated_full_data_df)))

    } else {
        message("WARN: CLUSTER_DATA missing, invalid, or lacks required columns (id, age, scaled_value). Cannot merge scaled_value.")
        # Fallback: Ensure the column exists, maybe using the old logic or setting to NA
        if (!"scaled_value" %in% names(updated_full_data_df)) {
             # Try renaming 'value' or add NA column as per your original logic
             potential_value_col <- "value" # Check if the original data had a 'value' column
             if (potential_value_col %in% names(updated_full_data_df)) {
                 message(sprintf("WARN: -> Using fallback: Renaming '%s' to 'scaled_value'.", potential_value_col))
                 updated_full_data_df <- updated_full_data_df %>% rename(scaled_value = !!potential_value_col)
             } else {
                 message("WARN: -> Using fallback: Adding 'scaled_value' column with NA values.")
                 updated_full_data_df$scaled_value <- NA_real_
             }
        } else {
             message("INFO: -> 'scaled_value' column already exists, leaving as is due to CLUSTER_DATA issue.")
        }
        # Ensure performance_tier exists if cluster data was missing/invalid
        if (!"performance_tier" %in% names(updated_full_data_df)) {
             updated_full_data_df$performance_tier <- NA_character_
             message("WARN: -> Added 'performance_tier' column with NA values.")
        }
    }
    # --- *** MODIFICATION END *** ---


    # --- Calculate flags based on new spline primes ---
    # Now uses the updated_full_data_df which SHOULD have the correct scaled_value
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
          # Ensure is_peak_age handles potential floating point inaccuracies if ages aren't integers
          is_peak_age = !is.na(prime_peak_age) & !is.na(age) &
                         abs(age - prime_peak_age) < 1e-6, # Safer comparison for numeric age
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

      message(sprintf("INFO: -> Full data flags calculated based on new primes (in_prime: %d T / %d F, is_peak_age: %d T / %d F, years_from_peak calculated)",
                      sum(updated_full_data_df$in_prime == "true", na.rm=TRUE), sum(updated_full_data_df$in_prime == "false", na.rm=TRUE),
                      sum(updated_full_data_df$is_peak_age == "true", na.rm=TRUE), sum(updated_full_data_df$is_peak_age == "false", na.rm=TRUE)))

    } else {
      message("WARN: Spline primes data empty or missing required columns. Setting flags to FALSE/NA in fullData.")
      # Ensure flag columns exist even if primes are missing
      if (!"in_prime" %in% names(updated_full_data_df)) updated_full_data_df$in_prime <- "false"
      else updated_full_data_df$in_prime <- ifelse(is.na(updated_full_data_df$in_prime), "false", updated_full_data_df$in_prime) # Ensure no NAs remain if column existed

      if (!"is_peak_age" %in% names(updated_full_data_df)) updated_full_data_df$is_peak_age <- "false"
      else updated_full_data_df$is_peak_age <- ifelse(is.na(updated_full_data_df$is_peak_age), "false", updated_full_data_df$is_peak_age)

      if (!"years_from_peak" %in% names(updated_full_data_df)) updated_full_data_df$years_from_peak <- NA_integer_
      else updated_full_data_df$years_from_peak[is.na(updated_full_data_df$years_from_peak)] <- NA_integer_ # Ensure existing NAs remain NA
    }
    # --- End flag calculation ---

    # --- Final Check: Ensure ALL required columns exist before proceeding ---
    required_cols <- c("id", "player_name", "age", "league", "sport", "position",
                       "scaled_value", "in_prime", "is_peak_age", "years_from_peak",
                       "games_played", "performance_tier") # Added performance_tier
    missing_cols <- required_cols[!required_cols %in% names(updated_full_data_df)]
    if(length(missing_cols) > 0) {
        # Add missing columns as NA of appropriate type
        for(col in missing_cols) {
           message(sprintf("WARN: Final Check - Adding missing required column '%s' as NA.", col))
           # Assign appropriate NA type based on expected column type
           if (col %in% c("scaled_value", "games_played")) { updated_full_data_df[[col]] <- NA_real_ }
           else if (col == "years_from_peak") { updated_full_data_df[[col]] <- NA_integer_ }
           else if (col %in% c("in_prime", "is_peak_age")) { updated_full_data_df[[col]] <- "false" } # Default flags to false
           else { updated_full_data_df[[col]] <- NA_character_ } # Default others to character
        }
        # Re-check after adding NAs - this should now pass unless column name is misspelled above
        final_missing <- required_cols[!required_cols %in% names(updated_full_data_df)]
        if(length(final_missing) > 0) {
           stop(paste("FATAL: Final updated_full_data_df STILL missing required columns after adding NAs:", paste(final_missing, collapse=", ")))
        }
    }
    message("INFO: -> Final column check passed for updated_full_data_df.")
    # --- End Final Check ---


    # --- Step 3: PQI Calculation ---
    message("INFO: Calculation Step 3: Calculating PQI...")
    # calculate_prime_quality_index uses the data structure from process_player_primes
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
    # Prepare data for CQI using the *fully updated* full_data
    # Ensure required columns are present and correctly named
    base_traj_minimal <- base_trajectory_data %>% select(id, age, predicted_value) %>% distinct(id, age, .keep_all = TRUE)
    processed_minimal <- temp_processed_data %>% select(id, career_avg_tier = mean_tier) %>% distinct(id, .keep_all=TRUE) # Use mean_tier if that's the output name

    # Check required columns for CQI prep
    cols_for_cqi_prep <- c("id", "age", "league", "sport", "position", "scaled_value", "games_played") # Base columns needed
    if (!all(cols_for_cqi_prep %in% names(updated_full_data_df))) {
        stop(paste("FATAL: Cannot prepare data for CQI. updated_full_data_df is missing one or more columns:",
                   paste(cols_for_cqi_prep[!cols_for_cqi_prep %in% names(updated_full_data_df)], collapse=", ")))
    }

    data_for_cqi <- updated_full_data_df %>%
                        select(all_of(cols_for_cqi_prep), any_of("season")) %>% # Select only necessary columns + season if exists
                        left_join(base_traj_minimal, by = c("id","age")) %>%
                        left_join(processed_minimal, by = "id")

    # Verify CQI function expected columns are present after joins
    cols_needed_by_cqi_func <- c("id", "age", "league", "sport", "position", "scaled_value", "predicted_value", "career_avg_tier", "games_played") # Check your function's needs
    if (!all(cols_needed_by_cqi_func %in% names(data_for_cqi))) {
         missing_cqi_cols <- cols_needed_by_cqi_func[!cols_needed_by_cqi_func %in% names(data_for_cqi)]
         stop(paste("FATAL: Data prepared for CQI (data_for_cqi) is missing columns required by calculate_career_quality_index:", paste(missing_cqi_cols, collapse=", ")))
    }
    message(sprintf("INFO: -> Data prepared for CQI: %d rows", nrow(data_for_cqi)))

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
    # This step only affects the pqi and cqi dataframes being returned, not fullData
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

    # Return successful results list, ensuring the corrected updated_full_data_df is used
    list(
      success = TRUE, message = "Recalculation successful.", parameters = current_params,
      dataSummary = list(rawPrimesCount = nrow(new_raw_primes_df), splinePrimesCount = nrow(new_spline_primes_df), pqiCount = nrow(pqi_final_df), cqiCount = nrow(cqi_final_df), fullDataRows = nrow(updated_full_data_df)),
      rawPrimes = new_raw_primes_df, splinePrimes = new_spline_primes_df, pqi = pqi_final_df, cqi = cqi_final_df,
      fullData = updated_full_data_df # Crucial: Return the fully updated dataframe
    )

  }, error = function(e) { # Catch errors from ANY calculation step
    error_message <- paste("Internal Server Error during calculation:", conditionMessage(e))
    message("ERROR: ", error_message)
    # Attempt to get traceback for detailed debugging
    tb <- tryCatch(rlang::trace_back(bottom = sys.frame(1)), error = function(e_tb) "Traceback not available")
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
    results$message <- paste("Internal server error during calculation. Please check server logs or contact support. Ref:", calculation_result$message)
    # Clear data fields in error response
    results$rawPrimes <- list(); results$splinePrimes <- list(); results$pqi <- list(); results$cqi <- list(); results$fullData = list()
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
    final_response_list <- list(success = FALSE, message = "Internal server error: Invalid response structure generated.", parameters = current_params, dataSummary = list(), rawPrimes = list(), splinePrimes = list(), pqi = list(), cqi = list(), fullData = list())
  }

  # --- Serialize to JSON ---
  response_body <- tryCatch({
    # Using na = "string" might be safer for some JS libraries if NAs are common
    jsonlite::toJSON(final_response_list, auto_unbox = TRUE, na = "null", digits = NA, pretty = FALSE)
  }, error = function(e) {
    error_msg_json <- "Internal server error: Failed to serialize results to JSON."
    message("CRITICAL ERROR: ", error_msg_json, " JSON Error: ", conditionMessage(e));
    res$status <- 500
    # Manually construct a safe JSON error response
    sprintf('{"success": false, "message": "%s", "parameters": %s, "dataSummary": {}, "rawPrimes": [], "splinePrimes": [], "pqi": [], "cqi": [], "fullData": []}',
            gsub('"', '\\\\"', error_msg_json), # Escape quotes in message
            jsonlite::toJSON(current_params, auto_unbox = TRUE, na = "null") %||% '{}'
    )
  })

  # --- Final Logging and Return ---
  endpoint_duration <- difftime(Sys.time(), endpoint_start_time, units = "secs")
  message(sprintf("INFO: /recalculate completed. Status: %d. Duration: %.2f seconds",
                  res$status %||% 500, # Use 500 if status is somehow NULL
                  endpoint_duration))

  res$setHeader("Content-Type", "application/json") # Explicitly set content type
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
      message = paste("API is running BUT critical base data failed to load during startup. Service is impaired. Check server logs. Startup Error:",
                      conditionMessage(.API_ENV$LOAD_ERROR)),
      data_loaded = FALSE,
      timestamp = Sys.time()
    )
    message("ERROR: Health Check Failed (Startup Data Load Error).")
  } else {
    # Check if data objects exist and are valid within the environment
    data_check_passed <- tryCatch({
      stopifnot(
        exists("SPORTS_FILTERED", envir = .API_ENV) && inherits(.API_ENV$SPORTS_FILTERED, "data.frame") && nrow(.API_ENV$SPORTS_FILTERED) > 0,
        exists("PLAYER_TRAJECTORIES", envir = .API_ENV) && inherits(.API_ENV$PLAYER_TRAJECTORIES, "data.frame") && nrow(.API_ENV$PLAYER_TRAJECTORIES) > 0,
        exists("CLUSTER_DATA", envir = .API_ENV) && inherits(.API_ENV$CLUSTER_DATA, "data.frame") && nrow(.API_ENV$CLUSTER_DATA) > 0 && all(c("id","age","scaled_value") %in% names(.API_ENV$CLUSTER_DATA)) # Also check essential columns in cluster data
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
  res$setHeader("Content-Type", "application/json") # Explicitly set content type
  res$body <- jsonlite::toJSON(response_list, auto_unbox = TRUE, na = "null", pretty = FALSE)
  message("INFO: / (health check) completed.")
  return(res)
} # End / endpoint

# --- Plumber Entrypoint ---
#* @plumber
function(pr) {
  message("INFO: Plumber router object created.")
  # Optionally add global error handler here if desired
  # pr$registerHooks(list(
  #   "error" = function(req, res, err){
  #     message("GLOBAL ERROR HANDLER:", conditionMessage(err))
  #     res$status <- 500
  #     res$body <- toJSON(list(error=unbox("An unexpected error occurred.")), auto_unbox = TRUE)
  #     res
  #   }
  # ))
  pr # Return the router object
}
