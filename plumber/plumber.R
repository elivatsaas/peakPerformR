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

  # Load data filtered to only include players with splines
  sports_filtered_path <- file.path(data_dir, "precalc_sports_filtered.rds")
  trajectories_path <- file.path(data_dir, "precalc_player_trajectories.rds")
  clusters_path <- file.path(data_dir, "precalc_cluster_data.rds") # Still loaded for potential use elsewhere

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
  .API_ENV$CLUSTER_DATA <- readRDS(clusters_path)

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
    # Since base data is already filtered, joins should align naturally
    spline_join_data <- base_sports_data %>% select(id, league, position, age, games_played) %>% distinct(id, age, .keep_all = TRUE)
    spline_input_data <- base_trajectory_data %>% left_join(spline_join_data, by = c("id", "age"))
    stopifnot("Data prep for spline input failed." = is.data.frame(spline_input_data))

    # Log package version being used
    message("INFO: peakPerformR version in API: ", as.character(packageVersion("peakPerformR")))

    # Calculate primes using the predicted trajectory values
    new_spline_primes_df <- peakPerformR::identify_prime(spline_input_data, method = "predicted", threshold_pct = threshold_pct, games_pct_threshold = games_pct_threshold)
    stopifnot("Spline prime identification failed." = is.data.frame(new_spline_primes_df))
    message(sprintf("INFO: -> Spline primes identified: %d rows", nrow(new_spline_primes_df)))

    # Calculate primes using the raw actual player values
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

    # Check NAs in predicted_value loaded by API
    message("DEBUG API: Checking NAs in loaded PLAYER_TRAJECTORIES$predicted_value...")
    if (exists("PLAYER_TRAJECTORIES", envir = .API_ENV) && "predicted_value" %in% names(.API_ENV$PLAYER_TRAJECTORIES)) {
      na_count_pred <- sum(is.na(.API_ENV$PLAYER_TRAJECTORIES$predicted_value))
      message("DEBUG API: Number of NAs in loaded predicted_value: ", na_count_pred)
    } else {
      message("DEBUG API: PLAYER_TRAJECTORIES or predicted_value column not found in .API_ENV")
    }

    traj_minimal <- base_trajectory_data %>%
      select(id, age, predicted_value) %>%
      distinct(id, age, .keep_all = TRUE)

    data_to_scale <- base_sports_data %>%
      select(id, age, league, position) %>%
      distinct(id, age, .keep_all = TRUE) %>%
      left_join(traj_minimal, by = c("id", "age"))

    stopifnot("Failed to join trajectory data for scaling." = is.data.frame(data_to_scale))

    # Perform the Z-score scaling within league/position groups
    scaled_values_calculated <- data_to_scale |>
      filter(!is.na(league), !is.na(position)) |>
      group_by(league, position) |>
      mutate(
        group_mean = mean(predicted_value, na.rm = TRUE),
        group_sd = sd(predicted_value, na.rm = TRUE),
        calculated_scaled_value = case_when(
            is.na(predicted_value) ~ NA_real_,
            is.na(group_sd) ~ 0,
            group_sd == 0 ~ 0,
            TRUE ~ (predicted_value - group_mean) / group_sd
        )
      ) |>
      ungroup() |>
      select(id, age, calculated_scaled_value)

    stopifnot("Scaling calculation failed." = is.data.frame(scaled_values_calculated))
    message(sprintf("INFO: -> Scaled values calculated for %d id-age records.", nrow(scaled_values_calculated)))

    # --- Update 'fullData' - Start with base data, merge calculated scaled_value ---
    message("INFO: Updating fullData dataframe...")
    # Start with the base sports data (which is already filtered to players with splines)
    updated_full_data_df <- base_sports_data %>%
      select(-any_of("scaled_value")) %>% # Remove old column if it exists
      left_join(scaled_values_calculated, by = c("id", "age")) %>%
      rename(scaled_value = calculated_scaled_value)

    if (!"scaled_value" %in% names(updated_full_data_df)) {
      message("WARN: 'scaled_value' column unexpectedly missing after join. Adding NA column.")
      updated_full_data_df$scaled_value <- NA_real_
    } else {
       num_scaled <- sum(!is.na(updated_full_data_df$scaled_value))
       message(sprintf("INFO: -> Merged calculated 'scaled_value'. %d non-NA values present.", num_scaled))
    }
    # --- End scaled_value update ---


    # --- Calculate flags and add prime_duration to updated_full_data_df ---
    message("INFO: -> Calculating flags (in_prime, etc.) AND adding prime_duration...")
    if (nrow(new_spline_primes_df) > 0 &&
        # Check for all required columns from identify_prime output
        all(c("id", "start_age", "end_age", "max_value_age", "prime_duration") %in% names(new_spline_primes_df))) {

      primes_info_to_join <- new_spline_primes_df %>%
        # Select necessary columns including prime_duration
        select(id, prime_start_age = start_age, prime_end_age = end_age, prime_peak_age = max_value_age, prime_duration) %>%
        distinct(id, .keep_all = TRUE) # Ensure one row per player

      # Join prime info into the main data frame
      updated_full_data_df <- updated_full_data_df %>%
        left_join(primes_info_to_join, by = "id") %>%
        # Ensure numeric types for age comparisons
        mutate(across(c(age, prime_start_age, prime_end_age, prime_peak_age), as.numeric)) %>%
        mutate(
          # Calculate flags
          in_prime = !is.na(prime_start_age) & !is.na(prime_end_age) & !is.na(age) &
                       age >= prime_start_age & age <= prime_end_age,
          is_peak_age = !is.na(prime_peak_age) & !is.na(age) &
                         abs(age - prime_peak_age) < 1e-6,
          years_from_peak = if_else(!is.na(age) & !is.na(prime_peak_age),
                                    as.integer(round(age - prime_peak_age)),
                                    NA_integer_),
          # Ensure prime_duration has correct type and handles potential NAs from join (though unlikely now)
          prime_duration = if_else(is.na(prime_duration), NA_integer_, as.integer(prime_duration))
        ) %>%
         mutate(
            # Convert logical flags to character for JSON output consistency
            in_prime = if_else(is.na(in_prime), "false", if_else(in_prime, "true", "false")),
            is_peak_age = if_else(is.na(is_peak_age), "false", if_else(is_peak_age, "true", "false"))
         ) %>%
        # Remove temporary age columns, KEEP prime_duration
        select(-prime_start_age, -prime_end_age, -prime_peak_age)

      # Optional Debug Log: Check prime_duration summary after join
      # message("DEBUG API: Summary of prime_duration in updated_full_data_df:")
      # print(summary(updated_full_data_df$prime_duration))

      message(sprintf("INFO: -> Full data flags & prime_duration added (in_prime: %d T / %d F, is_peak_age: %d T / %d F)",
                      sum(updated_full_data_df$in_prime == "true", na.rm=TRUE), sum(updated_full_data_df$in_prime == "false", na.rm=TRUE),
                      sum(updated_full_data_df$is_peak_age == "true", na.rm=TRUE), sum(updated_full_data_df$is_peak_age == "false", na.rm=TRUE)))

    } else {
      message("WARN: Spline primes data empty or missing required columns. Setting flags/prime_duration to FALSE/NA in fullData.")
      # Ensure flag columns exist even if primes are missing
      if (!"in_prime" %in% names(updated_full_data_df)) updated_full_data_df$in_prime <- "false"
      else updated_full_data_df$in_prime <- ifelse(is.na(updated_full_data_df$in_prime), "false", updated_full_data_df$in_prime)

      if (!"is_peak_age" %in% names(updated_full_data_df)) updated_full_data_df$is_peak_age <- "false"
      else updated_full_data_df$is_peak_age <- ifelse(is.na(updated_full_data_df$is_peak_age), "false", updated_full_data_df$is_peak_age)

      if (!"years_from_peak" %in% names(updated_full_data_df)) updated_full_data_df$years_from_peak <- NA_integer_
      else updated_full_data_df$years_from_peak[is.na(updated_full_data_df$years_from_peak)] <- NA_integer_

      # Add prime_duration as NA if it doesn't exist
      if (!"prime_duration" %in% names(updated_full_data_df)) updated_full_data_df$prime_duration <- NA_integer_
       else updated_full_data_df$prime_duration[is.na(updated_full_data_df$prime_duration)] <- NA_integer_
    }
    # --- End flag calculation ---

    # --- Final Check: Ensure ALL required columns exist before proceeding ---
    # Added prime_duration to required columns
    required_cols <- c("id", "player_name", "age", "league", "sport", "position",
                       "scaled_value", "in_prime", "is_peak_age", "years_from_peak",
                       "games_played", "prime_duration")
    missing_cols <- required_cols[!required_cols %in% names(updated_full_data_df)]
    if(length(missing_cols) > 0) {
        for(col in missing_cols) {
           message(sprintf("WARN: Final Check - Adding missing required column '%s' as NA.", col))
           if (col %in% c("scaled_value", "games_played")) { updated_full_data_df[[col]] <- NA_real_ }
           # Correctly handle integer columns
           else if (col %in% c("years_from_peak", "prime_duration")) { updated_full_data_df[[col]] <- NA_integer_ }
           else if (col %in% c("in_prime", "is_peak_age")) { updated_full_data_df[[col]] <- "false" }
           else { updated_full_data_df[[col]] <- NA_character_ }
        }
        final_missing <- required_cols[!required_cols %in% names(updated_full_data_df)]
        if(length(final_missing) > 0) {
           # This is a critical failure, stop execution
           stop(paste("FATAL: Final updated_full_data_df STILL missing required columns after adding NAs:", paste(final_missing, collapse=", ")))
        }
    }
    message("INFO: -> Final column check passed for updated_full_data_df.")
    # --- End Final Check ---

    # --- Get Valid IDs from Trajectory Data ---
    message("INFO: Getting valid player IDs from trajectory data...")
    valid_trajectory_ids <- character(0)
    if (exists("PLAYER_TRAJECTORIES", envir = .API_ENV) && "id" %in% names(.API_ENV$PLAYER_TRAJECTORIES)) {
        valid_trajectory_ids <- unique(.API_ENV$PLAYER_TRAJECTORIES$id)
        message(sprintf("INFO: Found %d unique player IDs in PLAYER_TRAJECTORIES.", length(valid_trajectory_ids)))
    } else {
        message("WARN: PLAYER_TRAJECTORIES or its 'id' column not found. Cannot filter CQI input by trajectory IDs.")
    }
    # --- End Get Valid IDs ---


    # --- Step 3: PQI Calculation ---
    message("INFO: Calculation Step 3: Calculating PQI...")
    pqi_calculated_df <- peakPerformR::calculate_prime_quality_index(temp_processed_data, nfl_by_position = TRUE, tier_method = "percentile")
    stopifnot("PQI calculation failed." = is.data.frame(pqi_calculated_df))
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
    # Prepare base data needed for CQI
    base_traj_minimal <- base_trajectory_data %>% select(id, age, predicted_value) %>% distinct(id, age, .keep_all = TRUE)
    # Select necessary columns from processed data - ENSURE 'career_avg_tier' IS CORRECT
    # !!! Check if `process_player_primes` actually produces 'career_avg_tier' !!!
    processed_cols_to_select <- intersect(c("id", "career_avg_tier"), names(temp_processed_data))
    if (!"career_avg_tier" %in% processed_cols_to_select) {
        message("WARN: 'career_avg_tier' not found in temp_processed_data. Joining may fail or column will be NA.")
        # Handle this gracefully, maybe add an NA column if critical for CQI function?
        processed_minimal <- temp_processed_data %>% select(id) %>% distinct(id, .keep_all=TRUE) %>% mutate(career_avg_tier = NA_character_)
    } else {
        processed_minimal <- temp_processed_data %>% select(all_of(processed_cols_to_select)) %>% distinct(id, .keep_all=TRUE)
    }
    message("DEBUG API: Check 'career_avg_tier' exists in temp_processed_data: ", "career_avg_tier" %in% names(temp_processed_data))


    # Create the base data for CQI input
    # Note: updated_full_data_df now contains prime_duration
    data_for_cqi <- updated_full_data_df %>%
                        left_join(base_traj_minimal, by = c("id","age")) %>%
                        left_join(processed_minimal, by = "id")
    stopifnot("Data preparation for CQI failed (check columns)." = is.data.frame(data_for_cqi))
    message(sprintf("INFO: Prepared data_for_cqi with %d rows (before filtering).", nrow(data_for_cqi)))

    # --- FILTERING STEP for CQI Input ---
    data_for_cqi_filtered <- data_for_cqi
    if (length(valid_trajectory_ids) > 0) {
        n_unique_ids_before_cqi_filter <- length(unique(data_for_cqi_filtered$id))
        message(sprintf("INFO: Filtering data_for_cqi (containing %d unique IDs) to include only %d valid trajectory IDs.", n_unique_ids_before_cqi_filter, length(valid_trajectory_ids)))
        data_for_cqi_filtered <- data_for_cqi %>%
                                   dplyr::filter(id %in% valid_trajectory_ids)
        n_unique_ids_after_cqi_filter <- length(unique(data_for_cqi_filtered$id))
        message(sprintf("INFO: Rows remaining in data_for_cqi after filtering: %d. Unique IDs remaining: %d", nrow(data_for_cqi_filtered), n_unique_ids_after_cqi_filter))
    } else {
        message("WARN: Skipping filtering of CQI data as no valid trajectory IDs were found.")
    }
    # Add check for number of unique IDs being passed
    n_unique_ids_passed <- length(unique(data_for_cqi_filtered$id))
    message(sprintf("DEBUG API: Number of UNIQUE IDs in data_for_cqi_filtered being passed to CQI function: %d", n_unique_ids_passed))

    # Add check for prime_duration column presence
    message("DEBUG API: Checking for 'prime_duration' column before CQI call: ", "prime_duration" %in% names(data_for_cqi_filtered))
    # Optional: summary of prime_duration if present
    # if("prime_duration" %in% names(data_for_cqi_filtered)) print(summary(data_for_cqi_filtered$prime_duration))


    # --- Call CQI function with FILTERED data ---
    # Ensure calculate_career_quality_index uses the prime_duration column
    cqi_calculated_df <- peakPerformR::calculate_career_quality_index(
        player_data = data_for_cqi_filtered, # Use the filtered data frame
        nfl_by_position = FALSE, # Your setting
        tier_method = "percentile",
        exclude_positions = c("OL", "SPEC"), # Your setting
        min_seasons = 5 # Your setting
    )
    stopifnot("CQI calculation failed." = is.data.frame(cqi_calculated_df))

    # Rename safely
    if (nrow(cqi_calculated_df) > 0 && all(c("career_tier", "cqi_score") %in% names(cqi_calculated_df))) {
      cqi_calculated_df <- cqi_calculated_df %>% rename(selected_tier = career_tier, cqi_selected = cqi_score)
    } else {
      message("WARN: CQI result empty or missing expected columns after calculation.")
       # Ensure columns exist even if empty, matching PQI structure
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
      # Ensure consistency based on players present in BOTH after SPEC filter
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
      fullData = updated_full_data_df # Return the dataframe with the newly calculated scaled_value AND prime_duration
    )

  }, error = function(e) { # Catch errors from ANY calculation step
    error_message <- paste("Internal Server Error during calculation:", conditionMessage(e))
    message("ERROR: ", error_message)
    tb <- tryCatch(rlang::trace_back(bottom = sys.frame(1)), error = function(e_tb) "Traceback not available")
    message("ERROR Traceback:\n", paste(capture.output(print(tb)), collapse = "\n"))
    # Return the error object itself to be handled below
     return(e)
  }) # End main calculation tryCatch

  # --- Prepare Final Response ---
  final_response_list <- list()
  if (inherits(calculation_result, "error")) {
    res$status <- 500 # Internal Server Error
    results$success <- FALSE
    results$message <- paste("Internal server error during calculation:", calculation_result$message)
    results$rawPrimes <- list(); results$splinePrimes <- list(); results$pqi <- list(); results$cqi <- list(); results$fullData = list()
    final_response_list <- results
  } else if (!is.list(calculation_result) || !("success" %in% names(calculation_result))) {
      # Handle cases where calculation_result is not a list or is malformed
      res$status <- 500
      results$success <- FALSE
      results$message <- "Internal server error: Calculation returned unexpected structure."
      results$rawPrimes <- list(); results$splinePrimes <- list(); results$pqi <- list(); results$cqi <- list(); results$fullData = list()
      final_response_list <- results
  } else {
    # Calculation was successful and returned the expected list structure
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
