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
  if (is.null(x) || length(x) == 0) y else x
}

# --- Load Pre-Computed Base Data (Load ONCE at API startup) ---
.API_ENV <- new.env(parent = emptyenv())

message("Loading pre-computed base data (filtered to players with splines)...")
tryCatch({
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
               "\nCurrent working directory reported as:", normalized_base_path))
  }

  .API_ENV$SPORTS_FILTERED_ORIG <- readRDS(sports_filtered_path) # Load original
  .API_ENV$PLAYER_TRAJECTORIES <- readRDS(trajectories_path)
  .API_ENV$CLUSTER_DATA <- readRDS(clusters_path)

  # Validate data after loading
  initial_player_ids <- unique(c(
      if(inherits(.API_ENV$SPORTS_FILTERED_ORIG, "data.frame")) unique(.API_ENV$SPORTS_FILTERED_ORIG$id) else character(0),
      if(inherits(.API_ENV$PLAYER_TRAJECTORIES, "data.frame")) unique(.API_ENV$PLAYER_TRAJECTORIES$id) else character(0),
      if(inherits(.API_ENV$CLUSTER_DATA, "data.frame")) unique(.API_ENV$CLUSTER_DATA$id) else character(0)
  ))
  .API_ENV$INITIAL_PLAYER_COUNT <- length(initial_player_ids)
  message(sprintf("Base data loaded. Found %d unique player IDs across initial datasets.", .API_ENV$INITIAL_PLAYER_COUNT))

  stopifnot(
    "`SPORTS_FILTERED_ORIG` did not load as a non-empty data frame." = inherits(.API_ENV$SPORTS_FILTERED_ORIG, "data.frame") && nrow(.API_ENV$SPORTS_FILTERED_ORIG) > 0,
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

#* CORS handler
#* @filter cors
function(req, res) {
  allowed_origin <- Sys.getenv("ALLOWED_ORIGIN", "http://localhost:3000")
  res$setHeader("Access-Control-Allow-Origin", allowed_origin)
  res$setHeader("Access-Control-Allow-Credentials", "true")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
    res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
    res$status <- 204
    return(list())
  } else {
    plumber::forward()
  }
}

#* Log incoming requests
#* @filter logger
function(req){
  start_time <- Sys.time()
  cat(format(start_time), "-", req$REQUEST_METHOD, req$PATH_INFO, "-", req$HTTP_USER_AGENT %||% "Unknown", "@", req$REMOTE_ADDR %||% "Unknown", "\n")
  plumber::forward()
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
    res$status <- 503
    err_msg <- paste("Service Unavailable: Critical base data failed to load.", conditionMessage(.API_ENV$LOAD_ERROR))
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
  params <- tryCatch(jsonlite::fromJSON(req$postBody %||% "{}", simplifyDataFrame = FALSE), error = function(e) { message("WARN: Failed to parse request body JSON. Using defaults. Error: ", conditionMessage(e)); list() })
  threshold_pct <- as.numeric(params$thresholdPct %||% default_threshold_pct)
  games_pct_threshold <- as.numeric(params$gamesPctThreshold %||% default_games_pct_threshold)
  current_params <- list(thresholdPct = threshold_pct, gamesPctThreshold = games_pct_threshold)
  results$parameters <- current_params

  # --- Input Validation ---
  param_errors <- character(0)
  if (length(threshold_pct) != 1 || is.na(threshold_pct) || !is.numeric(threshold_pct) || threshold_pct < 0 || threshold_pct > 100) param_errors <- c(param_errors, "Invalid 'thresholdPct'.")
  if (length(games_pct_threshold) != 1 || is.na(games_pct_threshold) || !is.numeric(games_pct_threshold) || games_pct_threshold < 0 || games_pct_threshold > 100) param_errors <- c(param_errors, "Invalid 'gamesPctThreshold'.")
  if (length(param_errors) > 0) {
    res$status <- 400; results$message <- paste("Input validation failed:", paste(param_errors, collapse = " ")); message("ERROR: Invalid input parameters: ", results$message); res$setHeader("Content-Type", "application/json"); res$body <- jsonlite::toJSON(results, auto_unbox = TRUE, na = "null"); return(res)
  }

  message(sprintf("INFO: Received valid request: thresholdPct=%.1f, gamesPctThreshold=%.1f", threshold_pct, games_pct_threshold))
  calculation_start_time <- Sys.time()

  # --- Core Calculation Logic (Wrapped in Error Handling) ---
  calculation_result <- tryCatch({
    # --- Step 0: Retrieve and Pre-process Base Data ---
    message("INFO: Calculation Step 0: Retrieving and Pre-processing Base Data...")
    # Retrieve original data
    base_sports_data_orig <- .API_ENV$SPORTS_FILTERED_ORIG
    base_trajectory_data <- .API_ENV$PLAYER_TRAJECTORIES
    base_cluster_data <- .API_ENV$CLUSTER_DATA
    message("INFO: -> Original base data retrieved.")

    # --- Step 0.5: Assign Most Common Position ---
    message("INFO: Step 0.5: Assigning most common position to each player...")
    # Calculate the most common non-NA position for each player
    most_common_positions <- base_sports_data_orig %>%
      filter(!is.na(position)) %>% # Exclude NA positions from counting initially
      count(id, position) %>%
      group_by(id) %>%
      # Arrange by count (desc) then position (alphabetical) to break ties consistently
      arrange(desc(n), position) %>%
      slice_head(n = 1) %>% # Select the top one
      ungroup() %>%
      select(id, most_common_position = position)

    # Join back and update the position column
    # Use the modified data going forward
    base_sports_data <- base_sports_data_orig %>%
      left_join(most_common_positions, by = "id") %>%
      mutate(
        # If a player only had NA positions, most_common_position will be NA, otherwise use the calculated one
        position = coalesce(most_common_position, position)
      ) %>%
      select(-most_common_position) # Remove the temporary column

    n_updated_pos <- sum(!is.na(most_common_positions$most_common_position))
    message(sprintf("INFO: -> Most common position assigned for %d players. Using this consistent position going forward.", n_updated_pos))
    # --- End Step 0.5 ---

    # --- Step 1: Prime Identification ---
    message("INFO: Calculation Step 1: Identifying Primes...!")
    # Join games_played data from the *now position-harmonized* base_sports_data
    spline_join_data <- base_sports_data %>% select(id, league, position, age, games_played) %>% distinct(id, age, .keep_all = TRUE)
    spline_input_data <- base_trajectory_data %>% left_join(spline_join_data, by = c("id", "age"))
    stopifnot("Data prep for spline input failed." = is.data.frame(spline_input_data))
    message("INFO: peakPerformR version in API: ", as.character(packageVersion("peakPerformR")))

    new_spline_primes_df <- peakPerformR::identify_prime(spline_input_data, method = "predicted", threshold_pct = threshold_pct, games_pct_threshold = games_pct_threshold)
    stopifnot("Spline prime identification failed." = is.data.frame(new_spline_primes_df))
    message(sprintf("INFO: -> Spline primes identified: %d rows", nrow(new_spline_primes_df)))
    spline_ids <- unique(new_spline_primes_df$id)
    message(sprintf("INFO: Unique IDs after Spline Prime calc: %d", length(spline_ids)))

    # Use position-harmonized base_sports_data for raw prime calculation
    new_raw_primes_df <- peakPerformR::identify_prime(base_sports_data, method = "actual", threshold_pct = threshold_pct, games_pct_threshold = games_pct_threshold)
    stopifnot("Raw prime identification failed." = is.data.frame(new_raw_primes_df))
    message(sprintf("INFO: -> Raw primes identified: %d rows", nrow(new_raw_primes_df)))

    # --- Step 2: Dependent Calculations (Performance Dataset & Processing) ---
    message("INFO: Calculation Step 2: Creating Performance Dataset & Processing Primes...")
    # Use position-harmonized base_sports_data here too
    temp_player_data <- peakPerformR::create_player_performance_dataset(sports_data = base_sports_data, prime_data = new_spline_primes_df, cluster_data = base_cluster_data)
    stopifnot("Create performance dataset failed." = is.data.frame(temp_player_data))
    temp_processed_data <- peakPerformR::process_player_primes(temp_player_data, base_sports_data)
    stopifnot("Process player primes failed." = is.data.frame(temp_processed_data))

    # --- Step 2.5: Calculate Scaled Value ---
    message("INFO: Calculation Step 2.5: Explicitly calculating scaled_value from predicted_value...")
    message("DEBUG API: Checking NAs in loaded PLAYER_TRAJECTORIES$predicted_value...")
    na_count_pred <- sum(is.na(base_trajectory_data$predicted_value))
    message("DEBUG API: Number of NAs in loaded predicted_value: ", na_count_pred)

    traj_minimal <- base_trajectory_data %>% select(id, age, predicted_value) %>% distinct(id, age, .keep_all = TRUE)
    # Use position-harmonized base_sports_data for joining grouping info
    data_to_scale <- base_sports_data %>% select(id, age, league, position) %>% distinct(id, age, .keep_all = TRUE) %>% left_join(traj_minimal, by = c("id", "age"))
    stopifnot("Failed to join trajectory data for scaling." = is.data.frame(data_to_scale))

    # Scaling now uses the consistent position
    scaled_values_calculated <- data_to_scale |> filter(!is.na(league), !is.na(position)) |> group_by(league, position) |>
      mutate(group_mean = mean(predicted_value, na.rm = TRUE), group_sd = sd(predicted_value, na.rm = TRUE),
             calculated_scaled_value = case_when(is.na(predicted_value) ~ NA_real_, is.na(group_sd) ~ 0, group_sd == 0 ~ 0, TRUE ~ (predicted_value - group_mean) / group_sd)) |>
      ungroup() |> select(id, age, calculated_scaled_value)
    stopifnot("Scaling calculation failed." = is.data.frame(scaled_values_calculated))
    message(sprintf("INFO: -> Scaled values calculated for %d id-age records.", nrow(scaled_values_calculated)))

    # --- Update 'fullData' ---
    message("INFO: Updating fullData dataframe...")
    # Start with the position-harmonized base data
    updated_full_data_df <- base_sports_data %>% select(-any_of("scaled_value")) %>%
      left_join(scaled_values_calculated, by = c("id", "age")) %>% rename(scaled_value = calculated_scaled_value)
    if (!"scaled_value" %in% names(updated_full_data_df)) { message("WARN: 'scaled_value' column missing. Adding NA column."); updated_full_data_df$scaled_value <- NA_real_ }
    else { num_scaled <- sum(!is.na(updated_full_data_df$scaled_value)); message(sprintf("INFO: -> Merged calculated 'scaled_value'. %d non-NA values present.", num_scaled)) }

    # --- Calculate flags and add prime_duration ---
    message("INFO: -> Calculating flags (in_prime, etc.) AND adding prime_duration...")
    if (nrow(new_spline_primes_df) > 0 && all(c("id", "start_age", "end_age", "max_value_age", "prime_duration") %in% names(new_spline_primes_df))) {
      primes_info_to_join <- new_spline_primes_df %>% select(id, prime_start_age = start_age, prime_end_age = end_age, prime_peak_age = max_value_age, prime_duration) %>% distinct(id, .keep_all = TRUE)
      updated_full_data_df <- updated_full_data_df %>% left_join(primes_info_to_join, by = "id") %>%
        mutate(across(c(age, prime_start_age, prime_end_age, prime_peak_age), as.numeric)) %>%
        mutate(prime_duration = if_else(is.na(prime_duration), NA_integer_, as.integer(prime_duration)),
               in_prime = !is.na(prime_start_age) & !is.na(prime_end_age) & !is.na(age) & age >= prime_start_age & age <= prime_end_age,
               is_peak_age = !is.na(prime_peak_age) & !is.na(age) & abs(age - prime_peak_age) < 1e-6,
               years_from_peak = if_else(!is.na(age) & !is.na(prime_peak_age), as.integer(round(age - prime_peak_age)), NA_integer_),
               in_prime = if_else(is.na(in_prime), "false", if_else(in_prime, "true", "false")),
               is_peak_age = if_else(is.na(is_peak_age), "false", if_else(is_peak_age, "true", "false"))) %>%
        select(-prime_start_age, -prime_end_age, -prime_peak_age)
      message(sprintf("INFO: -> Full data flags & prime_duration added (in_prime: %d T / %d F, is_peak_age: %d T / %d F)", sum(updated_full_data_df$in_prime == "true", na.rm=TRUE), sum(updated_full_data_df$in_prime == "false", na.rm=TRUE), sum(updated_full_data_df$is_peak_age == "true", na.rm=TRUE), sum(updated_full_data_df$is_peak_age == "false", na.rm=TRUE)))
    } else {
      message("WARN: Spline primes data empty/missing. Setting flags/prime_duration to FALSE/NA.")
      if (!"in_prime" %in% names(updated_full_data_df)) updated_full_data_df$in_prime <- "false" else updated_full_data_df$in_prime <- ifelse(is.na(updated_full_data_df$in_prime), "false", updated_full_data_df$in_prime)
      if (!"is_peak_age" %in% names(updated_full_data_df)) updated_full_data_df$is_peak_age <- "false" else updated_full_data_df$is_peak_age <- ifelse(is.na(updated_full_data_df$is_peak_age), "false", updated_full_data_df$is_peak_age)
      if (!"years_from_peak" %in% names(updated_full_data_df)) updated_full_data_df$years_from_peak <- NA_integer_ else updated_full_data_df$years_from_peak[is.na(updated_full_data_df$years_from_peak)] <- NA_integer_
      if (!"prime_duration" %in% names(updated_full_data_df)) updated_full_data_df$prime_duration <- NA_integer_ else updated_full_data_df$prime_duration[is.na(updated_full_data_df$prime_duration)] <- NA_integer_
    }

    # --- Final Column Check ---
    required_cols <- c("id", "player_name", "age", "league", "sport", "position", "scaled_value", "in_prime", "is_peak_age", "years_from_peak", "games_played", "prime_duration")
    missing_cols <- required_cols[!required_cols %in% names(updated_full_data_df)]
    if(length(missing_cols) > 0) {
        for(col in missing_cols) {
           message(sprintf("WARN: Final Check - Adding missing required column '%s' as NA.", col))
           if (col %in% c("scaled_value", "games_played")) updated_full_data_df[[col]] <- NA_real_
           else if (col %in% c("years_from_peak", "prime_duration")) updated_full_data_df[[col]] <- NA_integer_
           else if (col %in% c("in_prime", "is_peak_age")) updated_full_data_df[[col]] <- "false"
           else updated_full_data_df[[col]] <- NA_character_
        }
        final_missing <- required_cols[!required_cols %in% names(updated_full_data_df)]; if(length(final_missing) > 0) stop(paste("FATAL: Final updated_full_data_df STILL missing required columns:", paste(final_missing, collapse=", ")))
    }
    message("INFO: -> Final column check passed for updated_full_data_df.")

    # --- Step 3: PQI Calculation ---
    message("INFO: Calculation Step 3: Calculating PQI...")
    # temp_processed_data should now reflect consistent positions if process_player_primes uses the harmonized base_sports_data
    pqi_calculated_df <- peakPerformR::calculate_prime_quality_index(temp_processed_data, nfl_by_position = TRUE, tier_method = "percentile")
    stopifnot("PQI calculation failed." = is.data.frame(pqi_calculated_df))
    pqi_ids <- unique(pqi_calculated_df$id)
    message(sprintf("INFO: Unique IDs after PQI calc (before final filter): %d", length(pqi_ids)))
    if (nrow(pqi_calculated_df) > 0 && all(c("prime_tier", "pqi_score") %in% names(pqi_calculated_df))) { pqi_calculated_df <- pqi_calculated_df %>% rename(selected_tier = prime_tier, pqi_selected = pqi_score) }
    else { message("WARN: PQI result empty or missing expected columns."); if (!"selected_tier" %in% names(pqi_calculated_df)) pqi_calculated_df$selected_tier <- NA_character_; if (!"pqi_selected" %in% names(pqi_calculated_df)) pqi_calculated_df$pqi_selected <- NA_real_ }
    message(sprintf("INFO: -> PQI calculation complete: %d rows", nrow(pqi_calculated_df)))

    # --- Step 4: CQI Calculation ---
    message("INFO: Calculation Step 4: Calculating CQI...")
    base_traj_minimal <- base_trajectory_data %>% select(id, age, predicted_value) %>% distinct(id, age, .keep_all = TRUE)
    processed_cols_to_select <- intersect(c("id", "career_avg_tier"), names(temp_processed_data))
    if (!"career_avg_tier" %in% processed_cols_to_select) { message("WARN: 'career_avg_tier' not found."); processed_minimal <- temp_processed_data %>% select(id) %>% distinct(id) %>% mutate(career_avg_tier = NA_character_) }
    else { processed_minimal <- temp_processed_data %>% select(all_of(processed_cols_to_select)) %>% distinct(id, .keep_all=TRUE) }
    message("DEBUG API: Check 'career_avg_tier' exists in temp_processed_data: ", "career_avg_tier" %in% names(temp_processed_data))

    # Construct CQI input using the position-harmonized updated_full_data_df
    data_for_cqi <- updated_full_data_df %>% left_join(base_traj_minimal, by = c("id","age")) %>% left_join(processed_minimal, by = "id")
    stopifnot("Data preparation for CQI failed." = is.data.frame(data_for_cqi))
    n_unique_ids_before_cqi_call <- length(unique(data_for_cqi$id))
    message(sprintf("INFO: Prepared data_for_cqi with %d rows, containing %d unique IDs.", nrow(data_for_cqi), n_unique_ids_before_cqi_call))
    message("DEBUG API: Checking for 'prime_duration' column before CQI call: ", "prime_duration" %in% names(data_for_cqi))

    # Call CQI function - It should now receive data where each player has a consistent position
    cqi_calculated_df <- peakPerformR::calculate_career_quality_index(player_data = data_for_cqi, nfl_by_position = FALSE, tier_method = "percentile", exclude_positions = c("OL", "SPEC"), min_seasons = 5)
    stopifnot("CQI calculation failed." = is.data.frame(cqi_calculated_df))
    cqi_ids <- unique(cqi_calculated_df$id)
    message(sprintf("INFO: Unique IDs after CQI calc (before final filter): %d", length(cqi_ids)))
    if (nrow(cqi_calculated_df) > 0 && all(c("career_tier", "cqi_score") %in% names(cqi_calculated_df))) { cqi_calculated_df <- cqi_calculated_df %>% rename(selected_tier = career_tier, cqi_selected = cqi_score) }
    else { message("WARN: CQI result empty/missing columns."); if (!"selected_tier" %in% names(cqi_calculated_df)) cqi_calculated_df$selected_tier <- NA_character_; if (!"cqi_selected" %in% names(cqi_calculated_df)) cqi_calculated_df$cqi_selected <- NA_real_ }
    # The nrow here might still be > unique player IDs if CQI's summarise aggregates by position internally, but the underlying unique player count should be correct.
    message(sprintf("INFO: -> CQI calculation complete: %d rows returned by function.", nrow(cqi_calculated_df)))

    # --- Step 5: Final Intersection Filtering ---
    message("INFO: Calculation Step 5: Applying Final Intersection Filtering...")
    # Get unique IDs from each result set
    spline_ids_final <- unique(new_spline_primes_df$id)
    pqi_ids_final <- unique(pqi_calculated_df$id)
    cqi_ids_final <- unique(cqi_calculated_df$id)

    # Find the intersection of IDs present in ALL results
    common_ids_all <- base::Reduce(intersect, list(spline_ids_final, pqi_ids_final, cqi_ids_final))
    message(sprintf("INFO: Found %d common IDs across successful Spline Primes, PQI, and CQI results.", length(common_ids_all)))

    # Filter all dataframes to only include these common players
    raw_primes_final_df <- new_raw_primes_df %>% filter(id %in% common_ids_all) %>% distinct(id, .keep_all = TRUE) # Ensure unique players
    spline_primes_final_df <- new_spline_primes_df %>% filter(id %in% common_ids_all) %>% distinct(id, .keep_all = TRUE) # Ensure unique players
    pqi_final_df <- pqi_calculated_df %>% filter(id %in% common_ids_all) %>% distinct(id, .keep_all = TRUE) # Ensure unique players
    cqi_final_df <- cqi_calculated_df %>% filter(id %in% common_ids_all) %>% distinct(id, .keep_all = TRUE) # Ensure unique players
    # Keep all rows for full data for these players
    full_data_final_df <- updated_full_data_df %>% filter(id %in% common_ids_all)

    message(sprintf("INFO: -> Final data rows after intersection: Raw Primes=%d, Spline Primes=%d, PQI=%d, CQI=%d, Full Data=%d",
                    nrow(raw_primes_final_df), nrow(spline_primes_final_df),
                    nrow(pqi_final_df), nrow(cqi_final_df), nrow(full_data_final_df)))

    calculation_end_time <- Sys.time()
    message(sprintf("INFO: Calculation sequence successful. Duration: %.2f seconds", difftime(calculation_end_time, calculation_start_time, units = "secs")))

    # Return successful results list with the final, intersected data
    list(
      success = TRUE, message = "Recalculation successful, intersected results returned.", parameters = current_params,
      dataSummary = list(rawPrimesCount = nrow(raw_primes_final_df), splinePrimesCount = nrow(spline_primes_final_df), pqiCount = nrow(pqi_final_df), cqiCount = nrow(cqi_final_df), fullDataRows = nrow(full_data_final_df)),
      rawPrimes = raw_primes_final_df,
      splinePrimes = spline_primes_final_df,
      pqi = pqi_final_df,
      cqi = cqi_final_df,
      fullData = full_data_final_df
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
    res$status <- 500; results$success <- FALSE; results$message <- paste("Internal server error:", calculation_result$message)
    results$rawPrimes <- list(); results$splinePrimes <- list(); results$pqi <- list(); results$cqi <- list(); results$fullData = list()
    final_response_list <- results
  } else if (!is.list(calculation_result) || !("success" %in% names(calculation_result))) {
      res$status <- 500; results$success <- FALSE; results$message <- "Internal server error: Calculation returned unexpected structure."
      results$rawPrimes <- list(); results$splinePrimes <- list(); results$pqi <- list(); results$cqi <- list(); results$fullData = list()
      final_response_list <- results
  } else {
    final_response_list <- calculation_result; res$status <- 200 # OK
  }

  if (!is.list(final_response_list)) {
    message("CRITICAL ERROR: Final result object is not a list before JSON serialization."); res$status <- 500
    final_response_list <- list(success = FALSE, message = "Internal server error: Invalid response structure.", parameters = current_params, dataSummary = list(), rawPrimes = list(), splinePrimes = list(), pqi = list(), cqi = list(), fullData = list())
  }

  # --- Serialize to JSON ---
  response_body <- tryCatch({
    jsonlite::toJSON(final_response_list, auto_unbox = TRUE, na = "null", digits = NA, pretty = FALSE)
  }, error = function(e) {
    error_msg_json <- "Internal server error: Failed to serialize results to JSON."
    message("CRITICAL ERROR: ", error_msg_json, " JSON Error: ", conditionMessage(e)); res$status <- 500
    sprintf('{"success": false, "message": "%s", "parameters": %s, "dataSummary": {}, "rawPrimes": [], "splinePrimes": [], "pqi": [], "cqi": [], "fullData": []}',
            gsub('"', '\\\\"', error_msg_json), jsonlite::toJSON(current_params, auto_unbox = TRUE, na = "null") %||% '{}' )
  })

  # --- Final Logging and Return ---
  endpoint_duration <- difftime(Sys.time(), endpoint_start_time, units = "secs")
  message(sprintf("INFO: /recalculate completed. Status: %d. Duration: %.2f seconds", res$status %||% 500, endpoint_duration))
  res$setHeader("Content-Type", "application/json"); res$body <- response_body; return(res)
} # End /recalculate endpoint

#* API Status / Health Check
#* @get /
#* @serializer contentType list(type="application/json")
function(req, res){
  message("INFO: / (health check) endpoint invoked.")
  response_list <- list()
  if (exists("LOAD_ERROR", envir = .API_ENV)) {
    res$status <- 503; response_list <- list(status = "ERROR", message = paste("API running BUT critical base data failed to load. Startup Error:", conditionMessage(.API_ENV$LOAD_ERROR)), data_loaded = FALSE, timestamp = Sys.time())
    message("ERROR: Health Check Failed (Startup Data Load Error).")
  } else {
    data_check_passed <- tryCatch({ stopifnot(exists("SPORTS_FILTERED_ORIG", envir = .API_ENV) && inherits(.API_ENV$SPORTS_FILTERED_ORIG, "data.frame") && nrow(.API_ENV$SPORTS_FILTERED_ORIG) > 0, exists("PLAYER_TRAJECTORIES", envir = .API_ENV) && inherits(.API_ENV$PLAYER_TRAJECTORIES, "data.frame") && nrow(.API_ENV$PLAYER_TRAJECTORIES) > 0, exists("CLUSTER_DATA", envir = .API_ENV) && inherits(.API_ENV$CLUSTER_DATA, "data.frame") && nrow(.API_ENV$CLUSTER_DATA) > 0); TRUE }, error = function(e) { message("WARN: Health Check - Runtime data validation failed: ", conditionMessage(e)); FALSE })
    if (data_check_passed) {
      res$status <- 200; response_list <- list(status = "OK", message = "API is running and essential base data appears valid.", data_loaded = TRUE, timestamp = Sys.time(), endpoints = list(status = list(method = "GET", path = "/", description = "API health check."), recalculate = list(method = "POST", path = "/recalculate", description = "Recalculates metrics based on thresholds.")))
      message("INFO: Health Check OK.")
    } else {
      res$status <- 503; response_list <- list(status = "ERROR", message = "API running BUT essential base data is missing, invalid, or empty at runtime.", data_loaded = FALSE, timestamp = Sys.time())
      message("ERROR: Health Check Failed (Runtime Data Invalid/Missing).")
    }
  }
  res$setHeader("Content-Type", "application/json"); res$body <- jsonlite::toJSON(response_list, auto_unbox = TRUE, na = "null", pretty = FALSE)
  message("INFO: / (health check) completed."); return(res)
} # End / endpoint

# --- Plumber Entrypoint ---
#* @plumber
function(pr) {
  message("INFO: Plumber router object created.")
  pr # Return the router object
}
