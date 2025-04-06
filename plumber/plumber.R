# plumber.R - Production Ready API Endpoint with CORS
# v2023-04-07 - Added CORS filter for frontend interaction

# --- Load Required Libraries ---
# Ensure these are installed in the deployment environment via renv
suppressPackageStartupMessages({
  library(plumber)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(peakPerformR) # Your custom package
})
message("Required packages loaded. Custom package 'peakPerformR' loaded.")

# --- Utility Functions ---
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
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
#* Allows web applications (like your React app) hosted on specified domains
#* to interact with this API.
#* @filter cors
function(req, res) {
  # Get allowed origin from Environment Variable (BEST PRACTICE FOR PROD)
  # Set this variable in your Azure Container App configuration.
  # Default to localhost:3000 for local development if variable not set.
  allowed_origin <- Sys.getenv("ALLOWED_ORIGIN", "http://localhost:3000")

  # Set the Access-Control-Allow-Origin header for all responses
  res$setHeader("Access-Control-Allow-Origin", allowed_origin)

  # Handle preflight (OPTIONS) requests, often sent by browsers before POST/PUT etc.
  if (req$REQUEST_METHOD == "OPTIONS") {
    # Specify allowed methods and headers for cross-origin requests
    res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS") # Adjust if you add PUT/DELETE etc.
    res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization") # Add 'Authorization' if you use tokens
    # Optional: Allow credentials (like cookies, auth headers). Only use if origin != "*"
    # res$setHeader("Access-Control-Allow-Credentials", "true")
    # Optional: Define how long preflight response can be cached (in seconds)
    # res$setHeader("Access-Control-Max-Age", "86400") # 24 hours

    res$status <- 200 # OK status for preflight
    # Return empty body immediately for OPTIONS - DO NOT FORWARD
    return(list())
  } else {
    # For actual requests (GET, POST, etc.), forward to the next handler/endpoint
    plumber::forward()
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
  # Attempt to get status from response object, default to 200 if not set yet
  status_code <- tryCatch(req$pr$res$status, error = function(e) NULL) %||% 200
  cat(format(end_time), "-", req$REQUEST_METHOD, req$PATH_INFO, "- Status:", status_code, "- Duration:", duration, "s\n")
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
    err_msg <- paste("Service Unavailable: Critical base data failed to load during API startup. Check server logs. Original error:",
                     conditionMessage(.API_ENV$LOAD_ERROR))
    message("API Error: /recalculate cannot proceed due to data load failure.")
    # Use jsonlite::toJSON for consistency in error responses too
    res$body <- jsonlite::toJSON(list(success = FALSE, message = err_msg, parameters = list()), auto_unbox = TRUE, na = "null")
    return(res)
  }

  # --- (Rest of your /recalculate endpoint logic remains exactly the same) ---
  # ...
  # ... (calculation steps 1-5) ...
  # ...
  # --- Prepare Final Response ---
  # ... (including error handling and jsonlite::toJSON) ...
  # ...
  message(sprintf("API: /recalculate completed. Status: %d. Total duration: %.2f seconds",
                  res$status %||% 200, # Use default if status somehow not set
                  difftime(Sys.time(), endpoint_start_time, units = "secs")))
  res$body <- response_body
  return(res)

} # End /recalculate endpoint


#* API Status / Health Check
#* @get /
#* @serializer contentType list(type="application/json") # Manual serialization
function(req, res){
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
} # End / endpoint

# --- Plumber Entrypoint (If running directly, not via app.R/Shiny) ---
# This section is typically used when the Dockerfile directly runs this script.
# It might be commented out if using a different entrypoint mechanism.
#* @plumber
function(pr) {
  pr # Return the plumber router object
}

# --- Note on app.R ---
# The app.R file provided previously wraps this Plumber API in a Shiny app.
# For deployment to Azure Container Apps using Docker, you typically DON'T need app.R.
# Your Dockerfile should directly run the Plumber API using a command like:
# CMD ["R", "-e", "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8000)"]
# Running via Shiny (app.R) is common for shinyapps.io, not typically needed for ACA.
