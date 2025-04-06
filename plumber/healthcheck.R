# healthcheck.R
library(httr)
library(jsonlite)

# Base URL - change this to your eventual API URL
base_url <- "http://localhost:8080"  # For local testing

# Test health endpoint
health_response <- httr::GET(paste0(base_url, "/"))
cat("Health check status:", http_status(health_response)$message, "\n")
cat("Response:", content(health_response, "text"), "\n\n")

# Test recalculate endpoint with minimal data
test_body <- list(thresholdPct = 70, gamesPctThreshold = 100)
recalc_response <- httr::POST(
  paste0(base_url, "/recalculate"),
  body = jsonlite::toJSON(test_body, auto_unbox = TRUE),
  encode = "json",
  httr::content_type_json()
)

cat("Recalculate status:", http_status(recalc_response)$message, "\n")
