# Raw data processing script for Sports Analytics Package
# This script:
# 1) Reads in the raw .csv data files
# 2) Performs necessary data cleaning and processing
# 3) Saves the processed datasets to the package's data/ directory

# Load required packages
if (!requireNamespace("usethis", quietly = TRUE)) install.packages("usethis")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("fs", quietly = TRUE)) install.packages("fs")

# We'll load magrittr for the pipe operator
if (!requireNamespace("magrittr", quietly = TRUE)) install.packages("magrittr")

# Create inst/extdata directory if needed (for fallback CSV files)
if (!fs::dir_exists("inst/extdata")) {
  fs::dir_create("inst/extdata", recurse = TRUE)
}

# Process league datasets
message("Processing raw league datasets...")

# Define list of leagues
leagues <- c("chess", "mlb", "mls", "nba", "nhl", "nwsl", "pwhl", "wnba", "nfl", "all_sports")

for (league in leagues) {
  csv_path <- fs::path("data-raw", paste0(league, "_final.csv"))

  if (fs::file_exists(csv_path)) {
    message(paste("Processing", league, "data..."))

    # 1. Read the raw CSV data
    league_data <- readr::read_csv(csv_path, show_col_types = FALSE)

    # 2. Clean and process the data

    # General data cleaning
    league_data <- league_data |>
      # Remove any duplicate rows
      dplyr::distinct() |>
      # Convert any character NA values to real NAs
      dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

    # League-specific processing
    if (league == "chess") {
      # Ensure character columns remain as character (not factors)
      league_data <- league_data |>
        dplyr::mutate(dplyr::across(dplyr::where(is.character), as.character))
    }

    # 3. Save as R data object in the data/ directory
    # Use the standardized naming convention: {league}_tidy
    var_name <- paste0(league, "_tidy")
    assign(var_name, league_data)
    eval(parse(text = paste0("usethis::use_data(", var_name, ", overwrite = TRUE)")))

    # Also copy the CSV to inst/extdata for fallback access
    fs::file_copy(csv_path, fs::path("inst/extdata", paste0(league, "_final.csv")), overwrite = TRUE)


    message(paste(" -", var_name, "processed and saved"))
  } else {
    message(paste("Skipping", league, "- raw file not found"))
  }
}

# Process helper datasets
helper_datasets <- list(
  nhl_birthdays = "nhl_birthdays.csv",
  pwhl_players_with_ages = "pwhl_players_with_ages.csv",
  batter_pitcher_ids = "batter_pitcher_ids.csv"
)

for (dataset_name in names(helper_datasets)) {
  file_path <- fs::path("data-raw", helper_datasets[[dataset_name]])

  if (fs::file_exists(file_path)) {
    message(paste("Processing", dataset_name, "..."))

    # 1. Read the raw CSV data
    data_obj <- readr::read_csv(file_path, show_col_types = FALSE)

    # 2. Clean and process the data
    data_obj <- data_obj |>
      dplyr::distinct() |>
      dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

    # 3. Save as R data object
    assign(dataset_name, data_obj)
    eval(parse(text = paste0("usethis::use_data(", dataset_name, ", overwrite = TRUE)")))

    message(paste(" -", dataset_name, "processed and saved"))
  }
}

message("\nData processing complete!")
message("Processed datasets have been saved to the data/ directory.")
message("The documentation for these datasets should be in the R/ directory.")
