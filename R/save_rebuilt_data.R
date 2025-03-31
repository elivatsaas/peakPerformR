#' Save Rebuilt Sports Data
#'
#' After rebuilding data using any of the build_* functions, this function
#' properly saves the generated CSV files within the package structure.
#' This ensures that the rebuilt data is available for future use without
#' needing to rebuild from APIs again.
#'
#' @param leagues Character vector of leagues to save. Options are:
#'   "chess", "mlb", "mls", "nba", "nhl", "nwsl", "pwhl", "wnba", "nfl".
#'   Use "all" to save all available datasets.
#' @param data_dir Character string. Directory where the rebuilt CSV files are located.
#'   Default is "data-raw" in the current working directory.
#' @param package_dir Character string. Base directory of the package where data
#'   should be saved. Default is the current working directory.
#'
#' @return Invisibly returns a list of file paths where data was saved.
#'
#' @examples
#' \dontrun{
#' # First rebuild data
#' build_nba(use_cached = FALSE)
#' build_mlb(use_cached = FALSE)
#' build_nfl(use_cached = FALSE)
#'
#' # Then save the rebuilt data
#' save_rebuilt_data(leagues = c("nba", "mlb", "nfl"))
#'
#' # Save all available rebuilt data
#' save_rebuilt_data(leagues = "all")
#' }
#'
#' @export
save_rebuilt_data <- function(leagues = "all",
                              data_dir = "data-raw",
                              package_dir = ".") {
  # Define all available leagues
  all_leagues <- c("chess", "mlb", "mls", "nba", "nhl", "nwsl", "pwhl", "wnba", "nfl")

  # If "all" is specified, use all available leagues
  if (any(leagues == "all")) {
    leagues <- all_leagues
  }

  # Validate leagues argument
  invalid_leagues <- setdiff(leagues, all_leagues)
  if (length(invalid_leagues) > 0) {
    stop("Invalid league(s): ", paste(invalid_leagues, collapse = ", "),
         "\nAvailable leagues: ", paste(all_leagues, collapse = ", "))
  }

  # Ensure extdata directory exists
  extdata_dir <- file.path(package_dir, "inst", "extdata")
  if (!dir.exists(extdata_dir)) {
    dir.create(extdata_dir, recursive = TRUE)
    message("Created directory: ", extdata_dir)
  }

  # Initialize results tracking
  saved_files <- list()

  # Process each league
  for (league in leagues) {
    message("Processing ", league, " data...")

    # Construct file paths
    source_path <- file.path(data_dir, paste0(league, "_final.csv"))
    target_path <- file.path(extdata_dir, paste0(league, "_final.csv"))

    # Check if source file exists
    if (!file.exists(source_path)) {
      warning("File not found: ", source_path, ". Has ", league, " data been rebuilt?")
      next
    }

    # Copy file to extdata
    tryCatch({
      file.copy(source_path, target_path, overwrite = TRUE)
      message("  - Saved ", league, " data to ", target_path)
      saved_files[[league]] <- target_path
    }, error = function(e) {
      warning("Error saving ", league, " data: ", e$message)
    })
  }

  # Handle helper datasets if they exist
  helper_files <- list(
    "nhl_birthdays" = "nhl_birthdays.csv",
    "pwhl_players_with_ages" = "pwhl_players_with_ages.csv",
    "batter_pitcher_ids" = "batter_pitcher_ids.csv"
  )

  for (helper_name in names(helper_files)) {
    helper_file <- helper_files[[helper_name]]
    source_path <- file.path(data_dir, helper_file)
    target_path <- file.path(extdata_dir, helper_file)

    if (file.exists(source_path)) {
      tryCatch({
        file.copy(source_path, target_path, overwrite = TRUE)
        message("  - Saved helper data ", helper_file, " to ", target_path)
        saved_files[[helper_name]] <- target_path
      }, error = function(e) {
        warning("Error saving helper data ", helper_file, ": ", e$message)
      })
    }
  }

  # Print summary
  total_saved <- length(saved_files)
  if (total_saved > 0) {
    message("\nSaving complete! Saved ", total_saved, " data file(s).")
    message("The data is now properly stored in your package.")
    message("It will be available through the package's data access functions.")
  } else {
    warning("No data files were saved. Make sure the data has been rebuilt first.")
  }

  # Return the list of saved files invisibly
  invisible(saved_files)
}
