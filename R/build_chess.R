#' Build Chess Player Data
#'
#' @description
#' Builds a dataset of chess players with performance data from Chess.com and Wikidata.
#' By default, uses pre-packaged data included with the package. When use_cached=FALSE,
#' fetches and processes data from APIs, which may take significant time.
#'
#' @param output_path Character string. Optional path to save output CSV file.
#' @param use_cached Logical. Whether to use cached data (TRUE) or rebuild from APIs (FALSE).
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A data frame containing chess player data if output_path is NULL.
#'         Otherwise, writes to the specified path and returns that path invisibly.
#'
#' @importFrom httr GET content status_code
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate filter select rename left_join inner_join group_by summarize ungroup arrange anti_join bind_rows
#' @importFrom purrr map map_df map_chr map_lgl map_dbl map_dfr map2
#' @importFrom stringr str_to_lower str_replace_all
#' @importFrom tidyr unnest
#' @importFrom lubridate time_length interval
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # Get pre-packaged data
#' chess_data <- build_chess()
#'
#' # Rebuild data from APIs (takes time)
#' chess_data_fresh <- build_chess(use_cached = FALSE)
#'
#' # Save to specific location
#' build_chess(output_path = "path/to/save/directory")
#' }
#'
#' @export
build_chess <- function(output_path = NULL, use_cached = TRUE, ...) {
  # First try package data if use_cached is TRUE
  if(use_cached) {
    chess_data <- tryCatch({
      # Try to load package data
      chess_tidy <- NULL
      data("chess_tidy", envir = environment())
      chess_tidy
    }, error = function(e) {
      # If loading package data fails, try to load from CSV
      csv_path <- system.file("extdata", "chess_final.csv", package = "yourpackage")
      if (csv_path == "") {
        warning("Could not find cached chess data. Attempting to rebuild from APIs.")
        return(NULL)
      }
      tryCatch({
        read.csv(csv_path)
      }, error = function(e2) {
        warning("Could not read CSV file. Attempting to rebuild from APIs.")
        return(NULL)
      })
    })

    # If we successfully loaded the data, either return it or save it
    if (!is.null(chess_data)) {
      if (!is.null(output_path)) {
        # Create directory if it doesn't exist
        if (!dir.exists(output_path)) {
          dir.create(output_path, recursive = TRUE)
        }
        write.csv(chess_data, file.path(output_path, "chess_tidy.csv"), row.names = FALSE)
        message("Chess data saved to ", file.path(output_path, "chess_tidy.csv"))
        return(invisible(file.path(output_path, "chess_tidy.csv")))
      } else {
        return(chess_data)
      }
    }
  }

  # If we get here, either use_cached was FALSE or we couldn't load cached data
  message("Building chess data from APIs. This may take significant time...")

  # Check required packages
  required_packages <- c("WikidataR", "countrycode", "progress")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    stop("The following packages are required but not installed: ",
         paste(missing_packages, collapse = ", "),
         ". Please install them with install.packages()")
  }

  # Helper function to get titled players
  get_titled_players <- function(title_abbrev) {
    url <- paste0("https://api.chess.com/pub/titled/", title_abbrev)
    response <- httr::GET(url)
    jsonlite::fromJSON(httr::content(response, "text"))$players
  }

  # Helper function to get player details
  get_player_details <- function(username, pb) {
    pb$tick()
    url <- paste0("https://api.chess.com/pub/player/", username)
    response <- httr::GET(url)
    details <- jsonlite::fromJSON(httr::content(response, "text"))

    data.frame(
      username = username,
      name = ifelse(!is.null(details$name), details$name, NA_character_),
      country = ifelse(!is.null(details$country),
                       basename(details$country),
                       NA_character_),
      stringsAsFactors = FALSE
    )
  }

  # Helper function to slugify names
  slugify <- function(name) {
    name %>%
      stringr::str_to_lower() %>%      # Convert to lowercase
      stringr::str_replace_all(" ", "-") %>%  # Replace spaces with hyphens
      stringr::str_replace_all("[^a-z0-9-]", "") # Remove non-alphanumeric characters except hyphens
  }

  # Create data-raw directory if it doesn't exist
  if (!dir.exists("data-raw")) {
    dir.create("data-raw", recursive = TRUE)
  }

  # Get all titles usernames
  titles <- c("GM", "WGM", "IM", "WIM", "FM", "WFM", "NM", "WNM", "CM", "WCM")

  # Initialize empty data frame with correct structure
  usernames <- data.frame(
    username = character(),
    title = character(),
    stringsAsFactors = FALSE
  )

  # Modified loop with proper data frame creation
  for(title in titles) {
    # Get usernames for this title
    players <- get_titled_players(title)

    # Create temporary data frame with consistent structure
    temp_df <- data.frame(
      username = players,
      title = rep(title, length(players)),
      stringsAsFactors = FALSE
    )

    # Bind rows properly
    usernames <- rbind(usernames, temp_df)
  }

  # Save titles to data-raw
  write.csv(usernames, "data-raw/chess_titles.csv", row.names = FALSE)

  # Initialize progress bar for player details
  pb_chess <- progress::progress_bar$new(
    format = "Fetching Chess.com data [:bar] :percent (:current/:total)",
    total = length(usernames$username)
  )

  # Get player details with rate limiting
  chess_players <- purrr::map_dfr(usernames$username, ~{
    player_data <- get_player_details(.x, pb_chess)
    Sys.sleep(0.5)
    player_data
  })

  write.csv(chess_players, "data-raw/chess_players.csv", row.names = FALSE)

  # Fetch all chess players from Wikidata using sparql_query
  fetch_wikidata_chess_players <- function() {
    query <- '
    SELECT ?person ?personLabel ?dob ?countryCode ?genderLabel WHERE {
    ?person wdt:P31 wd:Q5;              # Instance of human
          wdt:P106 wd:Q10873124;      # Occupation: chess player
          wdt:P569 ?dob;              # Date of birth
          wdt:P21 ?gender.            # Gender
            OPTIONAL { ?person wdt:P27 ?country. # Country of citizenship
             ?country wdt:P298 ?countryCode. } # ISO 3166-1 alpha-2 code
      SERVICE wikibase:label {
    bd:serviceParam wikibase:language "en".
    ?person rdfs:label ?personLabel.
    ?gender rdfs:label ?genderLabel.
    }
  }
  '

    # Use WikidataR's sparql_query function
    results <- WikidataR::sparql_query(query)

    # Convert results to a data frame
    wikidata_players <- results$results$bindings %>%
      purrr::map_dfr(~{
        data.frame(
          wikidata_name = ifelse(!is.null(.x$personLabel$value), .x$personLabel$value, NA_character_),
          dob = ifelse(!is.null(.x$dob$value), .x$dob$value, NA),
          gender = ifelse(!is.null(.x$gender$value), .x$gender$value, NA),
          country_code = ifelse(!is.null(.x$countryCode$value), .x$countryCode$value, NA_character_),
          stringsAsFactors = FALSE
        )
      })

    return(wikidata_players)
  }

  # Fetch all chess players from Wikidata
  message("Fetching players from Wikidata...")
  wikidata_players <- fetch_wikidata_chess_players()

  # Process chess players data
  chess_players <- read.csv("data-raw/chess_players.csv") %>%
    dplyr::mutate(slug = slugify(name))

  # Create mapping for historical country codes
  historical_country_codes <- data.frame(
    alpha3 = c("CSK", "DDR", "SCG", "SUN", "YUG"),
    alpha2 = c("CZ", "DE", "RS", "RU", "RS")  # Map to modern equivalents
  )

  # Convert country codes using countrycode
  wikidata_players <- wikidata_players %>%
    dplyr::mutate(
      slug = slugify(wikidata_name),
      country_code_alpha2 = countrycode::countrycode(country_code, origin = "iso3c", destination = "iso2c"),
      country_code_alpha2 = ifelse(is.na(country_code_alpha2),
                                   historical_country_codes$alpha2[match(country_code, historical_country_codes$alpha3)],
                                   country_code_alpha2)
    )

  # Identify duplicates in chess_players
  chess_duplicates <- chess_players %>%
    dplyr::group_by(slug, country) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()

  # Identify duplicates in wikidata_players
  wikidata_duplicates <- wikidata_players %>%
    dplyr::group_by(slug, country_code_alpha2) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()

  # Remove duplicates from chess_players
  chess_players_clean <- chess_players %>%
    dplyr::anti_join(chess_duplicates, by = c("slug", "country"))

  # Remove duplicates from wikidata_players
  wikidata_players_clean <- wikidata_players %>%
    dplyr::anti_join(wikidata_duplicates, by = c("slug", "country_code_alpha2"))


  matched_players <- chess_players_clean %>%
    dplyr::left_join(wikidata_players_clean, by = c("slug", "country" = "country_code_alpha2")) %>%
    dplyr::filter(!is.na(dob))  # Keep only matched players

  matched_players <- matched_players %>%
    dplyr::mutate(
      gender = dplyr::case_when(
        gender %in% ("male") ~ "M",
        TRUE ~ "F"
      )
    )

  # Save results
  write.csv(matched_players, "data-raw/matched_chess_players.csv", row.names = FALSE)

  # Add safety wrappers for API calls
  get_player_archives <- function(username) {
    tryCatch({
      response <- httr::GET(paste0("https://api.chess.com/pub/player/", username, "/games/archives"))
      if (httr::status_code(response) == 200) {
        jsonlite::fromJSON(httr::content(response, "text"))$archives
      } else NULL
    }, error = function(e) NULL)
  }

  get_monthly_games <- function(archive_url, username) {
    tryCatch({
      for (i in 1:3) {
        Sys.sleep(0.05)
        response <- httr::GET(archive_url)
        if (httr::status_code(response) == 200) {
          games <- jsonlite::fromJSON(httr::content(response, "text"))$games
          games$username <- username
          return(games)
        }
        Sys.sleep(2^i)
      }
      NULL
    }, error = function(e) NULL)
  }

  get_all_games <- function(username) {
    tryCatch({
      archives <- get_player_archives(username)
      if (is.null(archives)) return(NULL)
      purrr::map_dfr(archives, ~get_monthly_games(.x, username))
    }, error = function(e) NULL)
  }

  clean_data <- function(game_df) {
    if (nrow(game_df) == 0) {
      return(NULL)
    }
    required_cols <- c("rules", "url", "white", "black")
    if (!all(required_cols %in% colnames(game_df))) {
      return(NULL)
    }

    chess_games <- game_df %>%
      dplyr::filter(rules == "chess")

    if (nrow(chess_games) == 0) {
      return(NULL)
    }

    white_data <- chess_games %>%
      dplyr::select(url, white) %>%
      tidyr::unnest(white) %>%
      dplyr::mutate(color = "white")

    black_data <- chess_games %>%
      dplyr::select(url, black) %>%
      tidyr::unnest(black) %>%
      dplyr::mutate(color = "black")

    final <- dplyr::bind_rows(white_data, black_data)

    return(final)
  }

  get_time <- function(game_df) {
    required_cols <- c("end_time", "url")
    if (!all(required_cols %in% colnames(game_df))) {
      return(NULL)
    }
    end_time <- game_df %>% dplyr::select(url, end_time)
    return(end_time)
  }

  # Process in chunks to fetch games
  if (requireNamespace("furrr", quietly = TRUE)) {
    message("Fetching chess games (this will take time)...")
    usernames <- matched_players$username

    chunk_size <- 50
    groups <- split(usernames, ceiling(seq_along(usernames) / chunk_size))

    combined_data <- data.frame()

    for (i in seq_along(groups)) {
      # Calculate group range
      group_start <- 1 + ((i - 1) * chunk_size)
      group_end <- min(length(usernames), group_start + chunk_size - 1)

      message(paste("Processing players", group_start, "to", group_end))

      # Process group - FIXED to use furrr:: prefix
      all_games <- furrr::future_map(groups[[i]], ~{
        get_all_games(.x)
      }, .options = furrr::furrr_options(
        globals = c("get_player_archives", "get_monthly_games",
                    "get_all_games"),
        packages = c("httr", "jsonlite", "purrr")
      ))

      # Clean and save
      mapped <- purrr::map_dfr(all_games, clean_data) %>%
        dplyr::filter(!is.na(url))

      end_times <- purrr::map_dfr(all_games, get_time) %>%
        dplyr::filter(!is.na(url))

      end_times <- end_times %>%
        dplyr::distinct(url, .keep_all = TRUE)

      mapped <- mapped %>%
        dplyr::left_join(end_times, by = "url")

      # Write chunked data
      filename <- paste0("data-raw/all_chess_games_", group_start, "_", group_end, ".csv")
      write.csv(mapped, filename)

      combined_data <- dplyr::bind_rows(combined_data, mapped)

      # Clear memory
      rm(all_games, mapped, end_times)
      gc()
    }

    combined_data$game_date <- as.POSIXct(combined_data$end_time, origin = "1970-01-01")

    usernames <- tolower(matched_players$username)
    combined_data <- combined_data %>%
      dplyr::filter(username %in% usernames)

    write.csv(combined_data, "data-raw/all_chess_matches_filtered.csv", row.names = FALSE)

    # Process final data
    final_data <- combined_data %>%
      dplyr::mutate(username = tolower(username)) %>%
      dplyr::inner_join(matched_players %>% dplyr::select(username, name, dob, gender), by = "username")

    write.csv(final_data, "data-raw/all_chess_matches.csv", row.names = FALSE)

    # Final data processing
    final_data <- final_data %>%
      dplyr::mutate(
        age = floor(lubridate::time_length(lubridate::interval(dob, end_time), "years")),
        year = lubridate::year(game_date)
      )

    final_data_season <- final_data %>%
      dplyr::group_by(username, age) %>%
      dplyr::arrange(end_time) %>%
      dplyr::summarize(
        name = first(name),
        year = first(year),
        games_played = dplyr::n(),
        start_rating = first(rating),
        end_rating = last(rating),
        avg_rating = mean(rating),
        max_rating = max(rating),
        min_rating = min(rating),
        .groups = 'drop'
      )

    # Join with titles information
    titles <- read.csv("data-raw/chess_titles.csv")

    final_data_season <- final_data_season %>%
      dplyr::left_join(titles, by = "username")

    # Save final cleaned data
    write.csv(final_data_season, "data-raw/chess_final.csv", row.names = FALSE)

    # Prepare output
    if (!is.null(output_path)) {
      # Create directory if it doesn't exist
      if (!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE)
      }
      write.csv(final_data_season, file.path(output_path, "chess_tidy.csv"), row.names = FALSE)
      message("Chess data saved to ", file.path(output_path, "chess_tidy.csv"))
      return(invisible(file.path(output_path, "chess_tidy.csv")))
    } else {
      return(final_data_season)
    }
  } else {
    stop("Package 'furrr' is required for processing chess games. Please install it with install.packages('furrr').")
  }
}
