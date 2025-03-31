#' Build NBA Player Data
#'
#' @description
#' Builds a dataset of NBA players with performance data from the hoopR package.
#' By default, uses pre-packaged data included with the package. When use_cached=FALSE,
#' fetches and processes data from APIs, which may take significant time.
#'
#' @param output_path Character string. Optional path to save output CSV file.
#' @param use_cached Logical. Whether to use cached data (TRUE) or rebuild from APIs (FALSE).
#' @param seasons Integer vector. Seasons to pull data for. Default is 2002:2024.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A data frame containing NBA player data if output_path is NULL.
#'         Otherwise, writes to the specified path and returns that path invisibly.
#'
#' @importFrom hoopR load_nba_player_box load_nba_team_box nba_teams nba_commonteamroster
#' @importFrom magrittr %>%
#' @importFrom stringr str_to_lower str_replace_all
#' @importFrom lubridate interval as.period year ymd mdy
#' @importFrom dplyr select mutate filter group_by summarize ungroup arrange left_join rename distinct sym case_when row_number n across everything n_distinct
#'
#' @examples
#' \dontrun{
#' # Get pre-packaged data
#' nba_data <- build_nba()
#'
#' # Rebuild data from APIs (takes time)
#' nba_data_fresh <- build_nba(use_cached = FALSE)
#'
#' # Rebuild for specific seasons
#' nba_data_recent <- build_nba(use_cached = FALSE, seasons = 2020:2023)
#'
#' # Save to specific location
#' build_nba(output_path = "path/to/save/directory")
#' }
#'
#' @export
build_nba <- function(output_path = NULL, use_cached = TRUE, seasons = 2002:2024, ...) {
  # First try package data if use_cached is TRUE
  if(use_cached) {
    nba_data <- tryCatch({
      # Try to load package data
      nba_tidy <- NULL
      data("nba_tidy", envir = environment())
      nba_tidy
    }, error = function(e) {
      # If loading package data fails, try to load from CSV
      csv_path <- system.file("extdata", "nba_final.csv", package = "yourpackage")
      if (csv_path == "") {
        warning("Could not find cached NBA data. Attempting to rebuild from APIs.")
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
    if (!is.null(nba_data)) {
      if (!is.null(output_path)) {
        # Create directory if it doesn't exist
        if (!dir.exists(output_path)) {
          dir.create(output_path, recursive = TRUE)
        }
        write.csv(nba_data, file.path(output_path, "nba_tidy.csv"), row.names = FALSE)
        message("NBA data saved to ", file.path(output_path, "nba_tidy.csv"))
        return(invisible(file.path(output_path, "nba_tidy.csv")))
      } else {
        return(nba_data)
      }
    }
  }

  # If we get here, either use_cached was FALSE or we couldn't load cached data
  message("Building NBA data from APIs. This may take significant time...")

  # Check for required package
  if (!requireNamespace("hoopR", quietly = TRUE)) {
    stop("Package 'hoopR' is required for building NBA data. Please install it with install.packages('hoopR').")
  }

  # Create data-raw directory if it doesn't exist
  if (!dir.exists("data-raw")) {
    dir.create("data-raw", recursive = TRUE)
  }

  # Helper function for slugifying names
  slugify <- function(name) {
    name %>%
      stringr::str_to_lower() %>%      # Convert to lowercase
      stringr::str_replace_all(" ", "-") %>%  # Replace spaces with hyphens
      stringr::str_replace_all("[^a-z0-9-]", "") # Remove non-alphanumeric characters except hyphens
  }

  # Check seasons ranges and possibly split into batches to avoid memory issues
  season_groups <- split(seasons, ceiling(seq_along(seasons)/6))

  message("Loading player box score data...")
  playerStats <- data.frame()

  # Load player box scores in batches
  for (group in season_groups) {
    message(paste("Loading seasons", min(group), "to", max(group)))
    temp_stats <- hoopR::load_nba_player_box(seasons = group)
    playerStats <- rbind(playerStats, temp_stats)
  }

  # Process positions
  playerStats <- playerStats %>%
    dplyr::mutate(
      position = dplyr::case_when(
        athlete_position_abbreviation %in% c("PF", "SF", "F", "GF") ~ "F",
        athlete_position_abbreviation %in% c("PG", "SG", "G") ~ "G",
        athlete_position_abbreviation %in% c("C") ~ "C",
        TRUE ~ athlete_position_abbreviation
      )
    )

  # Load team box scores
  message("Loading team box score data...")
  gameStats <- hoopR::load_nba_team_box(seasons = seasons)

  # Aggregate game stats
  combined_games <- gameStats %>%
    dplyr::group_by(game_id) %>%
    dplyr::summarize(
      game_pts = sum(team_score),
      game_fg_m = sum(field_goals_made),
      game_ft_m = sum(free_throws_made),
      game_fg_a = sum(field_goals_attempted),
      game_ft_a = sum(free_throws_attempted),
      game_def_reb = sum(defensive_rebounds),
      game_off_reb = sum(offensive_rebounds),
      game_assists = sum(assists),
      game_steals = sum(steals),
      game_blocks = sum(blocks),
      game_pf = sum(fouls),
      game_to = sum(turnovers)
    )

  # Get team IDs
  nba_team <- hoopR::nba_teams()
  teams <- nba_team %>% dplyr::select(team_name, team_id)

  # Join player stats with team IDs
  playerStats <- playerStats %>%
    dplyr::left_join(teams, by = "team_name") %>%
    dplyr::select(-team_id.x) %>%
    dplyr::rename(team_id = team_id.y)

  # Extract relevant box score data
  boxStats <- playerStats %>%
    dplyr::select(game_id, team_id, season, athlete_id, athlete_display_name, minutes,
                  field_goals_made, field_goals_attempted, three_point_field_goals_made,
                  three_point_field_goals_attempted, free_throws_made, free_throws_attempted,
                  offensive_rebounds, defensive_rebounds, rebounds, assists, steals, blocks,
                  turnovers, fouls, plus_minus, points, athlete_position_abbreviation, team_score,
                  team_winner, position) %>%
    dplyr::arrange(athlete_id) %>%
    dplyr::filter(!is.na(minutes))

  # Join box score data with game aggregates
  combined_stats <- boxStats %>%
    dplyr::left_join(combined_games, by = "game_id")

  # Remove any NA rows
  combined_stats_final <- stats::na.omit(combined_stats)

  # Calculate Player Impact Estimate (PIE)
  combined_stats <- combined_stats_final %>%
    dplyr::mutate(pie = ((points + field_goals_made + free_throws_made - field_goals_attempted - free_throws_attempted + defensive_rebounds +
                            (offensive_rebounds*0.5) + assists + steals + (blocks*0.5) - fouls - turnovers) / (game_pts + game_fg_m + game_ft_m - game_fg_a -
                                                                                                                 game_ft_a + game_def_reb + (game_off_reb * 0.5) +
                                                                                                                 game_assists + game_steals + (game_blocks*0.5) -
                                                                                                                 game_pf - game_to)))

  # Aggregate to season level
  season_stats <- combined_stats %>%
    dplyr::group_by(athlete_id, season) %>%
    dplyr::summarize(
      athlete_display_name = first(athlete_display_name),
      position = first(position),
      team_id = first(team_id),
      pie.season = sum(pie),
      pie.game = mean(pie),
      games_played = n_distinct(game_id),
      pie.season.f = pie.game * 82
    ) %>%
    dplyr::select(
      athlete_id, athlete_display_name, position, team_id, season, pie.season, pie.game, games_played, pie.season.f
    )

  # Scale PIE values within position groups and across league
  season.scaled <- season_stats %>%
    dplyr::group_by(season, position) %>%
    dplyr::mutate(
      pie.season.s.p = scale(pie.season),
      pie.game.s.p = scale(pie.game),
      pct_season_played = games_played / 82,
      pie.season.f.s.p = scale(pie.season.f)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(
      pie.season.s = scale(pie.season.s.p),
      pie.game.s = scale(pie.game.s.p),
      pct_season_played = games_played / 82,
      pie.season.f.s = scale(pie.season.f.s.p)
    ) %>%
    dplyr::arrange(athlete_id, season)

  # Fetch roster data to get birth dates
  message("Fetching roster data for birth dates...")

  # Either fetch live or use cached data
  # This is resource intensive, so we'll check for a cached file first
  if (file.exists("data-raw/nba_rosters.csv")) {
    message("Using cached roster data from data-raw/nba_rosters.csv")
    NBA_rosters <- read.csv("data-raw/nba_rosters.csv")
  } else {
    message("Fetching roster data from API (this will take time)...")
    # Get NBA team IDs
    nba_team_ids <- nba_team$team_id

    # Initialize empty data frame
    NBA_rosters <- data.frame()

    # Loop through seasons and teams
    for(season in seasons){
      for(team_id in nba_team_ids){
        tryCatch({
          NBAroster_data <- hoopR::nba_commonteamroster(
            league_id = "00",
            season = season,
            team_id = team_id
          )
          NBAroster <- NBAroster_data$CommonTeamRoster

          NBA_rosters <- dplyr::bind_rows(NBA_rosters, NBAroster)
          Sys.sleep(1)  # Avoid rate limiting
        }, error = function(e) {
          message(paste("Error fetching roster for team", team_id, "in season", season, ":", conditionMessage(e)))
        })
      }
    }

    # Save roster data for future use
    write.csv(NBA_rosters, "data-raw/nba_rosters.csv", row.names = FALSE)
  }

  # Process roster data
  NBA_rosters <- NBA_rosters %>%
    dplyr::rename(athlete_display_name = PLAYER, season = SEASON)

  NBA_rosters2 <- NBA_rosters %>%
    dplyr::select(season, athlete_display_name, BIRTH_DATE, PLAYER_ID) %>%
    stats::na.omit() %>%
    dplyr::arrange(athlete_display_name)

  NBA_rosters2$season <- as.integer(NBA_rosters2$season)

  # Create slug fields for joining
  season_data <- season.scaled %>%
    dplyr::mutate(slug = sapply(athlete_display_name, slugify))

  roster_data <- NBA_rosters2 %>%
    dplyr::mutate(slug = sapply(athlete_display_name, slugify))

  # Handle known problematic slugs with manual mapping
  known_duplicate_mapping <- dplyr::tribble(
    ~slug,             ~athlete_id,  ~PLAYER_ID,
    "mike-james",      1051,         2229,
    "mike-james",      2528096,      1628455,
    "gary-payton",     640,          56,
    "gary-payton",     3134903,      1627780,
    "glenn-robinson",  717,          299,
    "glenn-robinson",  2991039,      203922,
    "chris-johnson",   4400,         203187,
    "chris-johnson",   2325975,      202419,
    "marcus-williams", 3039,         200766,
    "marcus-williams", 3240,         201173
  )

  # Find and handle duplicate slugs
  season_dupe_slugs <- season_data %>%
    dplyr::group_by(slug) %>%
    dplyr::summarize(
      unique_athlete_ids = n_distinct(athlete_id),
      athlete_id_list = paste(unique(athlete_id), collapse = ", "),
      is_duplicate = unique_athlete_ids > 1,
      athlete_slug_list = paste(unique(slug), collapse = ", ")
    )

  dupe_slugs <- season_dupe_slugs %>%
    dplyr::filter(is_duplicate == TRUE) %>%
    dplyr::pull(slug)

  # Handle duplicates
  roster_data_dup <- roster_data %>%
    dplyr::filter(slug %in% dupe_slugs)

  roster_data <- roster_data %>%
    dplyr::filter(!(slug %in% dupe_slugs)) %>%
    dplyr::select(slug, PLAYER_ID, BIRTH_DATE) %>%
    dplyr::distinct(slug, PLAYER_ID, BIRTH_DATE) %>%
    dplyr::filter(!(PLAYER_ID == "203318"))  # Filter out problematic player ID

  # Join with birth dates
  season_data_birthday <- season_data %>%
    dplyr::left_join(roster_data, by = "slug") %>%
    dplyr::select(athlete_id, PLAYER_ID, BIRTH_DATE)

  season_data_birthday <- season_data_birthday %>%
    dplyr::mutate(birth_date = lubridate::mdy(BIRTH_DATE)) %>%
    dplyr::group_by(athlete_id) %>%
    dplyr::summarize(
      birth_date = first(birth_date)
    ) %>%
    stats::na.omit()

  # Create final dataset with ages
  NBA.final <- season.scaled %>%
    dplyr::left_join(season_data_birthday, by = "athlete_id") %>%
    stats::na.omit() %>%
    dplyr::mutate(
      # Convert season year to October 1st start date
      season_start = lubridate::ymd(paste0(season, "-10-01")),

      # Calculate age at season start using lubridate
      age = lubridate::interval(birth_date, season_start) %>%
        lubridate::as.period() %>%
        lubridate::year()
    )

  # Save final dataset
  write.csv(NBA.final, "data-raw/nba_final.csv", row.names = FALSE)

  # Prepare output
  if (!is.null(output_path)) {
    # Create directory if it doesn't exist
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE)
    }
    write.csv(NBA.final, file.path(output_path, "nba_tidy.csv"), row.names = FALSE)
    message("NBA data saved to ", file.path(output_path, "nba_tidy.csv"))
    return(invisible(file.path(output_path, "nba_tidy.csv")))
  } else {
    return(NBA.final)
  }
}
