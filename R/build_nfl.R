#' Build NFL Player Data
#'
#' @description
#' Builds a dataset of NFL players with performance data from the nflreadr package.
#' By default, uses pre-packaged data included with the package. When use_cached=FALSE,
#' fetches and processes data from APIs, which may take significant time.
#'
#' @param output_path Character string. Optional path to save output CSV file.
#' @param use_cached Logical. Whether to use cached data (TRUE) or rebuild from APIs (FALSE).
#' @param seasons Integer vector. Seasons to pull data for. Default is 1999:2024.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A data frame containing NFL player data if output_path is NULL.
#'         Otherwise, writes to the specified path and returns that path invisibly.
#'
#' @importFrom nflreadr load_player_stats load_players load_snap_counts load_ff_playerids
#' @importFrom dplyr filter mutate desc select rename group_by summarize ungroup arrange left_join inner_join anti_join bind_rows distinct ntile sym case_when row_number n across everything n_distinct
#' @importFrom tidyr replace_na
#' @importFrom lubridate ymd interval as.period year
#' @importFrom stringr str_to_lower str_replace_all
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # Get pre-packaged data
#' nfl_data <- build_nfl()
#'
#' # Rebuild data from APIs (takes time)
#' nfl_data_fresh <- build_nfl(use_cached = FALSE)
#'
#' # Rebuild data for specific seasons
#' nfl_data_recent <- build_nfl(use_cached = FALSE, seasons = 2020:2024)
#'
#' # Save to specific location
#' build_nfl(output_path = "path/to/save/directory")
#' }
#'
#' @export
build_nfl <- function(output_path = NULL, use_cached = TRUE, seasons = 1999:2024, ...) {
  # First try package data if use_cached is TRUE
  if(use_cached) {
    nfl_data <- tryCatch({
      # Try to load package data
      nfl_tidy <- NULL
      data("nfl_tidy", envir = environment())
      nfl_tidy
    }, error = function(e) {
      # If loading package data fails, try to load from CSV
      csv_path <- system.file("extdata", "nfl_final.csv", package = "yourpackage")
      if (csv_path == "") {
        warning("Could not find cached NFL data. Attempting to rebuild from APIs.")
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
    if (!is.null(nfl_data)) {
      if (!is.null(output_path)) {
        # Create directory if it doesn't exist
        if (!dir.exists(output_path)) {
          dir.create(output_path, recursive = TRUE)
        }
        write.csv(nfl_data, file.path(output_path, "nfl_tidy.csv"), row.names = FALSE)
        message("NFL data saved to ", file.path(output_path, "nfl_tidy.csv"))
        return(invisible(file.path(output_path, "nfl_tidy.csv")))
      } else {
        return(nfl_data)
      }
    }
  }

  # If we get here, either use_cached was FALSE or we couldn't load cached data
  message("Building NFL data from APIs. This may take significant time...")

  # Check for required package
  if (!requireNamespace("nflreadr", quietly = TRUE)) {
    stop("Package 'nflreadr' is required for building NFL data. Please install it with install.packages('nflreadr').")
  }

  # Create data-raw directory if it doesn't exist
  if (!dir.exists("data-raw")) {
    dir.create("data-raw", recursive = TRUE)
  }

  # Helper function to slugify names
  slugify <- function(name) {
    name %>%
      stringr::str_to_lower() %>%
      stringr::str_replace_all(" ", "-") %>%
      stringr::str_replace_all("[^a-z0-9-]", "")
  }

  # Load NFL data
  message("Loading offensive player stats...")
  weekly_off <- nflreadr::load_player_stats(seasons = seasons, stat_type = "offense") %>%
    dplyr::filter(!is.na(player_id)) %>%
    dplyr::mutate(team = recent_team) %>%
    dplyr::select(-fantasy_points) %>%
    dplyr::rename(
      fantasy_points = fantasy_points_ppr
    )

  message("Loading defensive player stats...")
  weekly_def <- nflreadr::load_player_stats(seasons = seasons, stat_type = "defense") %>%
    dplyr::filter(!is.na(player_id))

  # Calculate defense fantasy points
  weekly_def <- weekly_def %>%
    dplyr::mutate(
      fantasy_points =
        (def_tds * 6) +                   # Touchdowns
        (def_sacks * 4) +                 # Sacks
        (def_tackles_for_loss * 3) +      # TFL
        (def_pass_defended * 2) +         # PD
        (def_interceptions * 6) +         # INTs
        (def_safety * 10) +               # Safeties
        (def_fumble_recovery_opp * 2) +   # Fumble recoveries
        (def_fumbles_forced * 4) +        # Forced fumbles
        (def_tackles_solo * 1) +          # Solo tackles
        (def_tackle_assists * 0.5)        # Assisted tackles
    )

  message("Loading kicker stats...")
  weekly_st <- nflreadr::load_player_stats(seasons = seasons, stat_type = "kicking") %>%
    dplyr::filter(!is.na(player_id) & position == "K")

  # Function to calculate kicker fantasy points
  calculate_kicker_points <- function(weekly_st_data,
                                      fg_0_19_points = 3,
                                      fg_20_29_points = 3,
                                      fg_30_39_points = 3,
                                      fg_40_49_points = 4,
                                      fg_50_59_points = 5,
                                      fg_60_plus_points = 6,
                                      pat_made_points = 1,
                                      pat_missed_points = -4,
                                      fg_missed_distance_0_19 = -4,
                                      fg_missed_distance_20_29 = -3,
                                      fg_missed_distance_30_39 = -3,
                                      fg_missed_distance_40_49 = -2,
                                      fg_missed_distance_50_59 = -1,
                                      fg_blocked_points = -1,
                                      gwfg_made = 3,
                                      gwfg_missed = 2) {

    weekly_st_data %>%
      dplyr::mutate(
        fantasy_points =
          # Made Field Goals
          (fg_made_0_19 * fg_0_19_points) +
          (fg_made_20_29 * fg_20_29_points) +
          (fg_made_30_39 * fg_30_39_points) +
          (fg_made_40_49 * fg_40_49_points) +
          (fg_made_50_59 * fg_50_59_points) +
          (fg_made_60_ * fg_60_plus_points) +

          # Extra Points
          (pat_made * pat_made_points) +
          (pat_missed * pat_missed_points) +
          (pat_blocked * pat_missed_points) +  # Using same penalty as missed PATs

          # Missed Field Goals
          (fg_missed_0_19 * fg_missed_distance_0_19) +
          (fg_missed_20_29 * fg_missed_distance_20_29) +
          (fg_missed_30_39 * fg_missed_distance_30_39) +
          (fg_missed_40_49 * fg_missed_distance_40_49) +
          (fg_missed_50_59 * fg_missed_distance_50_59) +
          (fg_missed_60_ * fg_missed_distance_50_59) +  # Using 50-59 penalty for 60+ misses
          (fg_blocked * fg_blocked_points) +

          # Game-Winning FG Bonuses
          (gwfg_made * gwfg_made) +
          (gwfg_missed * gwfg_missed) +
          (gwfg_blocked * fg_blocked_points)  # Using same penalty as regular blocked FGs
      ) %>%
      dplyr::arrange(dplyr::desc(fantasy_points))
  }

  # Calculate kicker fantasy points
  weekly_st <- weekly_st %>%
    calculate_kicker_points()

  message("Combining player stats...")
  # Combine all data
  weekly_combined <- dplyr::bind_rows(
    weekly_off,
    weekly_def,
    weekly_st
  ) %>%
    dplyr::filter(!is.na(position))

  message("Calculating season totals...")
  # Calculate season totals
  season_totals <- weekly_combined %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(
      team = first(team),
      fantasy_points_season = sum(fantasy_points, na.rm = TRUE),
      games_played = dplyr::n_distinct(week),
      position = max(position),
      position_group = dplyr::case_when(
        position %in% c("QB") ~ "Passer",
        position %in% c("RB", "FB", "HB") ~ "Rusher",
        position %in% c("WR", "TE") ~ "Receiver",
        position %in% c("G", "T","C","OG","OL","OT") ~ "OL",
        position %in% c("OLB","DE","DT","DL", "NT") ~ "DL",
        position %in% c("ILB","MLB","LB") ~ "LB",
        position %in% c("CB","DB","FS","SS","SAF","S") ~ "Secondary",
        position %in% c("P","K","LS") ~ "SPEC",
        TRUE ~ position
      ),
      #Passing
      passing_yards = sum(passing_yards, na.rm = TRUE),
      passing_yards_after_catch = sum(passing_yards_after_catch, na.rm = TRUE),
      passing_tds = sum(passing_tds, na.rm = TRUE),
      interceptions = sum(interceptions, na.rm = TRUE),
      dakota = mean(dakota, na.rm = TRUE),
      passing_epa = mean(passing_epa, na.rm = TRUE),

      # Rushing stats
      rushing_yards = sum(rushing_yards, na.rm = TRUE),
      rushing_first_downs = sum(rushing_first_downs, na.rm = TRUE),
      rushing_tds = sum(rushing_tds, na.rm = TRUE),
      rushing_epa = mean(rushing_epa, na.rm = TRUE),

      # Receiving stats
      receiving_yards = sum(receiving_yards, na.rm = TRUE),
      receiving_tds = sum(receiving_tds, na.rm = TRUE),
      targets = sum(targets, na.rm = TRUE),
      target_share = mean(target_share, na.rm = TRUE),
      racr = mean(racr, na.rm = TRUE),
      receptions = sum(receptions, na.rm = TRUE),
      receiving_epa = mean(receiving_epa, na.rm = TRUE),

      # Defensive stats
      def_tackles = sum(def_tackles, na.rm = TRUE),
      def_tackles_for_loss = sum(def_tackles_for_loss, na.rm = TRUE),
      def_fumbles_forced = sum(def_fumbles_forced, na.rm = TRUE),
      def_sacks = sum(def_sacks, na.rm = TRUE),
      def_penalty = sum(def_penalty, na.rm = TRUE),
      def_qb_hits = sum(def_qb_hits, na.rm = TRUE),
      def_interceptions = sum(def_interceptions, na.rm = TRUE),
      def_pass_defended = sum(def_pass_defended, na.rm = TRUE),
      def_tds = sum(def_tds, na.rm = TRUE),

      # Kicking stats
      fg_made_0_19 = sum(fg_made_0_19, na.rm = TRUE),
      fg_made_20_29 = sum(fg_made_20_29, na.rm = TRUE),
      fg_made_30_39 = sum(fg_made_30_39, na.rm = TRUE),
      fg_made_40_49 = sum(fg_made_40_49, na.rm = TRUE),
      fg_made_50_59 = sum(fg_made_50_59, na.rm = TRUE),
      fg_made_60_ = sum(fg_made_60_, na.rm = TRUE),
      fg_missed_0_19 = sum(fg_missed_0_19, na.rm = TRUE),
      fg_missed_20_29 = sum(fg_missed_20_29, na.rm = TRUE),
      fg_missed_30_39 = sum(fg_missed_30_39, na.rm = TRUE),
      fg_missed_40_49 = sum(fg_missed_40_49, na.rm = TRUE),
      fg_missed_50_59 = sum(fg_missed_50_59, na.rm = TRUE),
      fg_missed_60_ = sum(fg_missed_60_, na.rm = TRUE),

      gwfg_att = sum(gwfg_att, na.rm = TRUE),
      gwfg_distance = sum(gwfg_distance, na.rm = TRUE),
      gwfg_made = sum(gwfg_made, na.rm = TRUE),
      gwfg_missed = sum(gwfg_missed, na.rm = TRUE),

      pat_made = sum(pat_made, na.rm = TRUE),
      pat_att = sum(pat_att, na.rm = TRUE),
      .groups = "drop"
    )

  # Replace NAs with 0s
  season_totals <- season_totals %>%
    dplyr::mutate(dplyr::across(everything(), ~tidyr::replace_na(., 0)))

  # Calculate career metrics
  season_totals <- season_totals %>%
    dplyr::arrange(player_id, season) %>%
    dplyr::group_by(player_id) %>%
    dplyr::mutate(
      career_games = cumsum(games_played),
      career_seasons = dplyr::row_number()
    ) %>%
    dplyr::ungroup()

  # Calculate position value
  position_value <- season_totals %>%
    dplyr::mutate(
      position_value = dplyr::case_when(
        position_group == "Passer" ~
          dplyr::ntile(passing_yards, 100) * 0.3 +
          dplyr::ntile(passing_tds, 100) * 0.25 +
          dplyr::ntile(passing_epa, 100) * 0.2 +
          (101 - dplyr::ntile(interceptions, 100)) * 0.25,

        position_group == "Rusher" ~
          dplyr::ntile(rushing_yards, 100) * 0.25 +
          dplyr::ntile(rushing_tds, 100) * 0.25 +
          dplyr::ntile(rushing_epa, 100) * 0.3 +
          dplyr::ntile(rushing_first_downs, 100) * 0.2,

        position_group == "Receiver" ~
          dplyr::ntile(receiving_yards, 100) * 0.25 +
          dplyr::ntile(receiving_tds, 100) * 0.2 +
          dplyr::ntile(targets, 100) * 0.2 +
          dplyr::ntile(racr, 100) * 0.15 +
          dplyr::ntile(receiving_epa, 100) * 0.2,

        position_group == "DL" ~
          dplyr::ntile(def_sacks, 100) * 0.4 +
          dplyr::ntile(def_tackles_for_loss, 100) * 0.3 +
          dplyr::ntile(def_qb_hits, 100) * 0.2 +
          dplyr::ntile(def_fumbles_forced, 100) * 0.1,

        position_group == "LB" ~
          dplyr::ntile(def_tackles, 100) * 0.4 +
          dplyr::ntile(def_sacks, 100) * 0.3 +
          dplyr::ntile(def_pass_defended, 100) * 0.2 +
          dplyr::ntile(def_interceptions, 100) * 0.1,

        position_group == "Secondary" ~
          dplyr::ntile(def_interceptions, 100) * 0.5 +
          dplyr::ntile(def_pass_defended, 100) * 0.3 +
          dplyr::ntile(def_tackles, 100) * 0.2,

        position_group == "SPEC" ~
          (dplyr::ntile(fg_made_40_49, 100) * 0.3) +
          (dplyr::ntile(fg_made_50_59, 100) * 0.4) +
          (dplyr::ntile(fg_made_60_, 100) * 0.2) +
          (dplyr::ntile(ifelse(pat_att > 0, pat_made/pat_att, 0), 100) * 0.1),

        position_group == "OL" ~
          dplyr::ntile(games_played + 0.1, 100) * 0.7 +
          dplyr::ntile(career_games + 0.1, 100) * 0.3,

        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(position_group) %>%
    dplyr::mutate(
      position_value =
        round(100 * (position_value - min(position_value, na.rm = TRUE)) /
                (max(position_value, na.rm = TRUE) - min(position_value, na.rm = TRUE)), 1)
    )

  # Calculate fantasy stats
  season_totals <- position_value %>%
    dplyr::mutate(
      # Calculate value per game by dividing season value by games played
      fpts.game = fantasy_points_season / games_played,
      # Calculate season value if they played every game of the season
      fpts.season = fantasy_points_season,
      # Calculate how much of the season they played
      pct.season.played = ifelse(season < 2021, games_played / 16, games_played / 17)
    )

  message("Standardizing stats by position and season...")
  # Scale value stat by position and season
  standardized_season_stats <- season_totals %>%
    # Group by position and season
    dplyr::group_by(position, season) %>%
    dplyr::mutate(
      #standardization (.s for standardized value)
      fpts.season.s.p = scale(fpts.season),
    ) %>%
    dplyr::ungroup()

  message("Loading player data for ages...")
  # Load players to get age
  players <- nflreadr::load_players()

  # Join birth dates to standardized stats
  standardized_season_stats <- standardized_season_stats %>%
    dplyr::left_join(
      # get player birthday, join by player_id
      players %>%
        dplyr::select(gsis_id, display_name, birth_date),
      by = c("player_id" = "gsis_id")
    ) %>%
    dplyr::mutate(
      # Create cutoff date for age at the start of each season (first game generally)
      age_cutoff = lubridate::ymd(paste0(season, "-09-10")),

      # Convert birth_date to Date type
      birth_date = lubridate::ymd(birth_date),
      display_name = display_name,

      # Calculate age
      age = lubridate::interval(birth_date, age_cutoff) %>%
        lubridate::as.period("years") %>%
        lubridate::year()
    ) %>%
    # Drop unnecessary columns
    dplyr::select(-age_cutoff)

  # Create nfl_final with necessary columns and filters
  nfl_final <- standardized_season_stats %>%
    dplyr::filter(!is.na(age) & !is.na(position)) %>%
    dplyr::mutate(
      position_value_2 = dplyr::case_when(
        position %in% c("QB") ~ passing_epa / games_played,
        position %in% c("RB", "FB", "HB") ~ rushing_epa / games_played,
        position %in% c("WR", "TE") ~ receiving_epa / games_played,
        position %in% c("OLB","DE","DT","DL", "NT") ~
          dplyr::ntile(def_tackles_for_loss + def_sacks + def_qb_hits, 100) * 0.6 +
          dplyr::ntile(def_tackles, 100) * 0.5 -
          dplyr::ntile(def_penalty, 100) * 0.1,
        position %in% c("ILB","MLB","LB") ~
          dplyr::ntile(def_tackles_for_loss + def_sacks + def_qb_hits, 100) * 0.2 +
          dplyr::ntile(def_tackles, 100) * 0.6 +
          dplyr::ntile(def_interceptions + def_pass_defended, 100) * 0.3 -
          dplyr::ntile(def_penalty, 100) * 0.1,
        position %in% c("CB","DB","FS","SS","SAF","S") ~
          dplyr::ntile(def_tackles_for_loss + def_sacks + def_qb_hits, 100) * 0.15 +
          dplyr::ntile(def_tackles, 100) * 0.35 +
          dplyr::ntile(def_interceptions + def_pass_defended, 100) * 0.5 +
          dplyr::ntile(def_tds, 100) * 0.1 -
          dplyr::ntile(def_penalty, 100) * 0.1,
        position %in% c("K") ~
          (dplyr::ntile(fg_made_40_49 / (fg_made_40_49 + fg_missed_40_49 + 1e-8), 100) * 0.3) +
          (dplyr::ntile(fg_made_50_59 / (fg_made_50_59 + fg_missed_50_59 + 1e-8), 100) * 0.3) +
          (dplyr::ntile(fg_made_60_, 100) * 0.1) +
          (dplyr::ntile(pat_made/pat_att, 100) * 0.1) +
          (dplyr::ntile(gwfg_made/(gwfg_att + 1e-8) * (101 - dplyr::ntile(gwfg_distance, 100)), 100) * 0.2),
        TRUE ~ NA_real_
      ),
    ) %>%
    dplyr::group_by(position_group) %>%
    dplyr::mutate(
      position_value_2 = scale(position_value_2)
    ) %>%
    dplyr::ungroup()

  message("Loading snap count data...")
  # Load snap count data
  snap_count_seasons <- intersect(2012:2024, seasons)
  if (length(snap_count_seasons) > 0) {
    snaps <- nflreadr::load_snap_counts(snap_count_seasons) %>%
      dplyr::rename(
        pfr_id = pfr_player_id
      ) %>%
      dplyr::group_by(pfr_id, season) %>%
      dplyr::summarise(
        player = first(player),
        team = first(team),
        total_snaps = sum(offense_snaps + defense_snaps + st_snaps, na.rm = TRUE),
        avg_snap_pct = mean(mean(offense_pct, na.rm = TRUE) + mean(defense_pct, na.rm = TRUE) + mean(st_pct, na.rm = TRUE), na.rm = TRUE),
        .groups = "drop"
      )

    # Load player IDs
    confirmed.ids <- nflreadr::load_ff_playerids() %>%
      dplyr::select(gsis_id, pfr_id)

    # Match IDs from confirmed sources
    matched.ids.1 <- snaps %>%
      dplyr::inner_join(confirmed.ids, by = "pfr_id") %>%
      dplyr::select(pfr_id, gsis_id) %>%
      dplyr::rename(
        player_id = gsis_id
      )

    # Try to match the rest by name
    unmatched.ids.1 <- snaps %>%
      dplyr::anti_join(confirmed.ids, by = "pfr_id") %>%
      dplyr::distinct(pfr_id, player) %>%
      dplyr::mutate(
        slug = slugify(player)
      ) %>%
      dplyr::select(pfr_id, slug)

    ids <- nfl_final %>%
      dplyr::filter(!player_id %in% matched.ids.1$player_id) %>%
      dplyr::distinct(player_id, display_name) %>%
      dplyr::mutate(slug = slugify(display_name)) %>%
      dplyr::select(player_id, slug)

    # Remove duplicate slugs
    dup_slugs <- ids %>% dplyr::group_by(slug) %>% dplyr::filter(dplyr::n() > 1) %>% dplyr::select(slug)
    dup_slugs.1 <- unmatched.ids.1 %>% dplyr::group_by(slug) %>% dplyr::filter(dplyr::n() > 1) %>% dplyr::select(slug)
    dup_slugs <- dplyr::bind_rows(dup_slugs, dup_slugs.1)

    unmatched.ids.1 <- unmatched.ids.1 %>%
      dplyr::filter(!slug %in% dup_slugs$slug)

    ids <- ids %>%
      dplyr::filter(!slug %in% dup_slugs$slug)

    # Match by slugs
    matched.ids.2 <- unmatched.ids.1 %>%
      dplyr::inner_join(ids, by = "slug") %>%
      dplyr::select(pfr_id, player_id)

    # Combine all matched IDs
    matched.ids <- dplyr::bind_rows(matched.ids.1, matched.ids.2) %>%
      dplyr::distinct(pfr_id, player_id)

    # Join snaps with matched IDs
    snaps <- snaps %>%
      dplyr::inner_join(matched.ids, by = "pfr_id") %>%
      dplyr::select(season, player_id, total_snaps, avg_snap_pct)

    # Join snaps with player data
    nfl_final <- nfl_final %>%
      dplyr::left_join(snaps, by = c("player_id", "season"))
  }

  # Save the data
  write.csv(nfl_final, "data-raw/nfl_final.csv", row.names = FALSE)

  # Also save to inst/extdata if it exists
  if (dir.exists("inst/extdata")) {
    write.csv(nfl_final, "inst/extdata/nfl_final.csv", row.names = FALSE)
  }

  # If output_path is specified, save there as well
  if (!is.null(output_path)) {
    # Create directory if it doesn't exist
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE)
    }
    write.csv(nfl_final, file.path(output_path, "nfl_tidy.csv"), row.names = FALSE)
    message("NFL data saved to ", file.path(output_path, "nfl_tidy.csv"))
    return(invisible(file.path(output_path, "nfl_tidy.csv")))
  } else {
    return(nfl_final)
  }
}
