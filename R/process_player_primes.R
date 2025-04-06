#' Process Player Prime Seasons for PQI Calculation
#'
#' @description
#' Aggregates season-level player data to player-level summaries with prime-related metrics
#' needed for Prime Quality Index (PQI) calculation.
#'
#' @param final_dataset A data frame containing player performance data with prime flags.
#'        Must include 'id', 'player_name', 'sport', 'league', 'position', 'season',
#'        'age', 'games_played', 'in_prime', 'scaled_value', 'tier_score',
#'        and 'performance_tier' columns.
#' @param season_data Original season-level data for calculating standard deviations.
#'        Must include 'id', 'season', 'age', 'scaled_value', and 'in_prime' columns.
#'
#' @return A data frame with one row per player containing prime-related metrics:
#'   \item{id}{Player identifier}
#'   \item{player_name}{Player name}
#'   \item{sport}{Sport}
#'   \item{league}{League}
#'   \item{position}{Position}
#'   \item{career_seasons}{Number of seasons in career}
#'   \item{career_games}{Total games played in career}
#'   \item{prime_seasons}{Number of prime seasons}
#'   \item{prime_avg_tier}{Average tier value during prime seasons}
#'   \item{prime_peak_value}{Peak performance value during prime seasons}
#'   \item{prime_avg_value}{Average performance value during prime seasons}
#'   \item{prime_elite_pct}{Percentage of prime seasons at Elite level}
#'   \item{prime_great_pct}{Percentage of prime seasons at Great level}
#'   \item{prime_avg_pct}{Percentage of prime seasons at Average level}
#'   \item{career_elite_pct}{Percentage of career at Elite level}
#'   \item{career_great_pct}{Percentage of career at Great level}
#'   \item{prime_density}{Percentage of career that was prime}
#'   \item{latest_season_played}{Most recent season played}
#'   \item{max_age}{Maximum age in dataset}
#'   \item{is_active}{Whether player is active (played in latest season)}
#'
#' @importFrom dplyr group_by summarise left_join n_distinct select mutate across where
#'
#' @examples
#' \dontrun{
#' # Create player performance dataset
#' player_data <- create_player_performance_dataset(
#'   sports_data = sports,
#'   prime_data = player_primes_pct,
#'   cluster_data = cluster_data
#' )
#'
#' # Process prime seasons for PQI calculation
#' processed_data <- process_player_primes(player_data)
#'
#' # Calculate PQI
#' pqi <- calculate_prime_quality_index(processed_data)
#' }
#'
#' @export
process_player_primes <- function(final_dataset, season_data) {
  # First, ensure we're working with one row per player by properly grouping

  # Determine the latest season for each league
  latest_seasons <- final_dataset %>%
    dplyr::group_by(league) %>%
    dplyr::summarise(max_season = max(season, na.rm = TRUE))

  player_summaries <- final_dataset %>%
    # Add latest season info for each league
    dplyr::left_join(latest_seasons, by = "league") %>%
    # Group by player identifiers to ensure uniqueness
    dplyr::group_by(id, player_name, sport, league, max_season) %>%
    dplyr::summarise(
      position = last(position),
      # Career information
      career_seasons = dplyr::n_distinct(age),
      career_games = sum(games_played, na.rm = TRUE),

      # Prime information
      prime_seasons = sum(in_prime, na.rm = TRUE),

      # Prime performance - safely handle cases with no prime seasons
      prime_avg_tier = if(sum(in_prime) > 0) mean(tier_score[in_prime], na.rm = TRUE) else NA,
      prime_peak_value = if(sum(in_prime) > 0) max(scaled_value[in_prime], na.rm = TRUE) else NA,
      prime_avg_value = if(sum(in_prime) > 0) mean(scaled_value[in_prime], na.rm = TRUE) else NA,

      # Distribution of performance tiers during prime
      prime_elite_pct = if(sum(in_prime) > 0)
        sum(in_prime & performance_tier == "Elite", na.rm = TRUE) /
        sum(in_prime, na.rm = TRUE) * 100
      else 0,
      prime_great_pct = if(sum(in_prime) > 0)
        sum(in_prime & performance_tier == "Great", na.rm = TRUE) /
        sum(in_prime, na.rm = TRUE) * 100
      else 0,
      prime_avg_pct = if(sum(in_prime) > 0)
        sum(in_prime & performance_tier == "Average", na.rm = TRUE) /
        sum(in_prime, na.rm = TRUE) * 100
      else 0,

      # Career tier distribution (not just prime)
      career_avg_tier = mean(tier_score, na.rm = TRUE),

      career_elite_pct = sum(performance_tier == "Elite", na.rm = TRUE) / dplyr::n() * 100,
      career_great_pct = sum(performance_tier == "Great", na.rm = TRUE) / dplyr::n() * 100,
      career_avg_value = mean(scaled_value, na.rm = TRUE),
      career_peak_value = max(scaled_value, na.rm = TRUE),

      # Prime density (what percentage of career was prime)
      prime_density = prime_seasons / career_seasons * 100,

      # Track if player is active (played in league's latest season and under 35)
      latest_season_played = max(season, na.rm = TRUE),
      max_age = max(age, na.rm = TRUE),
      is_active = (latest_season_played == max_season),

      .groups = "drop"
    ) %>%
    # Remove the max_season column from final output
    dplyr::select(-max_season) %>%
    # Handle NaN values
    dplyr::mutate(dplyr::across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))

  season_data <- season_data |>
    left_join(final_dataset %>% select(id,age,scaled_value, in_prime), by = c("id", "age"))

    # Calculate standard deviation for prime and career performance
    consistency_metrics <- season_data |>
      dplyr::group_by(id) |>
      dplyr::summarise(
        # Standard deviation during prime seasons
        prime_sd = stats::sd(scaled_value[in_prime], na.rm = TRUE),
        # Standard deviation across entire career
        career_sd = stats::sd(scaled_value, na.rm = TRUE),
        # Count of seasons for validation
        prime_count = sum(in_prime, na.rm = TRUE),
        career_count = dplyr::n(),
        .groups = "drop"
      ) |>
      # Handle cases with insufficient data for SD calculation
      dplyr::mutate(
        prime_sd = ifelse(prime_count < 2 | is.na(prime_sd), 0, prime_sd),
        career_sd = ifelse(career_count < 2 | is.na(career_sd), 0, career_sd)
      )

    # Join the consistency metrics with the player summaries
    player_summaries_with_consistency <- player_summaries |>
      dplyr::left_join(consistency_metrics, by = "id") |>
      dplyr::mutate(
        # Calculate consistency factors
        # Bound the standard deviation ratio to avoid extreme values
        prime_sd_ratio = pmin(prime_sd / pmax(prime_avg_value, 0.001), 1),
        career_sd_ratio = pmin(career_sd / pmax(career_avg_value, 0.001), 1),

        # Calculate consistency factors (higher values = more consistent)
        prime_consistency = pmax(1 - (prime_sd_ratio * 0.5), 0.5),
        career_consistency = pmax(1 - (career_sd_ratio * 0.5), 0.5)
      ) |>
      # Remove intermediate calculation columns
      dplyr::select(-prime_sd_ratio, -career_sd_ratio)
    player_summaries_with_consistency <-player_summaries_with_consistency %>%
      distinct(id, .keep_all = TRUE)
    return(player_summaries_with_consistency)

}
