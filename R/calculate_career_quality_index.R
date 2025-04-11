#' Calculate Career Quality Index (CQI) 
#'
#' @description
#' Calculates CQI using dplyr, addressing data mask errors in dynamic grouping.
#' Relies on a separate globals.R file for utils::globalVariables declarations.
#' Includes skip logic disabling, robustness features.
#' Allows negative cqi_raw values by removing pmax(0, ...).
#'
#' @param player_data A data frame with player performance data.
#'                    Requires: id, player_name, sport, position, league, season,
#'                    games_played, age, and player_value or z_score.
#' @param nfl_by_position Logical. Group NFL by position? Default: TRUE.
#' @param exclude_positions Character vector. Positions to exclude. Default: c("OL").
#' @param min_seasons Integer. Minimum career seasons. Default: 5.
#' @param tier_method Character. Tier method ("percentile" or "cluster"). Default: "percentile".
#' @param save_output Logical. Save results? Default: FALSE.
#' @param output_file Character. Output filename. Default: "cqi_scores.csv".
#'
#' @return A data frame with calculated CQI scores and tiers.
#'
#' @export
#' @import dplyr
#' @importFrom rlang .data ':='
#' @importFrom utils write.csv
#' @importFrom purrr map_dfr
#' @importFrom stats na.omit aggregate  quantile
#' @importFrom dplyr if_else first n_distinct percent_rank coalesce case_when select filter group_by summarise mutate ungroup left_join semi_join distinct arrange rename group_split bind_cols everything all_of any_of group_keys across


calculate_career_quality_index <- function(player_data,
                                                   nfl_by_position = TRUE,
                                                   exclude_positions = c("OL"),
                                                   min_seasons = 5,
                                                   tier_method = "percentile",
                                                   save_output = FALSE,
                                                   output_file = "cqi_scores.csv") {

  if (tier_method == "cluster" && !requireNamespace("cluster", quietly = TRUE)) { stop("Install 'cluster' package for cluster method.") }
  if (!tier_method %in% c("percentile", "cluster")) { stop("tier_method must be 'percentile' or 'cluster'") }
  if (length(exclude_positions) > 0 && "position" %in% names(player_data)) { player_data <- player_data |> dplyr::filter(!(.data$position %in% exclude_positions)) }
  required_input_cols<-c("id","player_name","sport","position","league","season","games_played","age")
  value_col_present <- "player_value" %in% names(player_data) || "z_score" %in% names(player_data)
  if(!all(required_input_cols %in% names(player_data)) || !value_col_present) {
    missing_req <- setdiff(required_input_cols, names(player_data))
    missing_val <- if(!value_col_present) "player_value or z_score" else NULL
    missing_all <- c(missing_req, missing_val)
    stop("Input 'player_data' is missing required columns: ", paste(missing_all, collapse=", "))
  }


  # Create scaled_value if not present
  if (!("scaled_value" %in% colnames(player_data))) {
    message("Creating scaled values by league and position...")
    value_source_col <- if ("player_value" %in% names(player_data)) "player_value" else "z_score"

    # --- MODIFIED SCALING GROUPING ---
    player_data <- player_data |>
      dplyr::mutate(
        # Ensure this name is in globals.R
        .scaling_group_pos = if (nfl_by_position && "position" %in% names(.)) {
          dplyr::if_else(.data$league == "NFL", .data$position, "All_Pos")
        } else {
          "All_Pos"
        }
      ) |>
      dplyr::group_by(.data$league, .data$.scaling_group_pos) |>
      dplyr::mutate(
        scaled_value = as.numeric(scale(.data[[value_source_col]]))
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(scaled_value = ifelse(is.nan(.data$scaled_value), 0, .data$scaled_value)) |>
      dplyr::select(-.data$.scaling_group_pos)
    # --- END MODIFIED SCALING GROUPING ---
  }

  message("Aggregating season data to career level...")
  season_counts <- player_data |> dplyr::group_by(.data$id) |>
    dplyr::summarise( season_count = dplyr::n_distinct(.data$season), .groups = "drop" ) |>
    dplyr::filter(.data$season_count >= min_seasons)
  if(nrow(season_counts) == 0) { message("No players found with min seasons."); return( # Return empty tibble...
    tibble(id=character(), player_name=character(), sport=character(), position=character(), league=character(),
           career_seasons=integer(), career_games=integer(), career_peak_value=numeric(), career_avg_value=numeric(),
           latest_season_played=integer(), max_age=integer(), is_active=logical(), cqi_raw=numeric(),
           cqi_score=numeric(), career_tier=character())) }
  player_data_filtered <- player_data |> dplyr::semi_join(season_counts, by = "id")
  if(nrow(player_data_filtered) == 0) { message("All players filtered out."); return( # Return empty tibble...
    tibble(id=character(), player_name=character(), sport=character(), position=character(), league=character(),
           career_seasons=integer(), career_games=integer(), career_peak_value=numeric(), career_avg_value=numeric(),
           latest_season_played=integer(), max_age=integer(), is_active=logical(), cqi_raw=numeric(),
           cqi_score=numeric(), career_tier=character())) }
  if (!"prime_duration" %in% names(player_data_filtered)) { player_data_filtered <- player_data_filtered |> dplyr::mutate(prime_duration = NA_integer_) }
  career_data <- player_data_filtered |>
    dplyr::group_by(.data$id, .data$player_name, .data$sport, .data$position) |>
    dplyr::summarise( league = dplyr::first(.data$league), career_seasons = dplyr::n_distinct(.data$season),
                      career_games = sum(.data$games_played, na.rm = TRUE), prime_seasons = dplyr::first(stats::na.omit(.data$prime_duration)),
                      career_peak_value = max(.data$scaled_value, na.rm = TRUE), career_avg_value = mean(.data$scaled_value, na.rm = TRUE),
                      latest_season_played = max(.data$season, na.rm = TRUE), max_age = max(.data$age, na.rm = TRUE),
                      is_active = .data$latest_season_played >= (max(player_data$season, na.rm = TRUE) - 2), .groups = "drop" ) |>
    dplyr::mutate(career_peak_value = ifelse(is.infinite(.data$career_peak_value), NA_real_, .data$career_peak_value))
  if (!"prime_seasons" %in% names(career_data)) { career_data <- career_data |> dplyr::mutate(prime_seasons = NA_integer_)
  } else { career_data <- career_data |> dplyr::mutate(prime_seasons = ifelse(is.na(.data$prime_seasons), 0L, .data$prime_seasons)) }

  # Calculate cqi_raw (MODIFIED: NO pmax)
  career_data <- career_data |>
    dplyr::mutate(
      career_avg_value_safe = dplyr::coalesce(.data$career_avg_value, 0),
      career_peak_value_safe = dplyr::coalesce(.data$career_peak_value, 0),
      career_seasons_safe = dplyr::coalesce(.data$career_seasons, 0),
      cqi_raw = dplyr::case_when(
        .data$career_seasons_safe > 0 ~
          (.data$career_avg_value_safe + (.data$career_peak_value_safe * 0.5)) * log(.data$career_seasons_safe + 1),
        TRUE ~ 0 # Default to 0 if no seasons (shouldn't happen due to filter)
      ),
      # Ensure cqi_raw is finite, replacing Inf/NaN with 0, but KEEP negatives
      cqi_raw = ifelse(is.finite(.data$cqi_raw), .data$cqi_raw, 0)
    ) |>
    dplyr::select(-dplyr::any_of(c("career_avg_value_safe", "career_peak_value_safe", "career_seasons_safe")))


  # --- MODIFIED FINAL GROUPING ---
  cqi <- career_data |>
    dplyr::mutate(
      # Ensure this name is in globals.R
      .final_group = if(nfl_by_position && "position" %in% names(.)) {
        dplyr::if_else(.data$league == "NFL", paste(.data$league, .data$position), .data$league)
      } else {
        .data$league
      }
    ) |>
    dplyr::group_by(.data$.final_group) |>
    dplyr::mutate(
      cqi_score = dplyr::percent_rank(.data$cqi_raw) * 100
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-.data$.final_group)
  # --- END MODIFIED FINAL GROUPING ---


  # (Tier assignment logic remains the same)
  if (tier_method == "percentile") {
    cqi <- cqi |> dplyr::mutate( career_tier = dplyr::case_when(
      .data$cqi_score >= 95 ~ "Hall of Fame", .data$cqi_score >= 85 ~ "Elite Career",
      .data$cqi_score >= 70 ~ "Great Career", .data$cqi_score >= 40 ~ "Solid Career",
      TRUE ~ "Limited Career" ) )
  } else { # Cluster method
    cqi <- cqi |>
      dplyr::mutate(.final_group = if(nfl_by_position && "position" %in% names(.)) { # Ensure this name is in globals.R
        dplyr::if_else(.data$league == "NFL", paste(.data$league, .data$position), .data$league)
      } else { .data$league } ) |>
      dplyr::group_split(.data$.final_group, .keep = TRUE) |>
      purrr::map_dfr(function(group_data) {
        min_players_for_clara <- 15
        if (nrow(group_data) < min_players_for_clara) {
          group_data <- group_data |> dplyr::mutate( career_tier = dplyr::case_when(
            .data$cqi_score >= 95 ~ "Hall of Fame", .data$cqi_score >= 85 ~ "Elite Career",
            .data$cqi_score >= 70 ~ "Great Career", .data$cqi_score >= 40 ~ "Solid Career",
            TRUE ~ "Limited Career" ) )
          return(group_data |> dplyr::select(-.data$.final_group))
        }
        set.seed(123); cluster_data_mat <- as.matrix(group_data$cqi_score); clara_result <- NULL
        tryCatch({ clara_result <- cluster::clara( cluster_data_mat, k = 5, samples = max(5, min(50, floor(nrow(group_data)/10))), pamLike = TRUE )
        }, error = function(e) { message("CLARA failed for group: ", unique(group_data$.final_group), ". Falling back.") })
        if (!is.null(clara_result)) {
          group_data$cluster <- clara_result$clustering
          cluster_means <- stats::aggregate(cqi_score ~ cluster, data = group_data, FUN = mean)
          cluster_means <- cluster_means[order(cluster_means$cqi_score, decreasing = TRUE), ]
          cluster_mapping <- data.frame(cluster = cluster_means$cluster, tier_rank = 1:5)
          group_data <- group_data |> dplyr::left_join(cluster_mapping, by = "cluster") |>
            dplyr::mutate( career_tier = dplyr::case_when(
              .data$tier_rank == 1 ~ "Hall of Fame", .data$tier_rank == 2 ~ "Elite Career",
              .data$tier_rank == 3 ~ "Great Career", .data$tier_rank == 4 ~ "Solid Career",
              TRUE ~ "Limited Career" ) ) |> dplyr::select(-cluster, -tier_rank)
        } else { group_data <- group_data |> dplyr::mutate( career_tier = dplyr::case_when(
          .data$cqi_score >= 95 ~ "Hall of Fame", .data$cqi_score >= 85 ~ "Elite Career",
          .data$cqi_score >= 70 ~ "Great Career", .data$cqi_score >= 40 ~ "Solid Career",
          TRUE ~ "Limited Career" ) )
        }
        return(group_data |> dplyr::select(-.data$.final_group))
      })
  }

  # Rename columns for consistency with original request output sample
  cqi <- cqi |>
    dplyr::rename(cqi_selected = cqi_score, selected_tier = career_tier) |>
    # Reorder to match the provided CSV structure as best as possible
    dplyr::select(
        id, player_name, sport, position, league, career_seasons, career_games,
        prime_seasons, career_peak_value, career_avg_value, latest_season_played,
        max_age, is_active, cqi_raw, cqi_selected, selected_tier, dplyr::everything() # Keep any extra columns at the end
    )


  # (Save output and final message remain the same)
  if(save_output) { if(nrow(cqi) > 0) { tryCatch({ utils::write.csv(cqi, output_file, row.names = FALSE); message("CQI scores saved.") }, error = function(e) { warning("Failed to save CQI output.") }) } else { message("No CQI results to save.") } }
  message("CQI calculation complete for ", nrow(cqi), " players meeting criteria using ", tier_method, " tier assignment.")
  return(cqi)
}
