#' Identify Player Prime Years (Robust Vectorized Version v11)
#'
#' @description
#' Identifies the prime years in a player's career using vectorized `dplyr`
#' operations. Determines prime as a period where performance is above a
#' threshold relative to the player's peak (max-min range). Includes a mechanism
#' to skip one season below the threshold if games played were low. This skip
#' mechanism AND the corresponding output flags (`skip_before_used`,
#' `skip_after_used`) are fully disabled for specified leagues (e.g., chess).
#' Handles NA values, edge cases, potential name conflicts during processing,
#' and calculation errors robustly. Addresses issues with combining results after
#' reframe by directly using reframe's output which includes the ID.
#'
#' @param data A data frame containing player performance data.
#'             Requires columns: `id`, `age`, `games_played`, `league`.
#'             If `method = "predicted"`: Requires `predicted_value`.
#'             If `method = "actual"`: Requires `player_value`, `season`.
#'             Handles potential duplicate input column names (e.g., 'value').
#' @param method Character string. Method for identifying prime: `"predicted"` or
#'               `"actual"`. Default: `"predicted"`.
#' @param threshold_pct Numeric. Percentage of peak performance range (`max - min`)
#'                      used as the lower bound for prime years. Default: 70.
#' @param games_pct_threshold Numeric. Percentile threshold for `games_played` within
#'                           a league. Seasons below this might trigger the skip
#'                           mechanism if performance dips (unless league is disabled).
#'                           Default: 40.
#' @param min_seasons Integer. For `method = "actual"`, the minimum number of
#'                    distinct seasons with non-NA `player_value` required for
#'                    a player to be included. Default: 5.
#' @param disable_skip_leagues Character vector. League names (case-sensitive,
#'                             must match `league` column exactly) for which the
#'                             `games_played` skip logic should be disabled
#'                             (by treating `games_played` as NA internally) AND
#'                             for which the output `skip_..._used` flags will
#'                             always be FALSE.
#'                             Default: `c("CHESS_M", "CHESS_F")`. Set to `NULL`
#'                             or `character(0)` to enable skip logic for all leagues.
#'
#' @return A data frame (tibble) containing prime years information: `id`, `league`,
#'   `max_value_age`, `start_age`, `end_age`, `prime_duration`, `threshold_value`,
#'   `threshold_pct`, `skip_before_used` (always FALSE for disabled leagues),
#'   `skip_after_used` (always FALSE for disabled leagues). Returns an empty
#'   tibble with the correct structure if no players meet criteria or errors occur.
#'
#' @examples
#' \dontrun{
#' # --- Generate Sample Data ---
#' set.seed(456)
#' n_players_demo <- 70; n_seasons_pred_demo <- 15
#' player_ids_pred_demo <- paste0("P", 1:n_players_demo)
#' leagues_demo <- sample(c("NFL", "NBA", "CHESS_M", "CHESS_F"), n_players_demo, TRUE, c(0.35,0.35,0.15,0.15))
#' player_trajectories_demo <- expand.grid(id=player_ids_pred_demo, age=20:(20+n_seasons_pred_demo-1)) |>
#'   dplyr::mutate(
#'     league = leagues_demo[match(id, player_ids_pred_demo)],
#'     peak_age = sample(25:30, n_players_demo, TRUE)[match(id, player_ids_pred_demo)],
#'     predicted_value = round(pmax(0, 100 - 0.5*(age-peak_age)^2 + rnorm(dplyr::n(), 0, 8)), 1),
#'     player_value = predicted_value + rnorm(dplyr::n(), 0, 2),
#'     season = 2000 + (age - 20),
#'     games_played = sample(5:100, dplyr::n(), TRUE),
#'     games_played = ifelse(runif(dplyr::n()) < 0.1, sample(1:4, dplyr::n(), TRUE), games_played),
#'     games_played = ifelse(runif(dplyr::n()) < 0.05, NA, games_played),
#'     predicted_value = ifelse(runif(dplyr::n()) < 0.02, NA, predicted_value),
#'     player_value = ifelse(runif(dplyr::n()) < 0.02, NA, player_value)
#'   )
#'
#' # --- Run the function (Predicted) ---
#' player_prime_pred <- identify_prime(
#'   player_trajectories_demo, method = "predicted", threshold_pct = 80,
#'   games_pct_threshold = 40, disable_skip_leagues = c("CHESS_M", "CHESS_F")
#' )
#' print(head(player_prime_pred))
#' summary(player_prime_pred)
#'
#' # --- Check Chess Flags ---
#' chess_res <- dplyr::filter(player_prime_pred, league %in% c("CHESS_M", "CHESS_F"))
#' print("Chess Skips:"); print(summary(chess_res[c("skip_before_used", "skip_after_used")]))
#' }
#'
#' @export
#' @import dplyr
#' @importFrom rlang .data sym ':='
#' @importFrom stats quantile
#' @importFrom tibble tibble
#' @importFrom utils str

identify_prime <- function(data,
                           method = c("predicted", "actual"),
                           threshold_pct = 70,
                           games_pct_threshold = 40,
                           min_seasons = 5,
                           disable_skip_leagues = c("CHESS_M", "CHESS_F")) {

  # --- Input Validation and Setup ---
  # (Identical to v10)
  method <- match.arg(method)
  threshold_decimal <- threshold_pct / 100
  required_cols_base <- c("id", "age", "games_played", "league")
  required_cols_method <- if (method == "predicted") "predicted_value" else c("player_value", "season")
  required_cols <- c(required_cols_base, required_cols_method)
  if (!is.data.frame(data)) stop("Input 'data' must be a data frame.")
  original_data_names <- names(as.data.frame(data))
  missing_cols <- setdiff(required_cols, original_data_names)
  if (length(missing_cols) > 0) { stop("Input data missing columns: ", paste(missing_cols, collapse=", ")) }
  value_col_name <- if (method == "predicted") "predicted_value" else "player_value"
  internal_value_col_name <- "..prime_calc_value_internal.."
  empty_result_tibble <- tibble::tibble(
    id = character(), league = character(), max_value_age = numeric(),
    start_age = numeric(), end_age = numeric(), prime_duration = numeric(),
    threshold_value = numeric(), threshold_pct = numeric(),
    skip_before_used = logical(), skip_after_used = logical()
  )

  # --- Create Internal Data Copy & Apply Disabled Skip Logic ---
  # (Identical to v10)
  data_internal <- data
  apply_skip_disable <- !is.null(disable_skip_leagues) && length(disable_skip_leagues) > 0
  if (apply_skip_disable && all(c("league", "games_played") %in% names(data_internal))) {
    data_internal <- data_internal |>
      dplyr::mutate(league = as.character(league)) |>
      dplyr::mutate( games_played = ifelse( !is.na(league) & league %in% disable_skip_leagues,
                                            NA_real_, as.numeric(games_played) ))
  } else { data_internal <- data_internal |> dplyr::mutate(games_played = as.numeric(games_played)) }


  # --- Pre-processing ---
  # (Identical to v10)
  if(dplyr::is_grouped_df(data_internal)) { data_internal <- dplyr::ungroup(data_internal) }
  cols_to_select <- c("id", "age", "league", "games_played")
  if(method == "actual") cols_to_select <- c(cols_to_select, "season")
  data_processed <- data_internal |>
    dplyr::mutate( id = as.character(id), age = as.numeric(age),
                   "{internal_value_col_name}" := as.numeric(.data[[value_col_name]]),
                   league = as.character(league), games_played = as.numeric(games_played),
                   across(any_of("season"), as.character) ) |>
    dplyr::filter( !is.na(id), !is.na(age), !is.na(league),
                   !is.na(.data[[internal_value_col_name]]), is.finite(.data[[internal_value_col_name]]) ) |>
    dplyr::select(dplyr::all_of(cols_to_select), dplyr::all_of(internal_value_col_name))
  if (nrow(data_processed) == 0) { message("No valid rows after initial filtering."); return(empty_result_tibble) }

  # --- Calculate Games Thresholds ---
  # (Identical to v10)
  games_thresholds <- data_processed |>
    dplyr::filter(!is.na(games_played), is.finite(games_played)) |> dplyr::group_by(league) |>
    dplyr::summarize( n_games = n(), games_threshold_calc = if (n() > 0) {
      stats::quantile(games_played, games_pct_threshold / 100, na.rm = TRUE, type = 1) } else { NA_real_ }, .groups = "drop" ) |>
    dplyr::filter(!is.na(games_threshold_calc), is.finite(games_threshold_calc)) |>
    dplyr::select(league, games_threshold = games_threshold_calc)

  # --- Join Thresholds & Apply Method/Row Count Filters ---
  # (Identical to v10)
  data_filtered <- data_processed |> dplyr::left_join(games_thresholds, by = "league")
  min_rows_per_group_val <- if (method == "actual") { if (!"season" %in% names(data_filtered)) stop("Method 'actual' requires 'season' column."); min_seasons } else { 2 }
  if (method == "actual") {
    player_season_counts <- data_filtered |> dplyr::filter(!is.na(season)) |> dplyr::group_by(id) |>
      dplyr::summarize(num_seasons = dplyr::n_distinct(season), .groups = "drop")
    valid_player_ids <- player_season_counts |> dplyr::filter(num_seasons >= min_rows_per_group_val) |> dplyr::pull(id)
    data_filtered <- data_filtered |> dplyr::filter(id %in% valid_player_ids)
  }
  data_ready_for_reframe <- data_filtered |>
    dplyr::mutate(min_rows_needed = min_rows_per_group_val) |> dplyr::arrange(id, age) |> dplyr::group_by(id) |>
    dplyr::filter(dplyr::n() >= min_rows_needed[1]) |>
    dplyr::select( id, age, league, games_played, dplyr::all_of(internal_value_col_name), games_threshold, min_rows_needed ) |>
    dplyr::ungroup()
  if (nrow(data_ready_for_reframe) == 0) { message("No players remaining after filtering."); return(empty_result_tibble) }


  # --- Grouped Calculation using reframe ---
  prime_results_raw <- tryCatch({
    data_ready_for_reframe |>
      dplyr::group_by(id) |> # Group by ID
      dplyr::reframe({
        # Reframe code block - calculates prime metrics for each group (player)
        # ... (inner reframe logic identical to v10) ...
        player_data <- .data
        n_rows_calc <- n()
        min_rpg_calc <- player_data$min_rows_needed[1]
        player_id_calc <- player_data$id[1] # Get ID for potential warnings

        skip_used_before <- FALSE; skip_used_after <- FALSE # Initialize early

        if(is.na(min_rpg_calc)||!is.finite(min_rpg_calc)||min_rpg_calc<1) {return(tibble::tibble())}
        if(n_rows_calc < min_rpg_calc) { return(tibble::tibble()) }

        current_value_vec <- player_data[[internal_value_col_name]]
        player_league <- player_data$league[1]
        games_threshold <- player_data$games_threshold[1]

        max_idx <- which.max(current_value_vec)
        if(length(max_idx)==0) { return(tibble::tibble()) }
        max_value <- current_value_vec[max_idx]; max_age <- player_data$age[max_idx]
        min_value <- min(current_value_vec, na.rm=TRUE); value_range <- max_value-min_value
        if (!is.finite(max_value)||!is.finite(min_value)||!is.finite(value_range)){ return(tibble::tibble()) }
        threshold_value <- if (value_range<=1e-9) max_value else min_value+(value_range*threshold_decimal)
        if(!is.finite(threshold_value)) { return(tibble::tibble()) }

        start_idx <- max_idx
        if (max_idx > 1) {
          for (i in (max_idx - 1):1) {
            cv <- current_value_vec[i]; cg <- player_data$games_played[i]
            if (cv>=threshold_value) { start_idx <- i } else if (!skip_used_before&&!is.na(cg)&&is.finite(cg)&&!is.na(games_threshold)&&is.finite(games_threshold)&&cg<=games_threshold) { skip_used_before <- TRUE } else { break }
          }
        }
        end_idx <- max_idx
        if (max_idx < n_rows_calc) {
          for (i in (max_idx + 1):n_rows_calc) {
            cv <- current_value_vec[i]; cg <- player_data$games_played[i]
            if (cv>=threshold_value) { end_idx <- i } else if (!skip_used_after&&!is.na(cg)&&is.finite(cg)&&!is.na(games_threshold)&&is.finite(games_threshold)&&cg<=games_threshold) { skip_used_after <- TRUE } else { break }
          }
        }
        start_age_res <- player_data$age[start_idx]; end_age_res <- player_data$age[end_idx]
        if (!is.finite(start_age_res)||!is.finite(end_age_res)||end_age_res<start_age_res) { return(tibble::tibble()) }

        # Return tibble - reframe automatically includes the grouping key 'id'
        tibble::tibble(
          league=player_league, max_value_age=max_age, start_age=start_age_res, end_age=end_age_res,
          prime_duration=end_age_res-start_age_res+1, threshold_value=threshold_value, threshold_pct=threshold_pct,
          skip_before_used=skip_used_before, skip_after_used=skip_used_after
        )
      })
    # Note: No explicit ungroup() needed here, reframe result structure depends on dplyr version
    # but we handle the ID column below regardless.
  }, error = function(e) {
    warning("Error occurred during reframe calculation: ", conditionMessage(e), call. = FALSE)
    return(empty_result_tibble)
  })

  # --- Finalize Results ---
  # Start assuming the raw result is the final one (if it's not empty)
  prime_results <- if (nrow(prime_results_raw) > 0) prime_results_raw else empty_result_tibble

  if (nrow(prime_results) > 0) {
    # ** Step 1: Ensure 'id' column exists and handle potential renaming **
    # (This part might be redundant if reframe reliably includes 'id', but safe to keep)
    expected_id_col <- "id"
    if(!expected_id_col %in% names(prime_results)) {
      potential_id_cols <- grep(paste0("^", expected_id_col, "(\\.{3}\\d+)?$"), names(prime_results), value = TRUE)
      if (length(potential_id_cols) == 1) {
        # Rename detected column back to 'id'
        prime_results <- prime_results |>
          dplyr::rename(id = dplyr::all_of(potential_id_cols[1]))
        message(paste("Note: Detected and renamed ID column from", potential_id_cols[1], "to 'id'."))
      } else {
        warning("Could not reliably identify the 'id' column in reframe results. Returning empty.", call. = FALSE)
        return(empty_result_tibble) # Return empty if ID cannot be found/fixed
      }
    }

    # ** Step 2: Reorder columns to ensure 'id' is first **
    prime_results <- prime_results |>
      dplyr::select(dplyr::all_of(expected_id_col), dplyr::everything())

    # ** Step 3: Force Skip Flags to FALSE for Disabled Leagues **
    if (apply_skip_disable) {
      prime_results <- prime_results |>
        dplyr::mutate(
          skip_before_used = ifelse(league %in% disable_skip_leagues, FALSE, skip_before_used),
          skip_after_used  = ifelse(league %in% disable_skip_leagues, FALSE, skip_after_used)
        )
    }

  }

  # Final structure check for empty results
  if (nrow(prime_results) == 0 && inherits(prime_results, "data.frame")) {
    if(!identical(names(prime_results), names(empty_result_tibble))) {
      prime_results <- empty_result_tibble
    }
  } else if (nrow(prime_results) == 0) {
    prime_results <- empty_result_tibble
  }

  return(prime_results)
}
