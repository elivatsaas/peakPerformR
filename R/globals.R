# Global variables declaration to avoid R CMD check notes
utils::globalVariables(c(
  # Common variables across multiple functions
  ".", "age", "player_id", "player_name", "position", "season", "games_played",
  "pct.season.played", "player_value", "fpts.game", "position_value", "total_war",
  "player_quality_score", "pie.game", "max_rating", "value", "slug", "sport",
  "league", "gender", "data", "summarise",

  # R utils functions
  "read.csv", "write.csv", "install.packages",

  # MLB-specific
  "playerid", "PlayerName", "Season", "Age", "BABIP", "BB_9", "BB_pct",
  "BaseRunning", "Batting", "Defense", "ERA", "FIP", "G", "GP", "GS", "HR_9",
  "IP", "ISO", "Inn", "K_9", "K_pct", "OPS", "Offense", "PA", "Positional",
  "RAR", "WHIP", "wOBA", "wRC", "games_batting", "games_pitching", "war_batting",
  "war_pitching", "player_type", "FP", "Pos", "fielding_pct", "total_games",
  "is_two_way", "WAR", "Date", "Team", "Opp", "id", "ever_two_way", "ever_pitcher",
  "player_type_consistent",

  # NFL-specific
  "team", "fantasy_points_season", "display_name", "birth_date", "avg_snap_pct",
  "passing_yards", "passing_tds", "interceptions", "rushing_yards", "rushing_tds",
  "receiving_yards", "receiving_tds", "targets", "receptions", "def_tackles",
  "def_sacks", "def_interceptions", "total_snaps", "position_group", "position_value_2",
  "fantasy_points", "fantasy_points_ppr", "def_tds", "def_tackles_for_loss",
  "def_pass_defended", "def_safety", "def_fumble_recovery_opp", "def_fumbles_forced",
  "def_tackles_solo", "def_tackle_assists", "fg_made_0_19", "fg_made_20_29",
  "fg_made_30_39", "fg_made_40_49", "fg_made_50_59", "fg_made_60_", "pat_made",
  "pat_missed", "pat_blocked", "fg_missed_0_19", "fg_missed_20_29", "fg_missed_30_39",
  "fg_missed_40_49", "fg_missed_50_59", "fg_missed_60_", "fg_blocked", "gwfg_blocked",
  "recent_team", "week", "passing_yards_after_catch", "dakota", "passing_epa",
  "rushing_first_downs", "rushing_epa", "target_share", "racr", "receiving_epa",
  "def_penalty", "def_qb_hits", "gwfg_att", "gwfg_distance", "gwfg_made", "gwfg_missed",
  "pat_att", "fpts.season", "gsis_id", "age_cutoff", "pfr_player_id", "pfr_id",
  "player", "offense_snaps", "defense_snaps", "st_snaps", "offense_pct", "defense_pct",
  "st_pct",

  # Basketball-specific
  "athlete_id", "athlete_display_name", "pct_season_played", "game_id", "team_score",
  "field_goals_made", "free_throws_made", "field_goals_attempted", "free_throws_attempted",
  "defensive_rebounds", "offensive_rebounds", "assists", "steals", "blocks", "fouls",
  "turnovers", "team_name", "team_id.x", "team_id.y", "minutes",
  "three_point_field_goals_made", "three_point_field_goals_attempted", "rebounds",
  "plus_minus", "points", "athlete_position_abbreviation", "team_winner", "game_pts",
  "game_fg_m", "game_ft_m", "game_fg_a", "game_ft_a", "game_def_reb", "game_off_reb",
  "game_assists", "game_steals", "game_blocks", "game_pf", "game_to", "pie",
  "pie.season", "pie.season.f", "pie.season.s.p", "pie.game.s.p", "pie.season.f.s.p",
  "PLAYER", "SEASON", "BIRTH_DATE", "PLAYER_ID", "unique_athlete_ids", "is_duplicate",
  "season_start",

  # NHL/Hockey-specific
  "position_type", "skater_stats_goals", "skater_stats_assists", "skater_stats_shots",
  "skater_stats_plus_minus", "skater_stats_blocked", "skater_stats_power_play_goals",
  "skater_stats_power_play_assists", "skater_stats_short_handed_goals",
  "skater_stats_short_handed_assists", "goalie_stats_decision",
  "goalie_stats_save_percentage", "wins", "goalie_stats_shots", "shutout",
  "player_full_name", "skater_stats_time_on_ice", "skater_stats_hits",
  "skater_stats_penalty_minutes", "skater_stats_face_off_wins",
  "skater_stats_faceoff_taken", "skater_stats_takeaways", "skater_stats_giveaways",
  "skater_stats_even_time_on_ice", "skater_stats_power_play_time_on_ice",
  "skater_stats_short_handed_time_on_ice", "fpts.season", "goalie_stats_time_on_ice",
  "goalie_stats_assists", "goalie_stats_goals", "goalie_stats_pim", "goalie_stats_saves",
  "goalie_stats_power_play_saves", "goalie_stats_short_handed_saves",
  "goalie_stats_even_saves", "goalie_stats_short_handed_shots_against",
  "goalie_stats_even_shots_against", "goalie_stats_power_play_shots_against",
  "time_on_ice", "goals_against", "goalie_wins", "save_pct", "quality_start_threshold",
  "league_avg_save_pct", "expected_goals_against", "gsaa", "win_pct", "sh_save_pct",
  "workload", "birthDate", "season_start_year", "birth_year",  "shots_against_per_game",

  # Soccer variables
  "team_id", "general_position", "minutes_played", "goals_added_raw",
  "goals_added_above_replacement", "count_actions", "xgoals", "goals",
  "goals_minus_xgoals", "xassists", "primary_assists", "goals_plus_primary_assists",
  "points_added", "xpoints_added", "pass_completion_percentage",
  "passes_completed_over_expected_p100", "share_team_touches", "goals_added_per_96",
  "xassists_per_96", "pass_value", "ga_score", "passing_score", "creation_score",
  "finishing_score", "goals_above_replacement_per_96", "g_plus_a_per_96", "shots_faced",
  "goals_conceded", "saves", "xgoals_gk_faced", "goals_minus_xgoals_gk",
  "goals_divided_by_xgoals_gk", "goals_saved_above_expected", "ga_per_96",
  "save_percentage", "shots_faced_per_96", "goals_saved_above_expected_per_96",
  "gsae_score", "save_pct_score", "workload_score", "position_rank",

  # PWHL specific
  "goals", "assists", "shots_on_goal", "blocks", "powerplay_goals",
  "shorthanded_goals", "takeaways", "save_percent", "shots_against", "points",
  "penalty_minutes", "giveaways", "shots", "shots_blocked", "faceoffs_won",
  "faceoffs_lost", "faceoffs_taken", "estimated_wins",

  # Chess specific
  "username", "name", "wikidata_name", "country_code", "country_code_alpha2",
  "country", "dob", "rules", "white", "black", "end_time", "game_date", "rating",
  "title",

  # External packages
  "httr", "dplyr", "purrr", "WikidataR", "progress", "lubridate", "stringr",
  "future_map", "furrr_options",

  # Trajectory analysis variables
  "model_info", "knots", "fit_method", "fit_success", "model", "min_age", "max_age",
  "trajectory", "trajectory_predicted_value", "trajectory_age", "availability_percentile",
  "unique_id", "id_suffix",   "boundary", "centroid", "cluster", "debut_age", "end_age",
  "group_mean", "group_sd", "last", "max_score", "max_value_age",
  "min_score", "performance_tier", "pqi_score", "pqi_selected",
  "predicted_value", "prime_avg_tier", "prime_avg_value",
  "prime_duration", "prime_peak_value", "prime_seasons",
  "prime_tier", "scaled_value", "start_age", "threshold_value",
  "upper_bound"
))
