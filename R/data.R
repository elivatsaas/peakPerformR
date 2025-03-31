#' Combined Multi-Sport Player Data
#'
#' A standardized dataset containing player performance metrics across multiple sports
#' and leagues with normalized values for cross-sport comparisons.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{player_id}{Original player identifier from source data}
#'   \item{player_name}{Player's full name}
#'   \item{slug}{URL-friendly version of player name}
#'   \item{season}{Season year}
#'   \item{age}{Player age in years}
#'   \item{position}{Player's position in their sport}
#'   \item{value}{Raw value from source data (various metrics)}
#'   \item{pct.season.played}{Percentage of the season played}
#'   \item{games_played}{Number of games played}
#'   \item{sport}{Standardized sport category (Baseball, Hockey, Football, etc.)}
#'   \item{league}{Original league (MLB, NHL, NFL, NBA, etc.)}
#'   \item{gender}{Player gender (M or F)}
#'   \item{id}{Unique player identifier across all sports}
#'   \item{career_games}{Cumulative games played in career}
#'   \item{career_seasons}{Number of seasons played}
#'   \item{career_mean}{Career average value weighted by recency}
#'   \item{player_value}{Standardized player value metric}
#'   \item{z_score}{Z-score of player value within league and position}
#' }
#' @source Combined data from all supported leagues
"all_sports_tidy"

#' Chess Player Data
#'
#' A dataset containing performance metrics for titled chess players
#' from Chess.com and birth dates from Wikidata.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{...1}{Row index}
#'   \item{username}{Chess.com username}
#'   \item{age}{Player age in years at the time of the data}
#'   \item{name}{Player's full name}
#'   \item{year}{Year of the season}
#'   \item{games_played}{Number of games played in the season}
#'   \item{start_rating}{Rating at start of the season}
#'   \item{end_rating}{Rating at end of the season}
#'   \item{avg_rating}{Average rating during the season}
#'   \item{max_rating}{Maximum rating achieved during the season}
#'   \item{min_rating}{Minimum rating during the season}
#'   \item{X}{Additional identifier}
#'   \item{title}{Chess title: GM, IM, FM, etc.}
#'   \item{gender}{Player gender (M or F)}
#' }
#' @source Chess.com API and Wikidata
"chess_tidy"

#' Major League Baseball (MLB) Player Data
#'
#' A dataset containing performance metrics for MLB players
#' from Fangraphs via the baseballr package.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{...1}{Row index}
#'   \item{playerid}{Unique player identifier}
#'   \item{PlayerName}{Player's full name}
#'   \item{Season}{MLB season year}
#'   \item{Age}{Player age in years}
#'   \item{games_batting}{Games played as a batter}
#'   \item{PA}{Plate appearances}
#'   \item{war_batting}{Wins Above Replacement as a batter}
#'   \item{RAR.x}{Runs Above Replacement (batting)}
#'   \item{OPS}{On-base Plus Slugging}
#'   \item{wOBA}{Weighted On-Base Average}
#'   \item{Batting}{Batting runs}
#'   \item{Defense}{Defensive runs}
#'   \item{Offense}{Offensive contribution}
#'   \item{BaseRunning}{Base running contribution}
#'   \item{Positional}{Positional adjustment}
#'   \item{BB_pct.x}{Walk percentage (batting)}
#'   \item{K_pct.x}{Strikeout percentage (batting)}
#'   \item{ISO}{Isolated Power}
#'   \item{BABIP}{Batting Average on Balls in Play}
#'   \item{wRC}{Weighted Runs Created}
#'   \item{id}{Unique ID combining player and season}
#'   \item{games_pitching}{Games played as a pitcher}
#'   \item{starts_pitching}{Games started as a pitcher}
#'   \item{IP}{Innings Pitched}
#'   \item{war_pitching}{Wins Above Replacement as a pitcher}
#'   \item{RAR.y}{Runs Above Replacement (pitching)}
#'   \item{ERA}{Earned Run Average}
#'   \item{FIP}{Fielding Independent Pitching}
#'   \item{K_9}{Strikeouts per 9 innings}
#'   \item{BB_9}{Walks per 9 innings}
#'   \item{HR_9}{Home Runs per 9 innings}
#'   \item{WHIP}{Walks and Hits per Inning Pitched}
#'   \item{K_pct.y}{Strikeout percentage (pitching)}
#'   \item{BB_pct.y}{Walk percentage (pitching)}
#'   \item{fielding_games}{Games played on defense}
#'   \item{fielding_innings}{Innings played on defense}
#'   \item{fielding_pct}{Fielding percentage}
#'   \item{primary_position}{Primary defensive position}
#'   \item{GP}{Games played for two-way players}
#'   \item{is_two_way}{Whether player is a two-way player}
#'   \item{player_type}{Position Player, Pitcher, or Two-Way}
#'   \item{total_games}{Total games played in season}
#'   \item{total_war}{Combined WAR (batting + pitching)}
#'   \item{RAR}{Total Runs Above Replacement}
#'   \item{K_pct}{Overall strikeout percentage}
#'   \item{BB_pct}{Overall walk percentage}
#'   \item{max_games}{Maximum games in a season}
#'   \item{pct.season.played}{Percentage of the season played}
#' }
#' @source Fangraphs via baseballr package
"mlb_tidy"

#' National Football League (NFL) Player Data
#'
#' A dataset containing performance metrics for NFL players
#' from the nflreadr package.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{...1}{Row index}
#'   \item{player_id}{Unique player identifier (GSIS ID)}
#'   \item{season}{NFL season year}
#'   \item{team}{Player's team abbreviation}
#'   \item{fantasy_points_season}{Total fantasy points (PPR format) for the season}
#'   \item{games_played}{Number of games played in the season}
#'   \item{position}{Player's position (QB, RB, WR, etc.)}
#'   \item{position_group}{Broader position category (Passer, Rusher, Receiver, etc.)}
#'   \item{passing_yards}{Total passing yards}
#'   \item{passing_yards_after_catch}{Yards after catch on passes}
#'   \item{passing_tds}{Total passing touchdowns}
#'   \item{interceptions}{Total interceptions thrown}
#'   \item{dakota}{Dakota metric value (QB performance)}
#'   \item{passing_epa}{Expected Points Added from passing plays}
#'   \item{rushing_yards}{Total rushing yards}
#'   \item{rushing_first_downs}{Rushing first downs gained}
#'   \item{rushing_tds}{Total rushing touchdowns}
#'   \item{rushing_epa}{Expected Points Added from rushing plays}
#'   \item{receiving_yards}{Total receiving yards}
#'   \item{receiving_tds}{Total receiving touchdowns}
#'   \item{targets}{Total targets}
#'   \item{target_share}{Percentage of team targets}
#'   \item{racr}{Receiver Air Conversion Ratio}
#'   \item{receptions}{Total receptions}
#'   \item{receiving_epa}{Expected Points Added from receiving plays}
#'   \item{def_tackles}{Total defensive tackles}
#'   \item{def_tackles_for_loss}{Defensive tackles for loss}
#'   \item{def_fumbles_forced}{Defensive fumbles forced}
#'   \item{def_sacks}{Total defensive sacks}
#'   \item{def_penalty}{Defensive penalties}
#'   \item{def_qb_hits}{Quarterback hits}
#'   \item{def_interceptions}{Total defensive interceptions}
#'   \item{def_pass_defended}{Passes defended}
#'   \item{def_tds}{Defensive touchdowns}
#'   \item{fg_made_0_19}{Field goals made 0-19 yards}
#'   \item{fg_made_20_29}{Field goals made 20-29 yards}
#'   \item{fg_made_30_39}{Field goals made 30-39 yards}
#'   \item{fg_made_40_49}{Field goals made 40-49 yards}
#'   \item{fg_made_50_59}{Field goals made 50-59 yards}
#'   \item{fg_made_60_}{Field goals made 60+ yards}
#'   \item{fg_missed_0_19}{Field goals missed 0-19 yards}
#'   \item{fg_missed_20_29}{Field goals missed 20-29 yards}
#'   \item{fg_missed_30_39}{Field goals missed 30-39 yards}
#'   \item{fg_missed_40_49}{Field goals missed 40-49 yards}
#'   \item{fg_missed_50_59}{Field goals missed 50-59 yards}
#'   \item{fg_missed_60_}{Field goals missed 60+ yards}
#'   \item{gwfg_att}{Game-winning field goal attempts}
#'   \item{gwfg_distance}{Game-winning field goal distances}
#'   \item{gwfg_made}{Game-winning field goals made}
#'   \item{gwfg_missed}{Game-winning field goals missed}
#'   \item{pat_made}{Extra points made}
#'   \item{pat_att}{Extra point attempts}
#'   \item{career_games}{Cumulative games played in career}
#'   \item{career_seasons}{Number of seasons played}
#'   \item{position_value}{Position-specific value metric (0-100 scale)}
#'   \item{fpts.game}{Fantasy points per game}
#'   \item{fpts.season}{Fantasy points for season}
#'   \item{pct.season.played}{Percentage of the season played}
#'   \item{fpts.season.s.p}{Standardized season fantasy points by position}
#'   \item{display_name}{Player's full name}
#'   \item{birth_date}{Player's date of birth}
#'   \item{age}{Player age in years}
#'   \item{position_value_2}{Alternative position-specific value metric}
#'   \item{total_snaps}{Total snaps played (if available)}
#'   \item{avg_snap_pct}{Average snap percentage (if available)}
#' }
#' @source NFL data via nflreadr package
"nfl_tidy"

#' Major League Soccer (MLS) Player Data
#'
#' A dataset containing performance metrics for MLS players
#' from American Soccer Analysis.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{...1}{Row index}
#'   \item{season}{MLS season year}
#'   \item{player_id}{Unique player identifier}
#'   \item{player_name}{Player's full name}
#'   \item{team_id}{Team identifier}
#'   \item{position_group}{Player's position group}
#'   \item{player_quality_score}{Overall player quality metric}
#'   \item{goals_above_replacement_per_96}{Goals above replacement per 96 minutes}
#'   \item{position_value}{Position-specific value metric}
#'   \item{age}{Player age in years}
#'   \item{pct.season.played}{Percentage of the season played}
#'   \item{minutes_played}{Total minutes played}
#' }
#' @source American Soccer Analysis API
"mls_tidy"

#' National Basketball Association (NBA) Player Data
#'
#' A dataset containing performance metrics for NBA players
#' from the hoopR package.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{...1}{Row index}
#'   \item{X}{Additional identifier}
#'   \item{athlete_id}{Unique player identifier}
#'   \item{athlete_display_name}{Player's full name}
#'   \item{position}{Player's position}
#'   \item{team_id}{Team identifier}
#'   \item{season}{NBA season year}
#'   \item{pie.season}{Player Impact Estimate for season}
#'   \item{pie.game}{Player Impact Estimate per game}
#'   \item{games_played}{Number of games played}
#'   \item{pie.season.f}{Full-season equivalent PIE}
#'   \item{pie.season.s.p}{Standardized season PIE by position}
#'   \item{pie.game.s.p}{Standardized game PIE by position}
#'   \item{pct_season_played}{Percentage of the season played}
#'   \item{pie.season.f.s.p}{Standardized full-season PIE by position}
#'   \item{pie.season.s}{Standardized season PIE}
#'   \item{pie.game.s}{Standardized game PIE}
#'   \item{pie.season.f.s}{Standardized full-season PIE}
#'   \item{birth_date}{Player's date of birth}
#'   \item{season_start}{Date of season start}
#'   \item{age}{Player age in years}
#'   \item{availability}{Player availability metric}
#' }
#' @source NBA data via hoopR package
"nba_tidy"

#' National Hockey League (NHL) Player Data
#'
#' A dataset containing performance metrics for NHL players
#' from the fastRhockey package.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{...1}{Row index}
#'   \item{player_id}{Unique player identifier}
#'   \item{season}{NHL season year}
#'   \item{fantasy_points_season}{Total fantasy points for the season}
#'   \item{player_full_name}{Player's full name}
#'   \item{games_played}{Number of games played}
#'   \item{position}{Player's position type}
#'   \item{time_on_ice}{Total time on ice (seconds)}
#'   \item{skater_stats_assists}{Total assists}
#'   \item{skater_stats_goals}{Total goals}
#'   \item{skater_stats_shots}{Total shots}
#'   \item{skater_stats_hits}{Total hits}
#'   \item{skater_stats_power_play_goals}{Power play goals}
#'   \item{skater_stats_power_play_assists}{Power play assists}
#'   \item{skater_stats_penalty_minutes}{Penalty minutes}
#'   \item{skater_stats_face_off_wins}{Faceoff wins}
#'   \item{skater_stats_faceoff_taken}{Faceoffs taken}
#'   \item{skater_stats_takeaways}{Takeaways}
#'   \item{skater_stats_giveaways}{Giveaways}
#'   \item{skater_stats_short_handed_goals}{Short-handed goals}
#'   \item{skater_stats_plus_minus}{Plus/minus statistic}
#'   \item{skater_stats_even_time_on_ice}{Even strength time on ice (seconds)}
#'   \item{skater_stats_power_play_time_on_ice}{Power play time on ice (seconds)}
#'   \item{skater_stats_short_handed_time_on_ice}{Short-handed time on ice (seconds)}
#'   \item{fpts.game}{Fantasy points per game}
#'   \item{fpts.season}{Fantasy points for season}
#'   \item{pct.season.played}{Percentage of the season played}
#'   \item{goals_per_game}{Goals per game}
#'   \item{assists_per_game}{Assists per game}
#'   \item{shots_per_game}{Shots per game}
#'   \item{ppg_per_game}{Power play goals per game}
#'   \item{ppa_per_game}{Power play assists per game}
#'   \item{hits_per_game}{Hits per game}
#'   \item{takeaways_per_game}{Takeaways per game}
#'   \item{giveaways_per_game}{Giveaways per game}
#'   \item{toi_per_game}{Time on ice per game (seconds)}
#'   \item{even_toi_per_game}{Even strength time on ice per game (seconds)}
#'   \item{pp_toi_per_game}{Power play time on ice per game (seconds)}
#'   \item{sh_toi_per_game}{Short-handed time on ice per game (seconds)}
#'   \item{faceoff_win_pct}{Faceoff win percentage}
#'   \item{position_value}{Position-specific value metric}
#'   \item{goalie_stats_time_on_ice}{Goalie time on ice (seconds)}
#'   \item{goalie_stats_assists}{Goalie assists}
#'   \item{goalie_stats_goals}{Goalie goals}
#'   \item{goalie_stats_pim}{Goalie penalty minutes}
#'   \item{goalie_stats_shots}{Shots against goalie}
#'   \item{goalie_stats_saves}{Goalie saves}
#'   \item{goalie_stats_power_play_saves}{Power play saves}
#'   \item{goalie_stats_short_handed_saves}{Short-handed saves}
#'   \item{goalie_stats_even_saves}{Even strength saves}
#'   \item{goalie_stats_short_handed_shots_against}{Short-handed shots against}
#'   \item{goalie_stats_even_shots_against}{Even strength shots against}
#'   \item{goalie_stats_power_play_shots_against}{Power play shots against}
#'   \item{goalie_wins}{Goalie wins}
#'   \item{goalie_stats_save_percentage}{Goalie save percentage}
#'   \item{saves_per_game}{Saves per game}
#'   \item{shots_against_per_game}{Shots against per game}
#'   \item{save_pct}{Save percentage}
#'   \item{even_save_pct}{Even strength save percentage}
#'   \item{pp_save_pct}{Power play save percentage}
#'   \item{sh_save_pct}{Short-handed save percentage}
#'   \item{goals_against}{Goals against}
#'   \item{goals_against_avg}{Goals against average}
#'   \item{win_pct}{Win percentage}
#'   \item{workload}{Workload metric}
#'   \item{quality_start_threshold}{Quality start threshold}
#'   \item{is_quality_start}{Whether it was a quality start}
#'   \item{league_avg_save_pct}{League average save percentage}
#'   \item{expected_goals_against}{Expected goals against}
#'   \item{gsaa}{Goals saved above average}
#'   \item{birthDate}{Player's date of birth}
#'   \item{season_start_year}{Year the season started}
#'   \item{age}{Player age in years}
#' }
#' @source NHL data via fastRhockey package
"nhl_tidy"

#' National Women's Soccer League (NWSL) Player Data
#'
#' A dataset containing performance metrics for NWSL players
#' from American Soccer Analysis.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{...1}{Row index}
#'   \item{season}{NWSL season year}
#'   \item{player_id}{Unique player identifier}
#'   \item{player_name}{Player's full name}
#'   \item{team_id}{Team identifier}
#'   \item{position_group}{Player's position group}
#'   \item{player_quality_score}{Overall player quality metric}
#'   \item{goals_above_replacement_per_96}{Goals above replacement per 96 minutes}
#'   \item{position_value}{Position-specific value metric}
#'   \item{age}{Player age in years}
#'   \item{pct.season.played}{Percentage of the season played}
#'   \item{minutes_played}{Total minutes played}
#' }
#' @source American Soccer Analysis API
"nwsl_tidy"

#' Professional Women's Hockey League (PWHL) Player Data
#'
#' A dataset containing performance metrics for PWHL players
#' from the fastRhockey package.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{...1}{Row index}
#'   \item{player_id}{Unique player identifier}
#'   \item{season}{PWHL season year}
#'   \item{fantasy_points_season}{Total fantasy points for the season}
#'   \item{games_played}{Number of games played}
#'   \item{position}{Player's position type}
#'   \item{goals}{Total goals}
#'   \item{assists}{Total assists}
#'   \item{points}{Total points (goals + assists)}
#'   \item{penalty_minutes}{Penalty minutes}
#'   \item{shots_on_goal}{Shots on goal}
#'   \item{blocks}{Shots blocked}
#'   \item{giveaways}{Giveaways}
#'   \item{takeaways}{Takeaways}
#'   \item{powerplay_goals}{Power play goals}
#'   \item{shorthanded_goals}{Short-handed goals}
#'   \item{shots}{Total shots attempted}
#'   \item{shots_blocked}{Player's shots that were blocked}
#'   \item{faceoffs_won}{Faceoffs won}
#'   \item{faceoffs_lost}{Faceoffs lost}
#'   \item{fpts.game}{Fantasy points per game}
#'   \item{fpts.season}{Full season fantasy points}
#'   \item{pct.season.played}{Percentage of the season played}
#'   \item{goals_per_game}{Goals per game}
#'   \item{assists_per_game}{Assists per game}
#'   \item{points_per_game}{Points per game}
#'   \item{shots_per_game}{Shots per game}
#'   \item{shots_on_goal_per_game}{Shots on goal per game}
#'   \item{pp_goals_per_game}{Power play goals per game}
#'   \item{sh_goals_per_game}{Short-handed goals per game}
#'   \item{blocks_per_game}{Blocks per game}
#'   \item{takeaways_per_game}{Takeaways per game}
#'   \item{giveaways_per_game}{Giveaways per game}
#'   \item{pim_per_game}{Penalty minutes per game}
#'   \item{shots_blocked_per_game}{Shots blocked per game}
#'   \item{faceoffs_taken}{Total faceoffs taken}
#'   \item{faceoff_win_pct}{Faceoff win percentage}
#'   \item{position_value}{Position-specific value metric}
#'   \item{minutes_played}{Minutes played}
#'   \item{shots_against}{Shots against (goalies)}
#'   \item{goals_against}{Goals against (goalies)}
#'   \item{saves}{Saves made (goalies)}
#'   \item{save_percent}{Save percentage (goalies)}
#'   \item{shutout}{Shutouts (goalies)}
#'   \item{fantasy_points}{Fantasy points (goalies)}
#'   \item{minutes_per_game}{Minutes per game (goalies)}
#'   \item{saves_per_game}{Saves per game (goalies)}
#'   \item{shots_against_per_game}{Shots against per game (goalies)}
#'   \item{goals_against_avg}{Goals against average (goalies)}
#'   \item{estimated_wins}{Estimated wins (goalies)}
#'   \item{win_pct}{Win percentage (goalies)}
#'   \item{quality_start_threshold}{Quality start threshold (goalies)}
#'   \item{is_quality_start}{Whether it was a quality start (goalies)}
#'   \item{league_avg_save_pct}{League average save percentage}
#'   \item{expected_goals_against}{Expected goals against (goalies)}
#'   \item{gsaa}{Goals saved above average (goalies)}
#'   \item{birth_year}{Player's birth year}
#'   \item{player_name}{Player's full name}
#'   \item{age}{Player age in years}
#' }
#' @source PWHL data via fastRhockey package
"pwhl_tidy"

#' Women's National Basketball Association (WNBA) Player Data
#'
#' A dataset containing performance metrics for WNBA players
#' from the wehoop package.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{...1}{Row index}
#'   \item{X}{Additional identifier}
#'   \item{athlete_display_name}{Player's full name}
#'   \item{athlete_id}{Unique player identifier}
#'   \item{position}{Player's position}
#'   \item{season}{WNBA season year}
#'   \item{pie.season}{Player Impact Estimate for season}
#'   \item{pie.game}{Player Impact Estimate per game}
#'   \item{games_played}{Number of games played}
#'   \item{pie.season.f}{Full-season equivalent PIE}
#'   \item{pie.season.s.p}{Standardized season PIE by position}
#'   \item{pie.game.s.p}{Standardized game PIE by position}
#'   \item{pct_season_played}{Percentage of the season played}
#'   \item{pie.season.f.s.p}{Standardized full-season PIE by position}
#'   \item{pie.season.s}{Standardized season PIE}
#'   \item{pie.game.s}{Standardized game PIE}
#'   \item{pie.season.f.s}{Standardized full-season PIE}
#'   \item{birth_date}{Player's date of birth}
#'   \item{season_start}{Date of season start}
#'   \item{age}{Player age in years}
#'   \item{availability}{Player availability metric}
#' }
#' @source WNBA data via wehoop package
"wnba_tidy"

#' NHL Player Birthdays
#'
#' A dataset containing birth dates for NHL players.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{...1}{Row index}
#'   \item{id}{Player ID matching the NHL API}
#'   \item{birthDate}{Date of birth in YYYY-MM-DD format}
#' }
#' @source NHL API
"nhl_birthdays"

#' PWHL Player Ages
#'
#' A dataset containing birth years for PWHL players.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{player_id}{Player ID matching the fastRhockey package}
#'   \item{birth_year}{Year of birth}
#'   \item{player_name}{Player's full name}
#' }
#' @source EliteProspects web scraping
"pwhl_players_with_ages"

#' MLB Batter-Pitcher IDs
#'
#' A dataset identifying players who were both batters and pitchers in a season.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{playerid}{Player ID matching Fangraphs}
#'   \item{Season}{MLB season year}
#' }
#' @source Fangraphs via baseballr package
"batter_pitcher_ids"
