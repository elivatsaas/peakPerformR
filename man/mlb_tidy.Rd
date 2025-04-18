% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{mlb_tidy}
\alias{mlb_tidy}
\title{Major League Baseball (MLB) Player Data}
\format{
A data frame with variables:
\describe{
\item{...1}{Row index}
\item{playerid}{Unique player identifier}
\item{PlayerName}{Player's full name}
\item{Season}{MLB season year}
\item{Age}{Player age in years}
\item{games_batting}{Games played as a batter}
\item{PA}{Plate appearances}
\item{war_batting}{Wins Above Replacement as a batter}
\item{RAR.x}{Runs Above Replacement (batting)}
\item{OPS}{On-base Plus Slugging}
\item{wOBA}{Weighted On-Base Average}
\item{Batting}{Batting runs}
\item{Defense}{Defensive runs}
\item{Offense}{Offensive contribution}
\item{BaseRunning}{Base running contribution}
\item{Positional}{Positional adjustment}
\item{BB_pct.x}{Walk percentage (batting)}
\item{K_pct.x}{Strikeout percentage (batting)}
\item{ISO}{Isolated Power}
\item{BABIP}{Batting Average on Balls in Play}
\item{wRC}{Weighted Runs Created}
\item{id}{Unique ID combining player and season}
\item{games_pitching}{Games played as a pitcher}
\item{starts_pitching}{Games started as a pitcher}
\item{IP}{Innings Pitched}
\item{war_pitching}{Wins Above Replacement as a pitcher}
\item{RAR.y}{Runs Above Replacement (pitching)}
\item{ERA}{Earned Run Average}
\item{FIP}{Fielding Independent Pitching}
\item{K_9}{Strikeouts per 9 innings}
\item{BB_9}{Walks per 9 innings}
\item{HR_9}{Home Runs per 9 innings}
\item{WHIP}{Walks and Hits per Inning Pitched}
\item{K_pct.y}{Strikeout percentage (pitching)}
\item{BB_pct.y}{Walk percentage (pitching)}
\item{fielding_games}{Games played on defense}
\item{fielding_innings}{Innings played on defense}
\item{fielding_pct}{Fielding percentage}
\item{primary_position}{Primary defensive position}
\item{GP}{Games played for two-way players}
\item{is_two_way}{Whether player is a two-way player}
\item{player_type}{Position Player, Pitcher, or Two-Way}
\item{total_games}{Total games played in season}
\item{total_war}{Combined WAR (batting + pitching)}
\item{RAR}{Total Runs Above Replacement}
\item{K_pct}{Overall strikeout percentage}
\item{BB_pct}{Overall walk percentage}
\item{max_games}{Maximum games in a season}
\item{pct.season.played}{Percentage of the season played}
}
}
\source{
Fangraphs via baseballr package
}
\usage{
mlb_tidy
}
\description{
A dataset containing performance metrics for MLB players
from Fangraphs via the baseballr package.
}
\keyword{datasets}
