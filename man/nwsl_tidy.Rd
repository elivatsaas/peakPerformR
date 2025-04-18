% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{nwsl_tidy}
\alias{nwsl_tidy}
\title{National Women's Soccer League (NWSL) Player Data}
\format{
A data frame with variables:
\describe{
\item{...1}{Row index}
\item{season}{NWSL season year}
\item{player_id}{Unique player identifier}
\item{player_name}{Player's full name}
\item{team_id}{Team identifier}
\item{position_group}{Player's position group}
\item{player_quality_score}{Overall player quality metric}
\item{goals_above_replacement_per_96}{Goals above replacement per 96 minutes}
\item{position_value}{Position-specific value metric}
\item{age}{Player age in years}
\item{pct.season.played}{Percentage of the season played}
\item{minutes_played}{Total minutes played}
}
}
\source{
American Soccer Analysis API
}
\usage{
nwsl_tidy
}
\description{
A dataset containing performance metrics for NWSL players
from American Soccer Analysis.
}
\keyword{datasets}
