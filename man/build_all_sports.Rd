% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_all_sports.R
\name{build_all_sports}
\alias{build_all_sports}
\title{Build Combined Sports Analytics Dataset}
\usage{
build_all_sports(
  output_path = NULL,
  use_cached = TRUE,
  nfl_value = fpts.game,
  nhl_value = position_value,
  pwhl_value = position_value,
  mlb_value = total_war,
  mls_value = player_quality_score,
  nwsl_value = player_quality_score,
  nba_value = pie.game,
  wnba_value = pie.game,
  chess_value = max_rating,
  ...
)
}
\arguments{
\item{output_path}{Character string. Optional path to save output CSV file.}

\item{use_cached}{Logical. Whether to use cached data (TRUE) or rebuild from APIs (FALSE).}

\item{nfl_value}{Symbol. The metric to use for NFL player value. Default is fpts.game}

\item{nhl_value}{Symbol. The metric to use for NHL player value. Default is position_value.}

\item{pwhl_value}{Symbol. The metric to use for PWHL player value. Default is position_value.}

\item{mlb_value}{Symbol. The metric to use for MLB player value. Default is total_war.}

\item{mls_value}{Symbol. The metric to use for MLS player value. Default is player_quality_score.}

\item{nwsl_value}{Symbol. The metric to use for NWSL player value. Default is player_quality_score.}

\item{nba_value}{Symbol. The metric to use for NBA player value. Default is pie.game.}

\item{wnba_value}{Symbol. The metric to use for WNBA player value. Default is pie.game.}

\item{chess_value}{Symbol. The metric to use for chess player value. Default is max_rating.}

\item{...}{Additional arguments passed to individual build functions.}
}
\value{
A data frame containing standardized player data across all sports, or
the path to the saved CSV file if output_path is specified.
}
\description{
Creates a combined dataset of player performance across multiple sports leagues,
standardizing player values and metrics to allow for cross-sport comparisons.
By default, uses pre-packaged data included with the package. When use_cached=FALSE,
rebuilds from source data for each sport.
}
\examples{
\dontrun{
# Get pre-packaged combined data
all_sports <- build_all_sports()

# Rebuild data for all sports and customize value metrics
all_sports_custom <- build_all_sports(
  use_cached = FALSE,
  nfl_value = position_value_2,
  mlb_value = war_batting
)

# Save to specific location
build_all_sports(output_path = "path/to/save/directory")
}

}
