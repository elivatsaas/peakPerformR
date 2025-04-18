% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_mlb.R
\name{build_mlb}
\alias{build_mlb}
\title{Build MLB Player Data}
\usage{
build_mlb(output_path = NULL, use_cached = TRUE, years = 1994:2024, ...)
}
\arguments{
\item{output_path}{Character string. Optional path to save output CSV file.}

\item{use_cached}{Logical. Whether to use cached data (TRUE) or rebuild from APIs (FALSE).}

\item{years}{Integer vector. Years to pull data for. Default is 1994:2024.}

\item{...}{Additional arguments passed to internal functions.}
}
\value{
A data frame containing MLB player data if output_path is NULL.
Otherwise, writes to the specified path and returns that path invisibly.
}
\description{
Builds a dataset of MLB players with performance data from baseballr.
By default, uses pre-packaged data included with the package. When use_cached=FALSE,
fetches and processes data from APIs, which may take significant time.
}
\examples{
\dontrun{
# Get pre-packaged data
mlb_data <- build_mlb()

# Rebuild data from APIs (takes time)
mlb_data_fresh <- build_mlb(use_cached = FALSE)

# Rebuild data for specific years
mlb_data_recent <- build_mlb(use_cached = FALSE, years = 2020:2023)

# Save to specific location
build_mlb(output_path = "path/to/save/directory")
}

}
