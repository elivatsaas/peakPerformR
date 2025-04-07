# --- Base Image ---
# Use a specific version tag instead of 'latest' for reproducible builds.
# Find available tags at: https://hub.docker.com/r/rocker/plumber/tags
# Replace with your desired specific version
FROM rocker/plumber:4.3.1

# --- Install R Packages ---
# Install CRAN packages first. Using install2.r is good practice.
# Keeping them in one RUN command improves layer caching.
RUN install2.r --error --deps TRUE \
    jsonlite \
    dplyr \
    tidyr \
    readr \
    purrr \
    remotes \
    && rm -rf /tmp/downloaded_packages/ /var/lib/apt/lists/* # Clean up cache

# Install custom package from GitHub.
# For better reproducibility, consider using a specific commit hash:
# RUN R -e "remotes::install_github('elivatsaas/peakPerformR@your_commit_hash')"
RUN R -e "remotes::install_github('elivatsaas/peakPerformR')"

# --- Application Setup ---
# Set the working directory for subsequent commands
WORKDIR /app

# Create the data directory within the working directory
RUN mkdir -p ./data

# Copy application files *after* dependencies are installed.
# This leverages Docker caching: if only your code changes,
# the R package installation layers won't need to be rebuilt.
COPY plumber/plumber.R ./plumber.R
COPY plumber/data/ ./data/

# --- Runtime Configuration ---
# Expose the port the application will listen on
EXPOSE 8080

# Define the command to run the application
# Use the relative path now that WORKDIR is set.
CMD ["R", "-e", "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8080)"]
