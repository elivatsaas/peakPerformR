# Dockerfile without renv - installing packages directly

# Start with a specific R version image (Choose one, e.g., 4.4.1 or 4.3.1)
# Using 4.4.1 as it might be more compatible with latest package versions
FROM rocker/r-ver:4.4.1

# Set DEBIAN_FRONTEND to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

# --- Install System Dependencies ---
# Required by R packages for compilation (keep these)
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
 && apt-get clean && rm -rf /var/lib/apt/lists/*

# --- Install R Packages Directly ---
# Install packages needed for installation (remotes) and runtime (plumber, etc.)
# Plus all packages listed in the 'Imports' section of peakPerformR's DESCRIPTION
# install.packages() should handle most transitive dependencies.
RUN R -e "install.packages(c( \
    'plumber', \
    'remotes', \
    'dplyr', \
    'purrr', \
    'tidyr', \
    'ggplot2', \
    'stringr', \
    'lubridate', \
    'httr', \
    'jsonlite', \
    'stats', \
    'magrittr', \
    'rlang', \
    'tibble', \
    'utils' \
    ), repos='https://cloud.r-project.org/')"
# Note: 'grDevices', 'stats', 'utils' are base packages, included for clarity but not strictly needed here.

# --- Configure Application ---
WORKDIR /app

# Copy your application code (which includes peakPerformR source code)
# This includes your plumber/, data/, start.sh etc.
COPY . /app/


# Ensure 'remotes' was installed in the previous step
RUN R -e "remotes::install_github('elivatsaas/peakPerformR')"

# Verify files copied (Optional Debugging)
RUN echo "--- Files in /app after COPY ---" && ls -lha /app && echo "--- Files in /app/plumber ---" && ls -lha /app/plumber || echo "/app/plumber not found or empty"

# Ensure the data directory exists (safer after COPY)
RUN mkdir -p ./data

# --- Setup Startup Script ---
# Make the copied start.sh executable (ensure it exists and is copied by 'COPY . /app/')
RUN chmod +x /app/start.sh

# --- Expose Port and Run ---
# Expose the standard port 80 (ensure start.sh uses this port)
EXPOSE 80

# Run via the start.sh script (ensure it uses port 80)
CMD ["/app/start.sh"]
