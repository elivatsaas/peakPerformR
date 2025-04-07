# Dockerfile without renv - installing packages directly including git

# Start with a specific R version image (Using 4.4.1 as previously discussed)
FROM rocker/r-ver:4.4.1

# Set DEBIAN_FRONTEND to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

# --- Install System Dependencies ---
# Added git, required by remotes::install_github
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    git `# <--- Added git` \
 && apt-get clean && rm -rf /var/lib/apt/lists/*

# --- Install R Packages Directly ---
# Install packages needed for installation (remotes) and runtime (plumber, etc.)
# Plus all packages listed in the 'Imports' section of peakPerformR's DESCRIPTION
# install.packages() should handle most transitive dependencies.
RUN echo "--- Installing CRAN dependencies ---" && \
    R -e "install.packages(c( \
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
    'baseballr', \
    'fastRhockey', \
    'hoopR', \
    'itscalledsoccer', \
    'magrittr', \
    'nflreadr', \
    'wehoop', \
    'rlang', \
    'tibble', \
    'utils' \
    ), repos='https://cloud.r-project.org/')"

# --- Configure Application ---
WORKDIR /app

# Copy your application code (which might include peakPerformR source if not installing from GitHub)
# This includes your plumber/, data/, start.sh etc.
COPY . /app/

# --- Install Your GitHub Package ---
# Installs the public package from GitHub after its dependencies are installed.
RUN echo "--- Installing GitHub package peakPerformR ---" && \
    R -e "remotes::install_github('elivatsaas/peakPerformR')"

# --- Optional Post-Install Verification ---
# Check if the package can be loaded (basic check)
RUN echo "--- Verifying peakPerformR installation ---" && \
    R -e "if (!requireNamespace('peakPerformR', quietly = TRUE)) stop('peakPerformR failed to install or load')" && \
    echo "peakPerformR loaded successfully."

# --- Verify Copied Application Files ---
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
