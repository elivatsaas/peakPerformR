# Dockerfile without renv - installing packages directly including git
# Attempting to install peakPerformR without its dependencies

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
    git \
 && apt-get clean && rm -rf /var/lib/apt/lists/*

# --- Install R Packages Directly ---
# Install packages listed in peakPerformR's DESCRIPTION Imports
# *CRITICAL*: Ensure this step completes successfully in the build log.
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
    'magrittr', \
    'rlang', \
    'tibble', \
    'utils' \
    ), repos='https://cloud.r-project.org/')"

# --- Configure Application ---
WORKDIR /app

# Copy your application code
COPY . /app/

# --- Install Your GitHub Package ---
# Added dependencies = FALSE to skip installing its listed dependencies
RUN echo "--- Installing GitHub package peakPerformR (dependencies=FALSE) ---" && \
    R -e "remotes::install_github('elivatsaas/peakPerformR', dependencies = FALSE)"

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
