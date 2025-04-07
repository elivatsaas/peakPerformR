# Dockerfile without renv - Installing peakPerformR without dependencies

# Use R 4.3.1 as in the "working" log, but be aware of potential future issues
FROM rocker/r-ver:4.3.1

# Set DEBIAN_FRONTEND to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

# --- Install System Dependencies ---
# Added git
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    git \
 && apt-get clean && rm -rf /var/lib/apt/lists/*

# --- Install R Packages Directly ---
# Install packages needed for runtime (plumber), installation (remotes),
# AND *all required* packages listed in Imports *EXCEPT* baseballr.
# *CRITICAL*: Ensure this list covers everything your API *actually uses*.
RUN echo "--- Installing REQUIRED CRAN dependencies (excluding baseballr) ---" && \
    R -e "install.packages(c( \
    'plumber', \
    'remotes', \
    'dplyr', \
    'purrr', \
    'tidyr', \
    'ggplot2',  # Keep if visualizations are used by the API
    'stringr', \
    'lubridate', \
    'httr', \
    'jsonlite', \
    # 'baseballr', # Skipped
    'fastRhockey', \
    'hoopR', \
    'itscalledsoccer', \
    'magrittr', \
    'nflreadr', \
    'wehoop', \
    'rlang', \
    'tibble' \
    ), repos='https://cloud.r-project.org/', Ncpus = 2)"

# --- Configure Application ---
WORKDIR /app
COPY . /app/

# --- Install Your GitHub Package (Skipping its Dependencies) ---
# Added dependencies = FALSE
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
RUN chmod +x /app/start.sh
# --- Expose Port and Run ---
# Use port 80 to align with typical Azure Container App setup
EXPOSE 80
# Run via the start.sh script (ensure it uses port 80)
CMD ["/app/start.sh"]
