# Dockerfile without renv - Fixing Rprofile issue and adding libsodium-dev

# Use R 4.3.1 as in the log, but consider 4.4.1 if further issues arise
FROM rocker/r-ver:4.3.1

# Set DEBIAN_FRONTEND to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

# --- Install System Dependencies ---
# Added git AND libsodium-dev
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    git \
    libsodium-dev `# <--- Added for sodium package` \
 && apt-get clean && rm -rf /var/lib/apt/lists/*

# --- Install R Packages Directly ---
# *CRITICAL*: This step MUST succeed now. Check build logs carefully.
RUN echo "--- Installing REQUIRED CRAN dependencies (excluding baseballr) ---" && \
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
    # 'baseballr', # Skipped
    'fastRhockey', \
    'hoopR', \
    'itscalledsoccer', \
    'magrittr', \
    'nflreadr', \
    'wehoop', \
    'rlang', \
    'tibble', \
    'sodium', \
    'httpuv'  \
    ), repos='https://cloud.r-project.org/', Ncpus = 2)"

# --- Configure Application ---
WORKDIR /app
# Ensure the .Rprofile being copied no longer sources renv/activate.R
COPY . /app/

# --- Install Your GitHub Package (Skipping its Dependencies) ---
# This should now run without the Rprofile error
RUN echo "--- Installing GitHub package peakPerformR (dependencies=FALSE) ---" && \
    R -e "remotes::install_github('elivatsaas/peakPerformR', dependencies = FALSE)"

# --- Optional Post-Install Verification ---
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
