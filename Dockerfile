# Dockerfile without renv - Fixing Rprofile issue and adding libsodium-dev
FROM rocker/r-ver:4.3.1
# Set DEBIAN_FRONTEND to noninteractive
ENV DEBIAN_FRONTEND=noninteractive
# --- Install System Dependencies ---
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    git \
    libsodium-dev \
    libz-dev \
 && apt-get clean && rm -rf /var/lib/apt/lists/*
# --- Install R Packages Directly ---
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
    'readr', \
    'magrittr', \
    'rlang', \
    'tibble', \
    'sodium', \
    'httpuv'  \
    ), repos='https://cloud.r-project.org/', Ncpus = 2)"
# --- Configure Application ---
WORKDIR /app
# Copy files
COPY . /app/
# --- Install peakPerformR with debugging ---
RUN echo "--- Installing GitHub package peakPerformR (forcing) ---" && \
    R -e "options(warn=1); \
          temp_dir <- tempdir(); \
          download_path <- file.path(temp_dir, 'peakPerformR.tar.gz'); \
          remotes::install_github('elivatsaas/peakPerformR', dependencies = FALSE, build = TRUE, build_opts = c('--no-build-vignettes'), upgrade = 'never'); \
          if(!requireNamespace('devtools', quietly = TRUE)) { \
            install.packages('devtools', repos='https://cloud.r-project.org/'); \
          } \
          devtools::install_github('elivatsaas/peakPerformR', dependencies = FALSE, upgrade = 'never', force = TRUE);"
# --- Verify Plumber File ---
RUN echo "--- Checking plumber.R file ---" && \
    cat /app/plumber/plumber.R | head -10
# --- Setup Startup Script ---
RUN mkdir -p ./data && chmod +x /app/start.sh
# --- Expose Port and Run ---
EXPOSE 80
CMD ["/app/start.sh"]
