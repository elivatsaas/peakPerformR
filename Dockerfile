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
RUN echo "--- Installing GitHub package peakPerformR ---" && \
    R -e "message('R library paths:'); \
          print(.libPaths()); \
          message('Installing from GitHub...'); \
          remotes::install_github('elivatsaas/peakPerformR', dependencies = FALSE); \
          message('Installation completed'); \
          installed <- installed.packages(); \
          if('peakPerformR' %in% rownames(installed)) { \
            message('Package installed at: ', find.package('peakPerformR')); \
          } else { \
            message('Package NOT in installed.packages()!'); \
            all_pkgs <- list.files(.libPaths(), recursive = FALSE); \
            message('All packages in library: '); \
            print(all_pkgs); \
          }"
# --- Verify Plumber File ---
RUN echo "--- Checking plumber.R file ---" && \
    cat /app/plumber/plumber.R | head -10
# --- Setup Startup Script ---
RUN mkdir -p ./data && chmod +x /app/start.sh
# --- Expose Port and Run ---
EXPOSE 80
CMD ["/app/start.sh"]
