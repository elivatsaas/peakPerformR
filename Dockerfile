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
    pkg-config \
 && apt-get clean && rm -rf /var/lib/apt/lists/*
# --- Install ALL R Packages Directly (including ALL sports packages) ---
RUN echo "--- Installing ALL required CRAN dependencies ---" && \
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
    'baseballr', \
    'fastRhockey', \
    'hoopR', \
    'itscalledsoccer', \
    'magrittr', \
    'nflreadr', \
    'wehoop', \
    'rlang', \
    'tibble', \
    'sodium', \
    'httpuv', \
    'devtools' \
    ), repos='https://cloud.r-project.org/', Ncpus = 2)"
# --- Configure Application ---
WORKDIR /app
# Copy files
COPY . /app/
# Install GitHub package (simple version)
RUN echo "--- Installing GitHub package peakPerformR ---" && \
    R -e "remotes::install_github('elivatsaas/peakPerformR', dependencies = FALSE)"
# --- Verify and setup ---
RUN mkdir -p ./data && chmod +x /app/start.sh
# --- Expose Port and Run ---
EXPOSE 8080
CMD ["/app/start.sh"]
