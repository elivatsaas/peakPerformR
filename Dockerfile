# Start with a specific R version image
FROM rocker/r-ver:4.3.1

# Set DEBIAN_FRONTEND to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

# --- Install System Dependencies ---
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
 && apt-get clean && rm -rf /var/lib/apt/lists/*

# --- Install R Packages ---
RUN install2.r --error \
    plumber \
    jsonlite \
    dplyr \
    tidyr \
    readr \
    purrr \
    remotes \
    rlang \
 && rm -rf /tmp/downloaded_packages/

RUN install2.r --error \
    logger \
    debugme \
 && rm -rf /tmp/downloaded_packages/

RUN R -e "remotes::install_github('elivatsaas/peakPerformR')"

# --- Configure Application ---
WORKDIR /app

# Copy all application files from the build context (your current directory)
# This MUST include the start.sh file you created, and your 'plumber' directory
COPY . /app/

# Verify files copied (Optional Debugging)
RUN echo "--- Files in /app after COPY ---" && ls -lha /app && echo "--- Files in /app/plumber ---" && ls -lha /app/plumber || echo "/app/plumber not found or empty"

# Ensure the data directory exists (safer to do after COPY)
RUN mkdir -p ./data

# --- Setup Startup Script ---
# Make the copied start.sh executable
RUN chmod +x /app/start.sh

# --- Expose Port and Run ---
EXPOSE 80
CMD ["/app/start.sh"]
