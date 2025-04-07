# Start with a base R version image
FROM rocker/r-ver:4.3.1 # Or use latest: FROM rocker/r-ver:latest

# Set DEBIAN_FRONTEND to noninteractive to avoid prompts during apt-get install
ENV DEBIAN_FRONTEND=noninteractive

# Update apt-get and install system dependencies if needed (e.g., for packages with C code)
# RUN apt-get update && apt-get install -y --no-install-recommends \
#    some-system-dependency \
#    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install plumber and required packages using install2.r for better dependency handling
RUN install2.r --error --deps TRUE \
    plumber \
    jsonlite \
    dplyr \
    tidyr \
    readr \
    purrr \
    remotes \
    rlang \
    logger \
    debugme \
    && rm -rf /tmp/downloaded_packages/ # Clean up

# Install your custom package
# Consider using a specific commit hash for reproducibility: remotes::install_github('elivatsaas/peakPerformR@commit_hash')
RUN R -e "remotes::install_github('elivatsaas/peakPerformR')"

# Set working directory
WORKDIR /app

# Copy application files. Copying plumber directory specifically might be better for caching
# COPY plumber ./plumber
# COPY data ./data # If data is separate
# Copying everything can be okay for debugging but invalidates cache easily
COPY . .

# Ensure the data directory exists (redundant if copied, but safe)
RUN mkdir -p ./data

# Make the startup script executable
# First, ensure the script content is correct and saved as start.sh in your repo
COPY start.sh ./start.sh
RUN chmod +x ./start.sh

# CRITICAL: Expose port 80 for Azure Container Apps default behavior
EXPOSE 80

# Run the API via the startup script
CMD ["./start.sh"]
