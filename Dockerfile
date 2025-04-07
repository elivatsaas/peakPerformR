# Start with a base R version image
FROM rocker/r-ver:4.3.1

# Set DEBIAN_FRONTEND to noninteractive to avoid prompts during apt-get install
ENV DEBIAN_FRONTEND=noninteractive

# OPTIONAL: Update apt-get and install system dependencies if needed by your R packages
# Common examples: libssl-dev, libcurl4-openssl-dev, libxml2-dev, build-essential
# Uncomment and modify the following lines if required:
# RUN apt-get update && apt-get install -y --no-install-recommends \
#     # list-your-system-dependencies-here \
#  && apt-get clean && rm -rf /var/lib/apt/lists/*

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

# Install your custom package from GitHub
# Consider using a specific commit hash for reproducibility: 'elivatsaas/peakPerformR@commit_hash'
RUN R -e "remotes::install_github('elivatsaas/peakPerformR')"

# Set working directory
WORKDIR /app

# Copy all application files from the build context (your current directory) to the image's /app directory
# This includes your Plumber script (e.g., plumber.R or main.R), data files, etc.
# Ensure your main Plumber R script is in the root of your project or copied correctly.
COPY . .

# Ensure the data directory exists within the container's /app directory
# This is useful if your code expects it, even if it's initially empty or populated by COPY . .
RUN mkdir -p ./data

# CRITICAL: Expose port 80 for Azure Container Apps default behavior (or your target deployment)
EXPOSE 80

# Run the Plumber API directly using R when the container starts
# IMPORTANT: Replace 'plumber.R' with the actual filename of your main Plumber API script.
CMD ["R", "-e", "api <- plumber::plumb('plumber.R'); api$run(host = '0.0.0.0', port = 80)"]
