# Start with a specific R version image
FROM rocker/r-ver:4.3.1

# Set DEBIAN_FRONTEND to noninteractive to avoid prompts during apt-get install
ENV DEBIAN_FRONTEND=noninteractive

# --- Install System Dependencies ---
# Needed for compiling R packages from source (like those using curl, ssl, xml)
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
 # Add other -dev packages if specific errors point to them during install
 && apt-get clean && rm -rf /var/lib/apt/lists/*

# --- Install R Packages ---
# Install plumber and core dependencies first (like the working example)
# Use --error to stop if any installation fails. Let install2.r handle implicit dependencies.
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

# Install additional debug/utility packages separately
RUN install2.r --error \
    logger \
    debugme \
 && rm -rf /tmp/downloaded_packages/

# Install your custom package from GitHub
# Ensure this step runs *after* its dependencies (like remotes) are installed
RUN R -e "remotes::install_github('elivatsaas/peakPerformR')"

# --- Configure Application ---
# Set working directory
WORKDIR /app

# Copy all application files from the build context to /app
COPY . /app/

# Ensure the data directory exists within the container's /app directory
# Do this *after* COPY in case 'data' is part of the source repo
RUN mkdir -p ./data

# Optional: Add debug info to check file structure inside the image
RUN echo "--- Files in /app after COPY ---" && ls -la /app && echo "--- R files found ---" && find /app -type f -name "*.R" | sort

# --- Create Startup Script ---
# Generate the start.sh script directly in the image (adapted from your working example)
# This avoids needing a start.sh file in your source repository during build
RUN echo '#!/bin/bash\n\
echo "Starting Plumber API on port 80..."\n\
echo "Working Directory: $(pwd)"\n\
# Define the expected Plumber file name\n\
PLUMBER_FILE="plumber.R"\n\
# Try to find the plumber file in the standard location first\n\
if [ -f "/app/${PLUMBER_FILE}" ]; then\n\
  PLUMBER_PATH="/app/${PLUMBER_FILE}"\n\
  echo "Found ${PLUMBER_FILE} at standard location: ${PLUMBER_PATH}"\n\
else\n\
  # If not found, search within /app\n\
  echo "Searching for ${PLUMBER_FILE} in /app and subdirectories..."\n\
  FOUND_PATH=$(find /app -name "${PLUMBER_FILE}" -print -quit)\n\
  if [ -n "${FOUND_PATH}" ]; then\n\
      PLUMBER_PATH="${FOUND_PATH}"\n\
      echo "Found ${PLUMBER_FILE} at: ${PLUMBER_PATH}"\n\
  else\n\
      echo "ERROR: Cannot find ${PLUMBER_FILE} in /app or subdirectories!"\n\
      # Optional: Create a fallback file for basic testing if needed\n\
      # echo "#* @get /ping\n function(){ list(status=\"OK\") }" > /app/fallback_plumber.R\n\
      # PLUMBER_PATH="/app/fallback_plumber.R"\n\
      # echo "WARNING: Using fallback_plumber.R"\n\
      exit 1 # More robust to exit if the main file isn't found\n\
  fi\n\
fi\n\
# Ensure data directory exists at runtime (redundant but safe)\n\
mkdir -p /app/data\n\
echo "Starting API using R script: ${PLUMBER_PATH}"\n\
# Execute Plumber API, listening on all interfaces (0.0.0.0) on the exposed port\n\
exec R -e "options(warn=2); pr <- plumber::plumb(file='${PLUMBER_PATH}'); pr\$run(host='0.0.0.0', port=80)"\n\
' > /app/start.sh

# Make the generated script executable
RUN chmod +x /app/start.sh

# --- Expose Port and Run ---
# CRITICAL: Expose port 80 for Azure Container Apps (or other environments)
EXPOSE 80

# Run the API via the generated startup script
CMD ["/app/start.sh"]
