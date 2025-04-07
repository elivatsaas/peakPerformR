FROM rocker/r-ver:latest

# Install plumber and required packages
RUN install2.r --error \
    plumber \
    jsonlite \
    dplyr \
    tidyr \
    readr \
    purrr \
    remotes \
    rlang

# Install additional debug/utility packages
RUN install2.r --error \
    logger \
    debugme

# Install your custom package
RUN R -e "remotes::install_github('elivatsaas/peakPerformR')"

# Create directory for your API
RUN mkdir -p /app/data

# Copy your API files - copying the entire repository to make troubleshooting easier
COPY . /app/

# Set working directory
WORKDIR /app

# CRITICAL: Use port 80 for Azure Container Apps
EXPOSE 80

# Add debug info to help troubleshoot
RUN echo "API files in /app:" && ls -la /app && echo "Files in subdirectories:" && find /app -type f -name "*.R" | sort

# Create a more robust startup script for Azure Container Apps
RUN echo '#!/bin/bash\n\
echo "Starting Plumber API on port 80..."\n\
echo "Current directory: $(pwd)"\n\
echo "Looking for plumber.R file..."\n\
\n\
# Search for plumber.R in common locations\n\
if [ -f "/app/plumber.R" ]; then\n\
  PLUMBER_PATH="/app/plumber.R"\n\
  echo "Found plumber.R at root level"\n\
elif [ -f "/app/plumber/plumber.R" ]; then\n\
  PLUMBER_PATH="/app/plumber/plumber.R"\n\
  echo "Found plumber.R in plumber directory"\n\
else\n\
  # Find it anywhere\n\
  PLUMBER_PATH=$(find /app -name "plumber.R" | head -n 1)\n\
  if [ -n "$PLUMBER_PATH" ]; then\n\
    echo "Found plumber.R at: $PLUMBER_PATH"\n\
  else\n\
    echo "ERROR: Could not find plumber.R file anywhere!"\n\
    exit 1\n\
  fi\n\
fi\n\
\n\
# Ensure data directory exists\n\
mkdir -p /app/data\n\
\n\
# Check for data files\n\
echo "Checking for data files:"\n\
find /app -name "*.rds" | sort\n\
\n\
# Create a very simple test endpoint file if needed\n\
if [ ! -f "$PLUMBER_PATH" ]; then\n\
  echo "Creating emergency test plumber.R file..."\n\
  echo "\n\
#* @get /\n\
function() {\n\
  list(status = \"API is running\", timestamp = Sys.time(), files = list.files(\"/app\", recursive = TRUE))\n\
}\n\
  " > /app/emergency-plumber.R\n\
  PLUMBER_PATH="/app/emergency-plumber.R"\n\
fi\n\
\n\
echo "Starting API with: $PLUMBER_PATH on port 80"\n\
exec R -e "options(warn=2); Sys.setenv(R_CONFIG_ACTIVE=\"production\"); message(\"Starting Plumber API...\"); pr <- plumber::plumb(\"$PLUMBER_PATH\"); pr\$run(host=\"0.0.0.0\", port=80)"\n\
' > /app/start.sh

RUN chmod +x /app/start.sh

# Run the API via the startup script
CMD ["/app/start.sh"]
