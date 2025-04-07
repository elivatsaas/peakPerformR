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

# Create directories for your API
RUN mkdir -p /app/data
RUN mkdir -p /app/plumber

# Set working directory
WORKDIR /app

# Copy your API files - first copy everything to make troubleshooting easier
# The context is the root of your GitHub repository
COPY . /app/

# Expose the port
EXPOSE 8080

# Add debug info to help troubleshoot
RUN echo "API files in /app:" && ls -la /app && echo "Files in subdirectories:" && find /app -type f -name "*.R" | sort

# Create a flexible startup script that tries different possible locations
RUN echo '#!/bin/bash\n\
echo "Starting Plumber API..."\n\
echo "Current directory: $(pwd)"\n\
echo "Looking for plumber.R file..."\n\
\n\
# Try different possible locations\n\
if [ -f "/app/plumber.R" ]; then\n\
  PLUMBER_PATH="/app/plumber.R"\n\
  echo "Found plumber.R at root level"\n\
elif [ -f "/app/plumber/plumber.R" ]; then\n\
  PLUMBER_PATH="/app/plumber/plumber.R"\n\
  echo "Found plumber.R in plumber directory"\n\
else\n\
  # Last resort - try to find it anywhere\n\
  PLUMBER_PATH=$(find /app -name "plumber.R" | head -n 1)\n\
  if [ -n "$PLUMBER_PATH" ]; then\n\
    echo "Found plumber.R at: $PLUMBER_PATH"\n\
  else\n\
    echo "ERROR: Could not find plumber.R file anywhere!"\n\
    exit 1\n\
  fi\n\
fi\n\
\n\
echo "Starting API with: $PLUMBER_PATH"\n\
R -e "options(warn=2); Sys.setenv(R_CONFIG_ACTIVE=\"production\"); message(\"Starting Plumber API...\"); pr <- plumber::plumb(\"$PLUMBER_PATH\"); pr\$run(host=\"0.0.0.0\", port=8080)"\n\
' > /app/start.sh

RUN chmod +x /app/start.sh

# Run the API via the startup script
CMD ["/app/start.sh"]
