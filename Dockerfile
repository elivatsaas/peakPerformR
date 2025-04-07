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

# Copy your API files - adjust these paths to match your repository structure
# Assumes plumber.R is at the root of your repository and data is in a "data" folder
COPY plumber.R /app/plumber.R
COPY data/* /app/data/

# Set working directory
WORKDIR /app

# Expose the port
EXPOSE 8080

# Add debug info to help troubleshoot
RUN echo "API files in /app:" && ls -la /app && echo "Data files in /app/data:" && ls -la /app/data || echo "No data files found"

# Create a safer startup script
RUN echo '#!/bin/bash\n\
echo "Starting Plumber API..."\n\
echo "Current directory: $(pwd)"\n\
echo "Files in current directory: $(ls -la)"\n\
echo "Files in data directory: $(ls -la data)"\n\
R -e "options(warn=2); Sys.setenv(R_CONFIG_ACTIVE=\"production\"); message(\"Starting Plumber API...\"); pr <- plumber::plumb(\"/app/plumber.R\"); pr$run(host=\"0.0.0.0\", port=8080)"\n\
' > /app/start.sh

RUN chmod +x /app/start.sh

# Run the API via the startup script
CMD ["/app/start.sh"]
