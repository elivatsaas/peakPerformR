# Use rocker/plumber which already has plumber installed
FROM rocker/plumber:4.2.0

# Set the working directory
WORKDIR /app

# Install additional R packages
RUN install2.r --error --deps TRUE jsonlite dplyr tidyr readr purrr remotes && \
    R -e "remotes::install_github('elivatsaas/peakPerformR')"

# Copy application files
# Since your Dockerfile is in the plumber directory, we use . to refer to current directory
COPY plumber.R /app/
COPY data/ /app/data/

# Expose the port
EXPOSE 8080

# Command to run the API
CMD ["/usr/local/bin/R", "-e", "pr <- plumber::plumb('/app/plumber.R'); pr$run(host='0.0.0.0', port=8080)"]
