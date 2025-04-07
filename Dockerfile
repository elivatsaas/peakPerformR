FROM rocker/plumber:latest

# Install required packages
RUN install2.r --error \
    jsonlite \
    dplyr \
    tidyr \
    readr \
    purrr \
    remotes

# Install your custom package
RUN R -e "remotes::install_github('elivatsaas/peakPerformR')"

# Create directory for your API
RUN mkdir -p /app/data

# Copy your API files
COPY plumber/plumber.R /app/plumber.R
COPY plumber/data/* /app/data/

# Set working directory
WORKDIR /app

# Expose the port
EXPOSE 8080

# Run the API
CMD ["R", "-e", "pr <- plumber::plumb('/app/plumber.R'); pr$run(host='0.0.0.0', port=8080)"]
