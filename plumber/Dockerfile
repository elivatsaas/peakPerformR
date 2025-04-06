FROM rocker/r-ver:4.2.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

# Install R packages
RUN R -e "install.packages(c('plumber', 'jsonlite', 'dplyr', 'tidyr', 'readr', 'purrr', 'remotes'))"
RUN R -e "remotes::install_github('elivatsaas/peakPerformR')"

# Copy application files
WORKDIR /app
COPY plumber.R /app/
COPY app.R /app/
COPY data/ /app/data/

# Expose port
EXPOSE 8080

# Start the API
CMD R -e "library(plumber); pr <- plumb('plumber.R'); pr$run(host='0.0.0.0', port=8080)"