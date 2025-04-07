FROM rocker/r-ver:4.2.0

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c('plumber', 'jsonlite', 'dplyr', 'tidyr', 'readr', 'purrr', 'rlang', 'remotes'), repos='https://cloud.r-project.org/')"

# Create app directory
WORKDIR /app

# Copy the package to the container
COPY . /tmp/peakPerformR/

# Install the package from the local source
RUN R -e "remotes::install_local('/tmp/peakPerformR', dependencies = TRUE, repos = 'https://cloud.r-project.org/')"

# Copy the plumber API and data files
COPY plumber/plumber.R /app/
COPY plumber/data/*.rds /app/data/

# Make sure the data directory exists
RUN mkdir -p /app/data

# Expose the port
EXPOSE 8000

# Set environment variable for the API
ENV R_CONFIG_ACTIVE=production

# Run the Plumber API
CMD ["R", "-e", "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8000)"]