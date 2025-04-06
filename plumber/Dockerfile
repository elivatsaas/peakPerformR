# Use a specific R version base image
FROM rocker/r-ver:4.2.0

# Install system dependencies needed for R packages
# Combine steps and clean up apt cache to reduce image size
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages from CRAN
# Consider combining into one RUN layer if desired, though less critical
RUN R -e "install.packages(c('plumber', 'jsonlite', 'dplyr', 'tidyr', 'readr', 'purrr', 'remotes'), repos='https://cloud.r-project.org/')"

# Install your custom package from GitHub
RUN R -e "remotes::install_github('elivatsaas/peakPerformR')"

# Set the working directory in the container
WORKDIR /app

# Copy only necessary application files
# plumber.R contains the API logic
COPY plumber.R /app/
# Copy pre-computed data required by the API
COPY data/ /app/data/
# COPY app.R /app/ # Removed: Only copy if app.R is explicitly needed by your CMD or entrypoint

# Expose the port the Plumber API will run on (for documentation/runtime mapping)
EXPOSE 8080

# Define the command to run the Plumber API
# Use JSON format for CMD for better signal handling (addresses warning)
CMD ["R", "-e", "library(plumber); pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8080)"]