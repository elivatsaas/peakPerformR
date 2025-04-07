# Use a specific R version base image
FROM rocker/r-ver:4.2.0

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c('plumber', 'jsonlite', 'dplyr', 'tidyr', 'readr', 'purrr', 'remotes'), repos='https://cloud.r-project.org/')"

# Install your custom package
RUN R -e "remotes::install_github('elivatsaas/peakPerformR')"

# Set the working directory
WORKDIR /app

# Copy application files
COPY plumber.R /app/
COPY data/ /app/data/

# Create a simple startup script
RUN echo '#!/bin/bash\n\
echo "Starting API..."\n\
R -e "pr <- plumber::plumb(\"/app/plumber.R\"); pr\$run(host=\"0.0.0.0\", port=8080)"\n' > /app/start.sh && \
chmod +x /app/start.sh

# Expose the port
EXPOSE 8080

# Run the startup script
CMD ["/app/start.sh"]
