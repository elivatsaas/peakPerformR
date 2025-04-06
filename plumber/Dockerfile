FROM rocker/r-ver:4.2.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

# Install R packages (including your package)
RUN R -e "install.packages(c('plumber', 'jsonlite', 'dplyr', 'tidyr', 'readr', 'purrr', 'remotes'))"
# Install your package if needed
# RUN R -e "remotes::install_github('yourusername/your-package')"
RUN R -e "remotes::install_github('elivatsaas/peakPerformR')"

# Copy only the plumber directory contents
WORKDIR /app
COPY . /app

# Expose port
EXPOSE 8000

# Start the API
CMD R -e "library(plumber); pr <- plumb('plumber.R'); pr$run(host='0.0.0.0', port=as.numeric(Sys.getenv('PORT')))"